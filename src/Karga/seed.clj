(ns Karga.seed
  (:require
   [clojure.core.async :as Little-Rock
    :refer [chan put! take! close! offer! to-chan! timeout thread
            sliding-buffer dropping-buffer
            go >! <! alt! alts! do-alts
            mult tap untap pub sub unsub mix unmix admix
            pipe pipeline pipeline-async]]
   [clojure.java.io :as Wichita.java.io]
   [clojure.string :as Wichita.string]
   [clojure.repl :as Wichita.repl])

  (:import
   (javax.swing JFrame WindowConstants ImageIcon JPanel JScrollPane JTextArea BoxLayout JEditorPane ScrollPaneConstants SwingUtilities JDialog)
   (javax.swing JMenu JMenuItem JMenuBar KeyStroke JOptionPane JToolBar JButton JToggleButton JSplitPane JLabel)
   (javax.swing.border EmptyBorder)
   (java.awt Canvas Graphics Graphics2D Shape Color Polygon Dimension BasicStroke Toolkit Insets BorderLayout)
   (java.awt.event KeyListener KeyEvent MouseListener MouseEvent ActionListener ActionEvent ComponentListener ComponentEvent)
   (java.awt.geom Ellipse2D Ellipse2D$Double Point2D$Double)
   (com.formdev.flatlaf FlatLaf FlatLightLaf)
   (com.formdev.flatlaf.extras FlatUIDefaultsInspector FlatDesktop FlatDesktop$QuitResponse FlatSVGIcon)
   (com.formdev.flatlaf.util SystemInfo UIScale)
   (java.util.function Consumer)
   (java.util ServiceLoader)
   (org.kordamp.ikonli Ikon)
   (org.kordamp.ikonli IkonProvider)
   (org.kordamp.ikonli.swing FontIcon)
   (org.kordamp.ikonli.codicons Codicons)
   (net.miginfocom.swing MigLayout)
   (net.miginfocom.layout ConstraintParser LC UnitValue)

   (java.awt.image BufferedImage)
   (java.awt Image Graphics2D Color)
   (javax.imageio ImageIO)
   (java.security MessageDigest)))

(defn invoke-later-on-swing-edt
  [f]
  (SwingUtilities/invokeLater
   (reify Runnable
     (run [_]
       (f _)))))

(defn color-for-word ^Color
  [^String word]
  (let [digest (->
                (doto (MessageDigest/getInstance "SHA-256")
                  (.update (.getBytes word)))
                (.digest))]
    (let [size (alength digest)
          step 3
          n (- size (mod size 3))
          positions (range 0 n step)
          positions-size (count positions)
          colors (->>
                  (reduce
                   (fn [result i]
                     (-> result
                         (update :red + (bit-and (aget digest i) 0xff))
                         (update :green + (bit-and (aget digest (+ i 1)) 0xff))
                         (update :blue + (bit-and (aget digest (+ i 2)) 0xff))))
                   {:red 0 :green 0 :blue 0}
                   positions)
                  (map (fn [[k value]]
                         [k (-> value (/ positions-size))]))
                  (into {}))]
      (Color. (int (:red colors)) (int (:green colors)) (int (:blue colors))))))

(defn eval-form
  ([form]
   (eval-form form {}))
  ([form
    {:keys [print-form?
            ns*]
     :or {print-form? true}
     :as opts}]
   (let [string-writer (java.io.StringWriter.)
         result (binding [*ns* ns*
                          *out* string-writer]
                  (eval form))]
     {:print-form? print-form?
      :result result
      :string-writer string-writer
      :form form})))

(defn print-ns-fns-docs
  [ns-sym]
  (go
    (let [fn-names (keys (ns-publics ns-sym))]
      (doseq [fn-name fn-names]
        (print (eval-form `(with-out-str (Wichita.repl/doc ~fn-name)) {:print-form? false}))))))

(defn draw-word
  "draw word"
  [^Canvas canvas]
  (.drawString ^Graphics2D (.getGraphics canvas) "word" (* 0.5 (.getWidth canvas)) (* 0.5 (.getHeight canvas))))

(defn draw-line
  "draw line"
  [^Canvas canvas]
  (.drawLine ^Graphics2D (.getGraphics canvas)  (* 0.3 (.getWidth canvas)) (* 0.3 (.getHeight canvas)) (* 0.7 (.getWidth canvas)) (* 0.7 (.getHeight canvas))))

(defn clear
  [{:keys [draw|]
    :as opts}]
  (put! draw| {:op :clear-output}))

(defn transmit
  "evaluate code in spe-editor-bike"
  [^JEditorPane jeditor]
  (-> (.getText jeditor) (clojure.string/trim) (clojure.string/trim-newline) (read-string) (eval-form)))

(defn create-editor
  [{:keys [draw|]
    :as opts}]
  (let [jcode-panel (JPanel.)
        jrepl (JTextArea. 1 80)
        joutput (JTextArea. 14 80)
        joutput-scroll (JScrollPane.)
        jeditor (JEditorPane.)
        jeditor-scroll (JScrollPane.)]

    (doto jeditor
      #_(.setBorder (EmptyBorder. #_top 0 #_left 0 #_bottom 0 #_right 0)))

    (doto jeditor-scroll
      (.setViewportView jeditor)
      (.setHorizontalScrollBarPolicy ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER)
      #_(.setPreferredSize (Dimension. 800 1300)))

    (doto joutput
      (.setEditable false))

    (doto joutput-scroll
      (.setViewportView joutput)
      (.setHorizontalScrollBarPolicy ScrollPaneConstants/HORIZONTAL_SCROLLBAR_NEVER))

    (doto jrepl
      (.addKeyListener (reify KeyListener
                         (keyPressed
                           [_ event]
                           (when (= (.getKeyCode ^KeyEvent event) KeyEvent/VK_ENTER)
                             (.consume ^KeyEvent event)))
                         (keyReleased
                           [_ event]
                           (when (= (.getKeyCode ^KeyEvent event) KeyEvent/VK_ENTER)
                             (-> (.getText jrepl) (clojure.string/trim) (clojure.string/trim-newline) (read-string) (eval-form) (->> (put! draw|)))
                             (.setText jrepl "")))
                         (keyTyped
                           [_ event]))))

    (doto jcode-panel
      (.setLayout (MigLayout. "insets 0"
                              "[grow,shrink,fill]"
                              "[grow,shrink,fill]"))
      (.add jeditor-scroll "wrap,height 70%")
      (.add joutput-scroll "wrap,height 30%")
      (.add jrepl "wrap"))

    #_(.add tab-panel code-panel "dock west")
    #_(.setLeftComponent split-pane code-panel)

    (go
      (loop []
        (when-let [value (<! draw|)]
          (condp = (:op value)
            :eval
            (let [{:keys [result
                          string-writer
                          print-form?
                          form]} value]
              (invoke-later-on-swing-edt
               (fn [_]
                 (doto joutput
                   (.append "=> "))
                 (when print-form?
                   (doto joutput
                     (.append (str form))
                     (.append "\n")))
                 (doto joutput
                   (.append (str string-writer))
                   (.append (if (string? result) result (pr-str result)))
                   (.append "\n"))))

              (go
                (<! (timeout 10))
                (invoke-later-on-swing-edt
                 (fn [_]
                   (let [scrollbar (.getVerticalScrollBar joutput-scroll)]
                     (.setValue scrollbar (.getMaximum scrollbar)))))))

            :clear-output
            (let []
              (invoke-later-on-swing-edt
               (fn [_]
                 (.setText joutput "")))))
          (recur))))

    {:jcode-panel jcode-panel
     :joutput-scroll joutput-scroll
     :jrepl jrepl
     :joutput joutput
     :jeditor jeditor}))


