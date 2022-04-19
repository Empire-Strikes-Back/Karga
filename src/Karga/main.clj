(ns Karga.main
  (:require
   [clojure.core.async :as Little-Rock
    :refer [chan put! take! close! offer! to-chan! timeout thread
            sliding-buffer dropping-buffer
            go >! <! alt! alts! do-alts
            mult tap untap pub sub unsub mix unmix admix
            pipe pipeline pipeline-async]]
   [clojure.java.io :as Wichita.java.io]
   [clojure.string :as Wichita.string]
   [clojure.repl :as Wichita.repl]
   [clojure.walk :as Wichita.walk]
   [clojure.test.check.generators :as Pawny.generators]
   [clj-http.client]
   [cheshire.core]

   [Karga.drawing]
   [Karga.seed]
   [Karga.raisins]
   [Karga.sunflower-seeds]
   [Karga.apples]
   [Karga.salt]
   [Karga.microwaved-potatoes]
   [Karga.corn]
   [Karga.beans])
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
   (java.security MessageDigest))
  (:gen-class))

#_(println (System/getProperty "clojure.core.async.pool-size"))
(do (set! *warn-on-reflection* true) (set! *unchecked-math* true))

(defonce stateA (atom nil))
(defonce shapesA (atom nil))
(defn jframe ^JFrame [] (:jframe @stateA))
(defn canvas ^Canvas [] (:canvas @stateA))
(defn repl ^JTextArea [] (:repl @stateA))
(defn output ^JTextArea [] (:output @stateA))
(defn editor ^JEditorPane [] (:editor @stateA))
(defn output-scroll ^JScrollPane [] (:output-scroll @stateA))
(defn graphics ^Graphics2D [] (:graphics @stateA))

(defn eval-form
  ([form]
   (eval-form form {}))
  ([form
    {:keys [print-form?]
     :or {print-form? true}
     :as opts}]
   (let [{:keys [^JTextArea output
                 ^JScrollPane output-scroll]
          ns* :main-ns} @stateA]
     (let [string-writer (java.io.StringWriter.)
           result (binding [*ns* ns*
                            *out* string-writer]
                    (eval form))]
       (doto output
         (.append "=> "))
       (when print-form?
         (doto output
           (.append (str form))
           (.append "\n")))
       (doto output
         (.append (str string-writer))
         (.append (if (string? result) result (pr-str result)))
         (.append "\n"))

       (go
         (<! (timeout 10))
         (let [scrollbar (.getVerticalScrollBar output-scroll)]
           (.setValue scrollbar (.getMaximum scrollbar))))))))

(defn color-for-word
  [word]
  (let [digest (->
                (doto (MessageDigest/getInstance "SHA-256")
                  (.update (.getBytes ^String word)))
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

(defn draw-word
  "draw word"
  []
  (let [{:keys [^Graphics2D graphics
                ^Canvas canvas]} @stateA]
    (.drawString graphics "word" (* 0.5 (.getWidth canvas)) (* 0.5 (.getHeight canvas)))))

(defn draw-line
  "draw line"
  []
  (let [{:keys [^Graphics2D graphics
                ^Canvas canvas]} @stateA]
    (.drawLine graphics  (* 0.3 (.getWidth canvas)) (* 0.3 (.getHeight canvas)) (* 0.7 (.getWidth canvas)) (* 0.7 (.getHeight canvas)))))

(defn draw-grid
  []
  (let [{:keys [^Graphics2D graphics
                ^Canvas canvas
                grid-rows
                grid-cols]} @stateA
        row-height (/ (.getHeight canvas) grid-rows)
        col-width (/ (.getWidth canvas) grid-cols)]
    (doseq [row-i (range grid-rows)]
      (let [y (* row-i row-height)]
        (.drawLine graphics 0 y (.getWidth canvas) y)))
    (doseq [col-i (range grid-cols)]
      (let [x (* col-i col-width)]
        (.drawLine graphics x 0 x (.getHeight canvas))))))

(defn draw-question-challenges
  []
  (let [{:keys [^Graphics2D graphics
                ^Canvas canvas
                grid-rows
                grid-cols
                question-challenges
                categories
                categories-name-to-category]} @stateA
        row-height (/ (.getHeight canvas) grid-rows)
        col-width (/ (.getWidth canvas) grid-cols)
        question-challenge-states (for [[question [row-i col-i]] question-challenges
                                        :let [x (* col-i col-width)
                                              y (* row-i row-height)
                                              category-id (:id (get categories-name-to-category (:category question)))
                                              category-color (:color (get categories-name-to-category (:category question)))
                                              category-shape (Ellipse2D$Double. x y (* 0.7 col-width) (* 0.7 row-height))]]
                                    {:question question
                                     :x x
                                     :y y
                                     :category-id category-id
                                     :category-color category-color
                                     :category-shape category-shape})]
    (swap! shapesA assoc :question-challenge-states question-challenge-states)
    (doseq [{:keys [question
                    category-id
                    category-shape
                    category-color
                    x
                    y]} question-challenge-states]
      #_(.drawString graphics (str category-id) (int x) (int y))
      (.setPaint graphics category-color)
      (.fill graphics category-shape)
      (.setPaint graphics Color/BLACK))))

(defn clear-canvas
  []
  (let [{:keys [^Graphics2D graphics
                ^Canvas canvas]} @stateA]
    (.clearRect graphics 0 0 (.getWidth canvas)  (.getHeight canvas))
    (.setPaint graphics (Color. 255 255 255 255) #_(Color. 237 211 175 200))
    (.fillRect graphics 0 0 (.getWidth canvas) (.getHeight canvas))
    (.setPaint graphics  Color/BLACK)))

(defn force-resize
  []
  (let [{:keys [^JFrame jframe]} @stateA]
    (let [width (.getWidth jframe)
          height (.getHeight jframe)]
      (.setSize jframe (Dimension. (+ 1 width) height))
      (.setSize jframe (Dimension. width height)))))

(defn clear
  []
  (let [{:keys [^JTextArea output]} @stateA]
    (.setText output "")))

(defn transmit
  "evaluate code in spe-editor-bike"
  []
  (let [{:keys [^JEditorPane editor]} @stateA]
    (-> (.getText editor) (clojure.string/trim) (clojure.string/trim-newline) (read-string) (eval-form))))

(defn print-fns
  []
  (go
    (let [fn-names (keys (ns-publics 'Karga.main))]
      (doseq [fn-name fn-names]
        (print (eval-form `(with-out-str (Wichita.repl/doc ~fn-name)) {:print-form? false}))))))

(defn create-jframe
  []
  (let [{:keys [^String jframe-title
                resize|]} @stateA]
    (let [jframe (JFrame. jframe-title)
          root-panel (JPanel.)
          screenshotsMode? (Boolean/parseBoolean (System/getProperty "flatlaf.demo.screenshotsMode"))

          on-menubar-item (fn [f]
                            (reify ActionListener
                              (actionPerformed [_ event]
                                (SwingUtilities/invokeLater
                                 (reify Runnable
                                   (run [_]
                                     (f _ event)))))))

          on-menu-item-show-dialog (on-menubar-item (fn [_ event] (JOptionPane/showMessageDialog jframe (.getActionCommand ^ActionEvent event) "menu bar item" JOptionPane/PLAIN_MESSAGE)))]

      (swap! stateA merge {:jframe jframe})

      (doto root-panel
        #_(.setLayout (BoxLayout. root-panel BoxLayout/Y_AXIS))
        (.setLayout (MigLayout. "wrap,insets 10"
                                "[grow,shrink,fill]"
                                "[grow,shrink,fill]")))

      (doto jframe
        (.add root-panel)
        (.addComponentListener (let []
                                 (reify ComponentListener
                                   (componentHidden [_ event])
                                   (componentMoved [_ event])
                                   (componentResized [_ event] (put! resize| (.getTime (java.util.Date.))))
                                   (componentShown [_ event])))))


      (let [jmenubar (JMenuBar.)]
        (doto jmenubar
          (.add (doto (JMenu.)
                  (.setText "file")
                  (.setMnemonic \F)
                  (.add (doto (JMenuItem.)
                          (.setText "new")
                          (.setAccelerator (KeyStroke/getKeyStroke KeyEvent/VK_N (-> (Toolkit/getDefaultToolkit) (.getMenuShortcutKeyMask))))
                          (.setMnemonic \U)
                          (.addActionListener on-menu-item-show-dialog)))
                  (.add (doto (JMenuItem.)
                          (.setText "exit")
                          (.setAccelerator (KeyStroke/getKeyStroke KeyEvent/VK_Q (-> (Toolkit/getDefaultToolkit) (.getMenuShortcutKeyMask))))
                          (.setMnemonic \X)
                          (.addActionListener (on-menubar-item (fn [_ event]
                                                                 (.dispose jframe)))))))))

        (.setJMenuBar jframe jmenubar))

      (FlatDesktop/setQuitHandler (reify Consumer
                                    (accept [_ response]
                                      (.performQuit ^FlatDesktop$QuitResponse response))
                                    (andThen [_ after] after)))

      (let [score-label (JLabel.)]
        (swap! stateA merge {:score-label score-label})
        (.add root-panel score-label "dock north"))

      (let [content-panel (JPanel.)
            split-pane (JSplitPane.)]
        (doto content-panel
          (.setLayout (BoxLayout. content-panel BoxLayout/X_AXIS))
          #_(.add (doto split-pane
                    (.setResizeWeight 0.5))))

        (let [canvas (Canvas.)
              canvas-panel (JPanel.)]

          (doto canvas-panel
            (.setLayout (MigLayout. "insets 0"
                                    "[grow,shrink,fill]"
                                    "[grow,shrink,fill]") #_(BoxLayout. canvas-panel BoxLayout/X_AXIS))
            #_(.setBorder (EmptyBorder. #_top 0 #_left 0 #_bottom 50 #_right 50)))

          (doto canvas
            #_(.setPreferredSize (Dimension. canvas-width canvas-height))
            (.addMouseListener (reify MouseListener
                                 (mouseClicked
                                   [_ event]
                                   (println :coordinate [(.getX ^MouseEvent event) (.getY ^MouseEvent event)])
                                   (let [point (.getPoint ^MouseEvent event)]
                                     (doseq [{:keys [^Shape category-shape
                                                     question]} (:question-challenge-states @shapesA)]
                                       (when (.contains category-shape point)
                                         (condp = (:type question)
                                           "boolean"
                                           (let [result (JOptionPane/showConfirmDialog jframe
                                                                                       (:question question)
                                                                                       (:category question)
                                                                                       JOptionPane/YES_NO_OPTION)
                                                 answer-string (if (= result 0) "True" "False")]
                                             (if (= answer-string (:correct_answer question))
                                               (swap! stateA update :correct inc)
                                               (swap! stateA update :incorrect inc)))
                                           "multiple"
                                           (let [answers (shuffle (concat [(:correct_answer question)] (:incorrect_answers question)))
                                                 result (JOptionPane/showInputDialog jframe
                                                                                     (:question question)
                                                                                     (:category question)
                                                                                     JOptionPane/PLAIN_MESSAGE
                                                                                     nil
                                                                                     (object-array answers)
                                                                                     (first answers))]
                                             (if (= result (:correct_answer question))
                                               (swap! stateA update :correct inc)
                                               (swap! stateA update :incorrect inc))))
                                         (swap! stateA update :question-challenges (fn [coll] (keep (fn [question-challenge]
                                                                                                      (if (= (:question (first question-challenge))
                                                                                                             (:question question))
                                                                                                        nil
                                                                                                        question-challenge)) coll)))))))
                                 (mouseEntered [_ event])
                                 (mouseExited [_ event])
                                 (mousePressed [_ event])
                                 (mouseReleased [_ event]))))

          #_(.setRightComponent split-pane canvas)

          (.add canvas-panel canvas "width 100%!,height 100%!")

          (.add root-panel canvas-panel "dock east,width 50%!, height 1:100%:")
          (go
            (<! (timeout 50))
            (swap! stateA merge {:canvas canvas
                                 :graphics (.getGraphics canvas)})))

        (.add root-panel content-panel))

      (when-let [url (Wichita.java.io/resource "icon.png")]
        (.setIconImage jframe (.getImage (ImageIcon. url))))

      nil)))

(defn window
  []
  (let [{:keys [jframe-title
                exit||
                resize|
                canvas-draw|]} @stateA]

    (when SystemInfo/isMacOS
      (System/setProperty "apple.laf.useScreenMenuBar" "true")
      (System/setProperty "apple.awt.application.name" jframe-title)
      (System/setProperty "apple.awt.application.appearance" "system"))

    (when SystemInfo/isLinux
      (JFrame/setDefaultLookAndFeelDecorated true)
      (JDialog/setDefaultLookAndFeelDecorated true))

    (when (and
           (not SystemInfo/isJava_9_orLater)
           (= (System/getProperty "flatlaf.uiScale") nil))
      (System/setProperty "flatlaf.uiScale" "2x"))

    (doseq [exit| exit||]
      (close! exit|))

    (let [exit| (chan 1)]
      (swap! stateA update :exit|| conj exit|)
      (go
        (loop [timeout| nil]
          (let [[value port] (alts! (concat [resize| exit|] (when timeout| [timeout|])))]
            (condp = port

              resize|
              (let []
                #_(println :resize)
                (recur (timeout 500)))

              timeout|
              (let []
                (>! canvas-draw| true)
                (recur nil))

              exit|
              (do
                (swap! stateA update :exit|| disj exit|)
                nil))))))

    (let [exit| (chan 1)]
      (go
        (<! (timeout 1000))
        (swap! stateA merge {:graphics (.getGraphics ^Canvas (:canvas @stateA))})
        (loop []
          (let [[value port] (alts! [canvas-draw| exit|])]
            (condp = port
              canvas-draw|
              (let []
                #_(println :canvas-draw)
                (clear-canvas)
                (draw-grid)
                (draw-question-challenges)
                (let [{:keys [^JLabel score-label
                              correct
                              incorrect]} @stateA
                      total (+ correct incorrect)]
                  (.setText score-label (format "    score: %s/%s %s%%" correct total (int (* 100 (/ correct (if (zero? total) 1 total)))))))
                #_(draw-line)
                #_(draw-word)
                (recur))

              exit|
              (do
                (swap! stateA update :exit|| disj exit|)
                nil))))))

    (SwingUtilities/invokeLater
     (reify Runnable
       (run [_]

         (FlatLightLaf/setup)

         (create-jframe)

         (let [{:keys [^JFrame jframe]} @stateA]
           (.setPreferredSize jframe
                              (let [size (-> (Toolkit/getDefaultToolkit) (.getScreenSize))]
                                (Dimension. (UIScale/scale 1024) (UIScale/scale 576)))
                              #_(if SystemInfo/isJava_9_orLater
                                  (Dimension. 830 440)
                                  (Dimension. 1660 880)))

           #_(doto jframe
               (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE #_WindowConstants/EXIT_ON_CLOSE)
               (.setSize 2400 1600)
               (.setLocation 1300 200)
               #_(.add panel)
               (.setVisible true))
           (doto jframe
             (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE #_WindowConstants/EXIT_ON_CLOSE)
             (.pack)
             (.setLocationRelativeTo nil)
             (.setVisible true))


           (remove-watch stateA :watch-fn)
           (add-watch stateA :watch-fn
                      (fn [ref wathc-key old-state new-state]

                        (when (not= old-state new-state)
                          (put! canvas-draw| true))))

           (do
             #_(eval-form `(print-fns))
             (force-resize)

             (go
               (let [questions| (chan 1)
                     categories| (chan 1)
                     ex| (chan 1)
                     _ (clj-http.client/get
                        "https://opentdb.com/api.php?amount=50"
                        {:async? true}
                        (fn [response] (put! questions| (-> response :body (cheshire.core/parse-string true) :results)))
                        (fn [ex] (println (ex-message ex)) (close! ex|)))
                     _ (clj-http.client/get
                        "https://opentdb.com/api_category.php"
                        {:async? true}
                        (fn [response] (put! categories| (-> response :body (cheshire.core/parse-string true) :trivia_categories)))
                        (fn [ex] (println (ex-message ex)) (close! ex|)))
                     response| (Little-Rock/map vector [questions| categories|])]
                 (alt!
                   response|
                   ([[questions categories]]
                    (let [{:keys [grid-rows
                                  grid-cols]} @stateA
                          cells (for [col-i (range grid-cols)
                                      row-i (range grid-rows)]
                                  [row-i col-i])
                          positions (take 50 (shuffle cells))]
                      (swap! stateA merge
                             {:questions questions
                              :categories categories
                              :categories-name-to-category (reduce (fn [result category]
                                                                     (assoc result (:name category)
                                                                            (merge category
                                                                                   {:color (color-for-word (:name category))}))) {} categories)
                              :question-challenges (mapv (fn [question position]
                                                           [question position])
                                                         questions positions)})))
                   ex|
                   ([ex-message]
                    (swap! stateA assoc :ex-message ex-message))))

               ))))))))

(defn reload
  []
  (require '[Karga.main] :reload))

(defn -main
  [& args]
  (let []
    (println "i dont want my next job")
    (reset! stateA {:resize| (chan (sliding-buffer 1))
                    :canvas-draw| (chan (sliding-buffer 1))
                    :exit|| #{}
                    :jframe nil
                    :jframe-title "no, i for one, i celebrate Mando's success - because it is my success as well - hell, even i am rich"
                    :canvas nil
                    :repl nil
                    :output nil
                    :editor nil
                    :output-scroll nil
                    :graphics nil
                    :main-ns (find-ns 'Karga.main)
                    :grid-rows 16
                    :grid-cols 32
                    :correct 0
                    :incorrect 0})
    (window)
    (println "Kuiil has spoken")))