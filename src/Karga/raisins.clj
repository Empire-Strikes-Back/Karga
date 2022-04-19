(ns Karga.raisins
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
   
   [Karga.seed])
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
   (net.miginfocom.layout ConstraintParser LC UnitValue)))

(do (set! *warn-on-reflection* true) (set! *unchecked-math* true))

(defonce stateA (atom nil))
(defonce shapesA (atom nil))
(defonce draw| (chan (sliding-buffer 10)))

(defn draw-grid
  [{:keys [^Canvas canvas]}]
  (let [{:keys [grid-rows
                grid-cols]} @stateA
        ^Graphics2D graphics (.getGraphics canvas)
        row-height (/ (.getHeight canvas) grid-rows)
        col-width (/ (.getWidth canvas) grid-cols)]
    (doseq [row-i (range grid-rows)]
      (let [y (* row-i row-height)]
        (.drawLine graphics 0 y (.getWidth canvas) y)))
    (doseq [col-i (range grid-cols)]
      (let [x (* col-i col-width)]
        (.drawLine graphics x 0 x (.getHeight canvas))))))

(defn draw-question-challenges
  [{:keys [^Canvas canvas]
    :as opts}]
  (let [{:keys [grid-rows
                grid-cols
                question-challenges
                categories
                categories-name-to-category]} @stateA
        ^Graphics2D graphics (.getGraphics canvas)
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
  [{:keys [^Canvas canvas]
    :as opts}]
  (let [^Graphics2D graphics (.getGraphics canvas)]
    (.clearRect graphics 0 0 (.getWidth canvas)  (.getHeight canvas))
    (.setPaint graphics (Color. 255 255 255 255) #_(Color. 237 211 175 200))
    (.fillRect graphics 0 0 (.getWidth canvas) (.getHeight canvas))
    (.setPaint graphics  Color/BLACK)))

(defn create-ui
  [{:keys [resize|
           jframe]
    :as opts}]
  (let [jtab-panel (JPanel.)
        split-pane (JSplitPane.)
        jscore-label (JLabel.)
        canvas (Canvas.)
        jcanvas-panel (JPanel.)]

    (doto jtab-panel
      (.setLayout (MigLayout. "insets 0"
                              "[grow,shrink,fill]"
                              "[grow,shrink,fill]"))
      #_(.setLayout (BoxLayout. tab-panel BoxLayout/X_AXIS))
      #_(.add (doto split-pane
                (.setResizeWeight 0.5))))

    (.add jtab-panel jscore-label "dock north")

    (doto jcanvas-panel
      (.setLayout (MigLayout. "insets 0"
                              "[grow,shrink,fill]"
                              "[grow,shrink,fill]") #_(BoxLayout. jcanvas-panel BoxLayout/X_AXIS))
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
                                   (swap! stateA update :question-challenges
                                          (fn [coll] (keep (fn [question-challenge]
                                                             (if (= (:question (first question-challenge))
                                                                    (:question question))
                                                               nil
                                                               question-challenge)) coll)))))))
                           (mouseEntered [_ event])
                           (mouseExited [_ event])
                           (mousePressed [_ event])
                           (mouseReleased [_ event]))))


    #_(.setRightComponent split-pane canvas)

    (.add jcanvas-panel canvas "width 100%!,height 100%!")

    (.add jtab-panel jcanvas-panel "dock east,width 1:100%:, height 1:100%:")

    (go
      (loop []
        (when-let [value (<! draw|)]
          (Karga.seed/invoke-later-on-swing-edt
           (fn [_]
             (let []
               #_(println :canvas-draw)
               (clear-canvas {:canvas canvas})
               (draw-grid {:canvas canvas})
               (draw-question-challenges {:canvas canvas})
               (let [{:keys [correct
                             incorrect]} @stateA
                     total (+ correct incorrect)]
                 (.setText jscore-label (format "    score: %s/%s %s%%" correct total (int (* 100 (/ correct (if (zero? total) 1 total)))))))
               #_(draw-line)
               #_(draw-word))))
          (recur))))

    (go
      (loop [timeout| nil]
        (let [[value port] (alts! (concat [resize|] (when timeout| [timeout|])))]
          (condp = port

            resize|
            (let []
              (when value
                #_(println :resize)
                (recur (timeout 500))))

            timeout|
            (let []
              #_(println (.getWidth canvas) (.getHeight canvas))
              (>! draw| true)
              (recur nil))))))

    jtab-panel))

(defn process
  [{:keys [resize|
           stop|]
    :as opts}]

  (let [ ]

    (go
      (<! stop|)
      (close! draw|)
      (remove-watch stateA :watch-fn))

    (let []
      (reset! stateA {:correct 0
                      :incorrect 0
                      :grid-rows 16
                      :grid-cols 32}))


    (add-watch stateA :watch-fn
               (fn [ref wathc-key old-state new-state]

                 (when (not= old-state new-state)
                   (put! draw| true))))

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
                                                                          {:color (Karga.seed/color-for-word (:name category))}))) {} categories)
                     :question-challenges (mapv (fn [question position]
                                                  [question position])
                                                questions positions)})))
          ex|
          ([ex-message]
           (swap! stateA assoc :ex-message ex-message)))))))

