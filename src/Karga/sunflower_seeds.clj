(ns Karga.sunflower-seeds
  (:require
   [clojure.core.async :as Little-Rock
    :refer [chan put! take! close! offer! to-chan! timeout thread
            sliding-buffer dropping-buffer
            go >! <! alt! alts! do-alts
            mult tap untap pub sub unsub mix unmix admix
            pipe pipeline pipeline-async]]
   [clojure.java.io :as Wichita.java.io]
   [clojure.string :as Wichita.string])
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

(defn clear-canvas
  [{:keys [^Canvas canvas]
    :as opts}]
  (let [^Graphics2D graphics (.getGraphics canvas)]
    (.clearRect graphics 0 0 (.getWidth canvas)  (.getHeight canvas))
    (.setPaint graphics (Color. 255 255 255 255) #_(Color. 237 211 175 200))
    (.fillRect graphics 0 0 (.getWidth canvas) (.getHeight canvas))
    (.setPaint graphics  Color/BLACK)))

(defn create-ui
  [{:keys []
    :as opts}]
  
  
  )

(defn process
  [{:keys []
    :as opts}]
  (let []
    
    (let []
      (reset! stateA {:grid-rows 16
                      :grid-cols 32}))
    
    
    
    ))