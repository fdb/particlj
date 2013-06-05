(ns particlj.core)

; TODO Use a Vector2D type for vector math: magnitude, normalize, etc.
; TODO Use a force vector in a particle that gets added to velocity.

;; Utility

(defn random [min max]
  "Generate a random number between min and max."
  (let [d (- max min)]
    (->> (Math/random)
         (* d)
         (+ min))))

;; State

(def current-id (atom 0))

(defn gen-id []
  "Generate a new, unique id."
  (swap! current-id inc))

(def the-system
  "This is a global containing the current particle system."
  (atom []))

;; GUI

(defn paint-particles [^java.awt.Graphics g]
  "Draw the particle system.
  This takes a graphics object and draws each individual
  particle as a dot with an id."
  (.translate g 300 300)
  (.setColor g java.awt.Color/BLUE)
  (.setFont g (java.awt.Font. java.awt.Font/MONOSPACED java.awt.Font/PLAIN 10))
  (doseq [p @the-system]
    (.fillRect g (p :x) (p :y) 2 2)
    (.drawString g (String/valueOf (p :id)) (+ (.intValue (p :x)) 2) (.intValue (p :y)))))

; Create a panel that will draw the particle system.
(defonce panel (doto (proxy [javax.swing.JPanel] []
                       (paintComponent [^java.awt.Graphics g]
                                       (paint-particles g)))
                 (.setPreferredSize (java.awt.Dimension. 600 600))))

; Create the window.
; We use defonce here so reloading everything doesn't create a new frame.
(defonce window (doto (javax.swing.JFrame.)
                  (.setContentPane panel)
                  .pack
                  (.setDefaultCloseOperation javax.swing.WindowConstants/EXIT_ON_CLOSE)
                  (.setVisible true)))

;; Vector math

(defn magnitude
  "Calculate the magnitude of the given vector."
  [{x :x y :y}]
  (Math/sqrt (+ (* x x) (* y y))))

(defn normalize
  "Divide each component of the vector by its magnitude."
  [v]
  (let [m (magnitude v)]
    (if (not= m 0.0)
      (assoc v
        :x (/ (v :x) m)
        :y (/ (v :y) m))
      v)))

(defn multiply-vector
  "Multiply each component of the vector with a constant."
  [v m]
  (assoc v
    :x (* (v :x) m)
    :y (* (v :y) m)))


;; Particle behaviours

(defn random-particle [& attrs]
  "Generate a random particle.
  You can optionally give a map of attributes that will be combined with the random particle."
  (merge
   {:id (gen-id)
    :x 0
    :y 0
    :vx (random -2 2)
    :vy (random -2 2)
    :age 0
    } (first attrs)))

(defn emit-from-point
  "Emit new particles from a certain point.
  Just like every particle node, it takes in a list of particles.
  The point should be encoded as {:x 0 :y 0}."
  [particles pos]
  (conj particles (random-particle pos)))

(defn kill-by-age
  "Remove particles that are older than max-age."
  [particles max-age]
  (filter #(<= (% :age) max-age) particles))

(defn wind
  "Add directional velocity."
  [particles vx vy]
  (map (fn [p]
         (let [nx (+ (p :x) vx)
               ny (+ (p :y) vy)]
           (assoc p :x nx :y ny)))
       particles))

; Drag doesn't work yet. It should calculate a force to apply,
; not change x/y directly.
(defn drag-point
  "Apply drag to a vector."
  [v coeff]
  (let [speed (magnitude v)
        drag-mag (* coeff speed speed)]
    (-> v
        (multiply-vector -1)
        normalize
        (multiply-vector drag-mag))))

(defn drag
  "Decrease the velocity."
  [particles coeff]
  (map #(drag-point % coeff) particles))

;; Feedback mechanism

(defn update-particle
  "Global update behaviour.
  This adds the velocity to the position and increases the age."
  [{:keys [x y vx vy age] :as p}]
  (assoc p
    :x (+ x vx)
    :y (+ y vy)
    :age (inc age)))

(defn default-behaviour [particles]
  (map update-particle particles))

(defn update-particles
  "The node 'network': the list of behaviours and their settings."
  [particles]
  (-> @the-system
      (emit-from-point {:x 100 :y -50})
      (wind -2 1)
      (kill-by-age 200)
      default-behaviour))

(defn update-system
  "Execute one time step.
  This is the feedback mechanism of the system."
  []
  (swap! the-system update-particles))

(defn do-frame
  "Execute the code for one frame. This updates the system and repaints."
  []
  (update-system)
  (.repaint window))

(defn do-reset
  "Reset the system to an empty state."
  []
  (reset! current-id 0)
  (reset! the-system [])
  (.repaint window))

;; Animation

(def animation-timer (atom nil))

(defn stop-animation
  []
  (if-not (nil? @animation-timer)
    (.stop @animation-timer)))

(defn start-animation
  []
  (stop-animation)
  (reset! animation-timer
          (javax.swing.Timer. 20
                              (proxy [java.awt.event.ActionListener] []
                                (actionPerformed [e] (do-frame)))))
  (.start @animation-timer))

;(start-animation)
;(stop-animation)
;(do-reset)
;(do-frame)

(defn -main []
  (start-animation))

