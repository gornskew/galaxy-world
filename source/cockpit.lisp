;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

;; The cockpit: the room the player plays from, holding the HELM.
;; The idiom is a concours-kept classic pickup cab -- stitched bench,
;; painted metal and chrome instrument panel, thin-rim wheel, three
;; on the tree.  How the hull achieves steering and propulsion stays
;; unsaid; the truck needs no explanation.
;;
;; Frame: meters, origin at the driver's hip point.  x runs forward
;; toward the windshield, y to port (the driver's left), z up.
;; Everything here is modeled from first principles with the stock
;; primitives -- no imported meshes.

;; The cab's paint and brightwork.
(defparameter +paint+ "#4c8c8c")        ; painted metal, sea-green
(defparameter +chrome+ "#d9dde1")
(defparameter +leather+ "#8a5a33")      ; saddle bench
(defparameter +rubber+ "#202020")
(defparameter +gauge-face+ "#101418")
(defparameter +needle+ "#d84b2a")
(defparameter +glass+ "#aac4d0")        ; barely-there, the stars do the rest

(defparameter +wheel-rake+ (deg->rad 38))

(define-object cockpit (base-object)

  :computed-slots
  (;; the column's axis, pointing up and back toward the driver
   (column-axis (let ((theta +wheel-rake+))
                  (make-vector (- (cos theta)) 0 (sin theta))))
   (wheel-center (make-point 0.42 0 0.44))
   ;; in-plane frame of the wheel: across, and up-forward
   (wheel-across (make-vector 0 1 0))
   (wheel-up (cross-vectors (the wheel-across) (the column-axis)))
   (wheel-radius 0.20)
   (shifter-pivot (add-vectors (the wheel-center)
                               (scalar*vector -0.14 (the column-axis))))

   ;; The greenhouse: the wraparound panoramic windshield sweeps
   ;; between two arcs -- the cowl line and the header, the header
   ;; pulled back and in so the glass rakes.  Arc azimuth runs from
   ;; dead ahead, positive to port.  The glass is DIRECT VIEW out of
   ;; the hull; the basilisk's own two reptile eyes feed the
   ;; flatscreens in the instrument panel instead.
   (cowl-center (make-point -1.067 -0.36 0.55))
   (cowl-radius 2.097)
   (header-center (make-point -1.11 -0.36 1.05))
   (header-radius 2.0)
   (greenhouse-span (deg->rad 48))
   ;; the cab's inner walls, where the glass sweep lands -- a roomy
   ;; crew cab, bench behind bench
   (port-wall 0.49)
   (starboard-wall -1.21))

  :functions
  ((rim-point
    (alpha)  ; angle from wheel-up, radians
    (add-vectors (the wheel-center)
                 (add-vectors
                  (scalar*vector (* (the wheel-radius) (cos alpha)) (the wheel-up))
                  (scalar*vector (* (the wheel-radius) (sin alpha)) (the wheel-across)))))

   (arc-point
    (center radius theta)  ; azimuth from +x, in center's z plane
    (make-point (+ (get-x center) (* radius (cos theta)))
                (+ (get-y center) (* radius (sin theta)))
                (get-z center)))

   (cowl-point
    (theta)
    (the (arc-point (the cowl-center) (the cowl-radius) theta)))

   (header-point
    (theta)
    (the (arc-point (the header-center) (the header-radius) theta)))

   (span-theta
    (i n)  ; the i-th of n stations across the sweep, port to starboard
    (- (* 0.5 (the greenhouse-span))
       (* (/ i n) (the greenhouse-span))))

   (span-tangent
    (theta)  ; horizontal tangent along the sweep
    (make-vector (- (sin theta)) (cos theta) 0))

   ;; the leather-wrapped rim: cut as a fine smooth-shaded mesh
   ;; rather than the stock faceted torus, so the wrap can carry a
   ;; grain and the silhouette stays round
   (wheel-rim-x3d
    ()
    (torus-x3d :center (the wheel-center)
               :axis (the column-axis)
               :across (the wheel-across)
               :major-radius (the wheel-radius)
               :minor-radius 0.013
               :texture-id "leather-tex"
               :fallback-color "0.42 0.27 0.14")))

  :objects
  (;; the shell of the room: floor, bench, instrument panel
   (floor-pan :type 'box
              :center (make-point -0.20 -0.36 -0.37)
              :width 2.6 :length 1.80 :height 0.04
              :display-controls (list :color +rubber+))

   (bench-cushion :type 'box
                  :center (make-point -0.03 -0.36 -0.075)
                  :width 0.55 :length 1.66 :height 0.15
                  :display-controls (list :color +leather+))

   (bench-back :type 'box
               :center (make-point -0.42 -0.36 0.22)
               :width 0.12 :length 1.66 :height 0.60
               :display-controls (list :color +leather+))

   ;; the back seat: same stitched bench, a footwell behind the front
   (rear-bench-cushion :type 'box
                       :center (make-point -1.025 -0.36 -0.075)
                       :width 0.55 :length 1.66 :height 0.15
                       :display-controls (list :color +leather+))

   (rear-bench-back :type 'box
                    :center (make-point -1.42 -0.36 0.22)
                    :width 0.12 :length 1.66 :height 0.60
                    :display-controls (list :color +leather+))

   ;; the instrument panel itself is NOT here: it wears wood veneer,
   ;; so it renders as a textured shape alongside the cab -- see
   ;; wood-panel-x3d

   ;; the gauge cluster, chrome bezels proud of the panel face
   (speedo-bezel :type 'c-cylinder
                 :start (make-point 0.722 0 0.46)
                 :end (make-point 0.732 0 0.46)
                 :radius 0.085
                 :number-of-sections 24
                 :display-controls (list :color +chrome+))
   (speedo-face :type 'c-cylinder
                :start (make-point 0.716 0 0.46)
                :end (make-point 0.723 0 0.46)
                :radius 0.075
                :number-of-sections 24
                :display-controls (list :color +gauge-face+))
   ;; the speedo and compass needles are NOT here: they read the
   ;; session's own state, so they render per cockpit, not in the
   ;; shared cab

   (port-gauge-bezel :type 'c-cylinder
                     :start (make-point 0.722 0.19 0.45)
                     :end (make-point 0.732 0.19 0.45)
                     :radius 0.055
                     :number-of-sections 24
                     :display-controls (list :color +chrome+))
   (port-gauge-face :type 'c-cylinder
                    :start (make-point 0.717 0.19 0.45)
                    :end (make-point 0.723 0.19 0.45)
                    :radius 0.047
                    :number-of-sections 24
                    :display-controls (list :color +gauge-face+))

   (starboard-gauge-bezel :type 'c-cylinder
                          :start (make-point 0.722 -0.19 0.45)
                          :end (make-point 0.732 -0.19 0.45)
                          :radius 0.055
                     :number-of-sections 24
                          :display-controls (list :color +chrome+))
   (starboard-gauge-face :type 'c-cylinder
                         :start (make-point 0.717 -0.19 0.45)
                         :end (make-point 0.723 -0.19 0.45)
                         :radius 0.047
                    :number-of-sections 24
                         :display-controls (list :color +gauge-face+))
   (starboard-gauge-needle :type 'c-cylinder
                           :start (make-point 0.715 -0.19 0.452)
                           :end (make-point 0.715 -0.218 0.486)
                           :radius 0.0025
                           :display-controls (list :color +needle+))

   ;; the eye displays: two flatscreens in the instrument panel,
   ;; fed by the basilisk's own port and starboard reptile eyes.
   ;; Dark glass until the feeds are wired.
   (eye-screen-bezels :type 'box
                      :sequence (:size 2)
                      :center (make-point 0.728
                                          (ecase (the-child index)
                                            (0 0.34) (1 -0.75))
                                          0.42)
                      :width 0.015
                      :length 0.27
                      :height 0.18
                      :display-controls (list :color +chrome+))

   (eye-screens :type 'box
                :sequence (:size 2)
                :center (make-point 0.719
                                    (ecase (the-child index)
                                      (0 0.34) (1 -0.75))
                                    0.42)
                :width 0.01
                :length 0.24
                :height 0.15
                :display-controls (list :color +gauge-face+))

   ;; the rear-view mirror, hung from the header at the cab's
   ;; centerline
   (mirror-stem :type 'c-cylinder
                :start (make-point 0.86 -0.36 1.03)
                :end (make-point 0.78 -0.36 0.98)
                :radius 0.008
                :display-controls (list :color +chrome+))

   (mirror-back :type 'box
                :center (make-point 0.775 -0.36 0.955)
                :width 0.02
                :length 0.28
                :height 0.09
                :display-controls (list :color +chrome+))

   (mirror-glass :type 'box
                 :center (make-point 0.763 -0.36 0.955)
                 :width 0.006
                 :length 0.26
                 :height 0.08
                 :display-controls (list :color +gauge-face+))

   ;; the cosmic dice are NOT here: they answer the last burn, so
   ;; like the needles they render per cockpit, not in the shared cab

   ;; the HELM's rim is NOT here either: it is wrapped in leather,
   ;; cut as a smooth-shaded mesh by (the wheel-rim-x3d) -- see
   ;; cockpit-x3d, which appends it to the cab

   ;; the wheel is dished: the hub sits recessed down the column and
   ;; the spokes climb out to the rim
   (wheel-hub :type 'c-cylinder
              :start (add-vectors (the wheel-center)
                                  (scalar*vector -0.075 (the column-axis)))
              :end (add-vectors (the wheel-center)
                               (scalar*vector -0.015 (the column-axis)))
              :radius 0.04
              :number-of-sections 24
              :display-controls (list :color +chrome+))

   (horn-button :type 'sphere
                :center (add-vectors (the wheel-center)
                                     (scalar*vector -0.008 (the column-axis)))
                :radius 0.025
                :display-controls (list :color +chrome+))

   ;; classic three-spoke: two high, one straight down
   (spokes :type 'c-cylinder
           :sequence (:size 3)
           :start (add-vectors (the wheel-center)
                               (scalar*vector -0.055 (the column-axis)))
           :end (the (rim-point (ecase (the-child index)
                                  (0 (deg->rad 60))
                                  (1 (deg->rad -60))
                                  (2 pi))))
           :radius 0.008
           :display-controls (list :color +chrome+))

   (column :type 'c-cylinder
           :start (add-vectors (the wheel-center)
                               (scalar*vector -0.07 (the column-axis)))
           :end (add-vectors (the wheel-center)
                             (scalar*vector -0.60 (the column-axis)))
           :radius 0.024
           :display-controls (list :color +paint+))

   ;; three on the tree: the column shifter is the display-scale lever
   (shifter-lever :type 'c-cylinder
                  :start (the shifter-pivot)
                  :end (add-vectors (the shifter-pivot)
                                    (make-vector -0.06 -0.34 0.10))
                  :radius 0.007
                  :display-controls (list :color +chrome+))
   (shifter-knob :type 'sphere
                 :center (add-vectors (the shifter-pivot)
                                      (make-vector -0.06 -0.34 0.10))
                 :radius 0.018
                 :display-controls (list :color +gauge-face+))

   ;; the pedals: clutch, brake, gas.  The brake is fully present
   ;; and does nothing whatsoever -- space doesn't brake, and the
   ;; pedal is how the cockpit says so.
   (pedal-plates :type 'box
                 :sequence (:size 3)
                 :center (make-point 0.72
                                     (ecase (the-child index)
                                       (0 0.22) (1 0.05) (2 -0.14))
                                     (ecase (the-child index)
                                       (0 -0.18) (1 -0.18) (2 -0.20)))
                 :width 0.02
                 :length (ecase (the-child index) (0 0.09) (1 0.09) (2 0.06))
                 :height (ecase (the-child index) (0 0.08) (1 0.08) (2 0.15))
                 :display-controls (list :color +rubber+))
   (pedal-stalks :type 'c-cylinder
                 :sequence (:size 3)
                 :start (make-point 0.73
                                    (ecase (the-child index)
                                      (0 0.22) (1 0.05) (2 -0.14))
                                    -0.22)
                 :end (make-point 0.78
                                  (ecase (the-child index)
                                    (0 0.22) (1 0.05) (2 -0.14))
                                  -0.35)
                 :radius 0.008
                 :display-controls (list :color +chrome+))

   ;; the greenhouse: cowl and header arcs as segmented rails
   (cowl-rail :type 'c-cylinder
              :sequence (:size 10)
              :start (the (cowl-point (the (span-theta (the-child index) 10))))
              :end (the (cowl-point (the (span-theta (1+ (the-child index)) 10))))
              :radius 0.02
              :display-controls (list :color +paint+))

   (header-rail :type 'c-cylinder
                :sequence (:size 10)
                :start (the (header-point (the (span-theta (the-child index) 10))))
                :end (the (header-point (the (span-theta (1+ (the-child index)) 10))))
                :radius 0.02
                :display-controls (list :color +paint+))

   (a-pillars :type 'c-cylinder
              :sequence (:size 2)
              :start (the (cowl-point (* (ecase (the-child index) (0 1) (1 -1))
                                         0.5 (the greenhouse-span))))
              :end (the (header-point (* (ecase (the-child index) (0 1) (1 -1))
                                         0.5 (the greenhouse-span))))
              :radius 0.028
              :display-controls (list :color +paint+))

   ;; the glass itself, five raked chords along the sweep
   (windshield-glass :type 'box
                     :sequence (:size 5)
                     :theta (the (span-theta (+ (the-child index) 0.5) 5))
                     :sill (the (cowl-point (the-child theta)))
                     :head (the (header-point (the-child theta)))
                     :center (midpoint (the-child sill) (the-child head))
                     :orientation (alignment
                                   :top (unitize-vector
                                         (subtract-vectors (the-child head)
                                                           (the-child sill)))
                                   :rear (the (span-tangent (the-child theta))))
                     :height (3d-distance (the-child sill) (the-child head))
                     :length 0.34
                     :width 0.012
                     :display-controls (list :color +glass+ :transparency 0.85))

   ;; the cowl deck closes the gap between panel top and glass base
   (cowl-deck :type 'box
              :sequence (:size 5)
              :theta (the (span-theta (+ (the-child index) 0.5) 5))
              :center (let ((sill (the (cowl-point (the-child theta)))))
                        (make-point (- (get-x sill) (* 0.085 (cos (the-child theta))))
                                    (- (get-y sill) (* 0.085 (sin (the-child theta))))
                                    0.545))
              :orientation (alignment :rear (the (span-tangent (the-child theta))))
              :width 0.17
              :length 0.36
              :height 0.025
              :display-controls (list :color +paint+))

   ;; the cab sides: beltline, side headers, rear posts, side glass
   (beltline-rails :type 'c-cylinder
                   :sequence (:size 2)
                   :start (make-point 0.85
                                      (ecase (the-child index)
                                        (0 (the port-wall)) (1 (the starboard-wall)))
                                      0.55)
                   :end (make-point -1.65
                                    (ecase (the-child index)
                                      (0 (the port-wall)) (1 (the starboard-wall)))
                                    0.55)
                   :radius 0.02
                   :display-controls (list :color +paint+))

   (side-headers :type 'c-cylinder
                 :sequence (:size 2)
                 :start (the (header-point (* (ecase (the-child index) (0 1) (1 -1))
                                              0.5 (the greenhouse-span))))
                 :end (make-point -1.65
                                  (ecase (the-child index)
                                    (0 (the port-wall)) (1 (the starboard-wall)))
                                  1.03)
                 :radius 0.02
                 :display-controls (list :color +paint+))

   (rear-posts :type 'c-cylinder
               :sequence (:size 2)
               :start (make-point -1.65
                                  (ecase (the-child index)
                                    (0 (the port-wall)) (1 (the starboard-wall)))
                                  0.55)
               :end (make-point -1.65
                                (ecase (the-child index)
                                  (0 (the port-wall)) (1 (the starboard-wall)))
                                1.03)
               :radius 0.028
               :display-controls (list :color +paint+))

   (side-glass :type 'box
               :sequence (:size 2)
               :center (make-point -0.415
                                   (ecase (the-child index) (0 0.488) (1 -1.208))
                                   0.795)
               :width 2.48
               :length 0.012
               :height 0.45
               :display-controls (list :color +glass+ :transparency 0.85))

   ;; the body below the beltline, so the glasshouse stands on
   ;; something: door sides, the panel below the rear window, and
   ;; the firewall the pedals hang before
   (body-sides :type 'box
               :sequence (:size 2)
               :center (make-point -0.40
                                   (ecase (the-child index) (0 0.5025) (1 -1.2225))
                                   0.10)
               :width 2.50
               :length 0.025
               :height 0.90
               :display-controls (list :color +paint+))

   (rear-body-panel :type 'box
                    :center (make-point -1.6625 -0.36 0.10)
                    :width 0.025
                    :length 1.72
                    :height 0.90
                    :display-controls (list :color +paint+))

   (firewall :type 'box
             :center (make-point 0.95 -0.36 -0.035)
             :width 0.025
             :length 1.70
             :height 0.63
             :display-controls (list :color +paint+))

   ;; the rear window: no truck bed back there -- the rest of him,
   ;; one day, looming
   (rear-sill :type 'c-cylinder
              :start (make-point -1.65 (the starboard-wall) 0.55)
              :end (make-point -1.65 (the port-wall) 0.55)
              :radius 0.02
              :display-controls (list :color +paint+))

   (rear-header :type 'c-cylinder
                :start (make-point -1.65 (the starboard-wall) 1.03)
                :end (make-point -1.65 (the port-wall) 1.03)
                :radius 0.02
                :display-controls (list :color +paint+))

   (rear-glass :type 'box
               :center (make-point -1.645 -0.36 0.795)
               :width 0.012
               :length 1.58
               :height 0.45
               :display-controls (list :color +glass+ :transparency 0.85))

   ;; the roof is OPEN: a painted rim around the edge, and over the
   ;; opening the glass bubble star-roof (see star-dome-x3d).  Strips
   ;; run front, rear, port, starboard, leaving the center to the sky.
   (roof-rims :type 'box
              :sequence (:size 4)
              :center (ecase (the-child index)
                        (0 (make-point 0.73 -0.36 1.055))    ; front
                        (1 (make-point -1.62 -0.36 1.055))   ; rear
                        (2 (make-point -0.45 0.435 1.055))   ; port
                        (3 (make-point -0.45 -1.155 1.055))) ; starboard
              :width (ecase (the-child index)
                       (0 0.26) (1 0.24) (2 2.10) (3 2.10))
              :length (ecase (the-child index)
                        (0 1.80) (1 1.80) (2 0.21) (3 0.21))
              :height 0.03
              :display-controls (list :color +paint+))))

;; The world the cockpit falls around: the home planet, at the
;; origin of the plane.  Real figures -- km, km/s, km^3/s^2.
(defparameter +mu+ 398600.0d0)
(defparameter +planet-radius+ 6371.0)
(defparameter +sky-radius+ 6471.0)      ; meet this and the flight is over
(defparameter +ring-radius+ 20742.0)    ; the ship's ring, same as the chart's
(defparameter +ring-speed+ (sqrt (/ +mu+ +ring-radius+)))

;; The berth keeps a tally of what the helms aboard actually do --
;; the seed of the play-feeds-the-buildout channel.  One image, one
;; table, every cockpit.
(defvar *helm-tallies* (make-hash-table :test 'eq))

(defun tally! (key)
  (incf (gethash key *helm-tallies* 0)))

;; The planet as seen from the ship: drawn at a fixed scene distance
;; with the radius that subtends the true angle, so he grows as you
;; fall toward him and shrinks as you climb away.  He wears his face
;; now -- an ImageTexture the page paints onto a canvas client-side
;; (see the texture script in cockpit-view) -- and he turns: a
;; TimeSensor spins him about his pole, so the continents file past
;; the glass and the orbit FEELS like an orbit.  The sphere's poles
;; lie on its local y, so the inner rotation stands them up along
;; the scene's z before the spin.
(defun planet-x3d (bearing-rad distance-km)
  (let* ((scene-d 3000.0)
         (half-angle (asin (min 0.999 (/ +planet-radius+ (max distance-km 1.0)))))
         (scene-r (* scene-d (tan half-angle))))
    (format nil "<Transform translation=\"~,1f ~,1f 0\"><Transform rotation=\"1 0 0 1.5708\"><Transform DEF=\"planet-spin\"><Shape><Appearance><ImageTexture id=\"earth-tex\" url=\"\"></ImageTexture><Material diffuseColor=\"0.10 0.18 0.85\" emissiveColor=\"0.05 0.07 0.12\"></Material></Appearance><Sphere radius=\"~,1f\"></Sphere></Shape></Transform></Transform></Transform><TimeSensor DEF=\"planet-clock\" cycleInterval=\"240\" loop=\"true\"></TimeSensor><OrientationInterpolator DEF=\"planet-swing\" key=\"0 0.25 0.5 0.75 1\" keyValue=\"0 1 0 0 0 1 0 1.5708 0 1 0 3.14159 0 1 0 4.71239 0 1 0 6.28319\"></OrientationInterpolator><ROUTE fromNode=\"planet-clock\" fromField=\"fraction_changed\" toNode=\"planet-swing\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"planet-swing\" fromField=\"value_changed\" toNode=\"planet-spin\" toField=\"set_rotation\"></ROUTE>"
            (* scene-d (cos bearing-rad))
            (* scene-d (sin bearing-rad))
            scene-r)))

;; The night itself drifts: the whole starfield swings slowly about
;; the scene's zenith, the way the sky wheels past a ship falling
;; around a world.  One revolution in twenty minutes -- game time
;; runs generous.
(defparameter *sky-drift-x3d*
  "<TimeSensor DEF=\"sky-clock\" cycleInterval=\"1200\" loop=\"true\"></TimeSensor><OrientationInterpolator DEF=\"sky-swing\" key=\"0 0.25 0.5 0.75 1\" keyValue=\"0 0 1 0 0 0 1 1.5708 0 0 1 3.14159 0 0 1 4.71239 0 0 1 6.28319\"></OrientationInterpolator><ROUTE fromNode=\"sky-clock\" fromField=\"fraction_changed\" toNode=\"sky-swing\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"sky-swing\" fromField=\"value_changed\" toNode=\"sky-drift\" toField=\"set_rotation\"></ROUTE>")

;; A torus as one smooth-shaded IndexedFaceSet in world coordinates.
;; The stock torus primitive facets visibly at the rim; this mesh
;; carries a large creaseAngle so the shading rounds over, and
;; texture coordinates so a grain can wrap the tube -- U-REPEATS
;; turns of the texture around the ring keep the texel density even.
(defun torus-x3d (&key center axis across major-radius minor-radius
                       (major-sections 96) (minor-sections 20)
                       (u-repeats 8) texture-id
                       (fallback-color "0.8 0.8 0.8"))
  (let* ((a (unitize-vector axis))
         (u (unitize-vector across))
         (v (cross-vectors u a))
         (n major-sections) (m minor-sections)
         (points (make-string-output-stream))
         (texs (make-string-output-stream))
         (idx (make-string-output-stream)))
    (dotimes (i (1+ n))
      (let* ((alpha (* 2 pi (/ i n)))
             (ring (add-vectors (scalar*vector (cos alpha) v)
                                (scalar*vector (sin alpha) u))))
        (dotimes (j (1+ m))
          (let* ((phi (* 2 pi (/ j m)))
                 (p (add-vectors
                     center
                     (add-vectors
                      (scalar*vector (+ major-radius (* minor-radius (cos phi)))
                                     ring)
                      (scalar*vector (* minor-radius (sin phi)) a)))))
            (format points "~,4f ~,4f ~,4f, " (get-x p) (get-y p) (get-z p))
            (format texs "~,3f ~,3f, " (* u-repeats (/ i n)) (/ j m))))))
    (dotimes (i n)
      (dotimes (j m)
        (let ((p00 (+ (* i (1+ m)) j)))
          (format idx "~d ~d ~d ~d -1 " p00 (+ p00 m 1) (+ p00 m 2) (1+ p00)))))
    (format nil "<Shape><Appearance><ImageTexture id=\"~a\" url=\"\"></ImageTexture><Material diffuseColor=\"~a\"></Material></Appearance><IndexedFaceSet solid=\"false\" creaseAngle=\"3.14159\" coordIndex=\"~a\"><Coordinate point=\"~a\"></Coordinate><TextureCoordinate point=\"~a\"></TextureCoordinate></IndexedFaceSet></Shape>"
            texture-id fallback-color
            (get-output-stream-string idx)
            (get-output-stream-string points)
            (get-output-stream-string texs))))

;; The instrument panel, in wood veneer: same box the cab used to
;; carry in paint, now wearing the grain the page paints client-side.
(defun wood-panel-x3d ()
  "<Transform translation=\"0.79 -0.36 0.415\"><Shape><Appearance><ImageTexture id=\"wood-tex\" url=\"\"></ImageTexture><Material diffuseColor=\"0.48 0.31 0.16\"></Material></Appearance><Box size=\"0.12 1.66 0.27\"></Box></Shape></Transform>")

;; The star-roof: a glass bubble over the open center of the roof,
;; an ellipsoidal dome meshed band by band, its rim landing on the
;; painted roof strips.  solid=false so the glass reads from the
;; bench seats below it, which is the whole point.
(defun star-dome-x3d (&key (center (make-point -0.45 -0.36 1.05))
                           (rx 1.10) (ry 0.72) (rz 0.48)
                           (bands 8) (sectors 28))
  (let ((points (make-string-output-stream))
        (idx (make-string-output-stream)))
    (dotimes (b (1+ bands))
      (let* ((lam (* (/ pi 2) (/ b bands)))
             (cl (cos lam)) (sl (sin lam)))
        (dotimes (s (1+ sectors))
          (let ((th (* 2 pi (/ s sectors))))
            (format points "~,4f ~,4f ~,4f, "
                    (+ (get-x center) (* rx cl (cos th)))
                    (+ (get-y center) (* ry cl (sin th)))
                    (+ (get-z center) (* rz sl)))))))
    (dotimes (b bands)
      (dotimes (s sectors)
        (let ((p00 (+ (* b (1+ sectors)) s)))
          (format idx "~d ~d ~d ~d -1 "
                  p00 (1+ p00) (+ p00 sectors 2) (+ p00 sectors 1)))))
    (format nil "<Shape><Appearance><Material diffuseColor=\"0.67 0.77 0.82\" specularColor=\"0.5 0.55 0.6\" shininess=\"0.6\" transparency=\"0.88\"></Material></Appearance><IndexedFaceSet solid=\"false\" creaseAngle=\"3.14159\" coordIndex=\"~a\"><Coordinate point=\"~a\"></Coordinate></IndexedFaceSet></Shape>"
            (get-output-stream-string idx)
            (get-output-stream-string points))))

;; The cosmic dice, hanging from the mirror on their cords -- the
;; ship's free inertial indicator.  Under thrust they lean away from
;; it: aft on a burn, forward on a retro burn, plumb on a coast.
(defun dice-x3d (lean-deg)
  (let* ((lam (deg->rad lean-deg))
         (dir (make-vector (- (sin lam)) 0 (- (cos lam))))
         (up (make-vector (sin lam) 0 (cos lam))))
    (with-output-to-string (s)
      (with-format (geom-base::x3d s)
        (dolist (y '(-0.325 -0.395))
          (let* ((pivot (make-point 0.77 y 0.91))
                 (cord (make-object 'c-cylinder
                                    :start pivot
                                    :end (add-vectors pivot (scalar*vector 0.06 dir))
                                    :radius 0.0018
                                    :display-controls (list :color +chrome+)))
                 (die (make-object 'box
                                   :center (add-vectors pivot (scalar*vector 0.083 dir))
                                   :orientation
                                   (if (= y -0.395)
                                       (alignment :top up
                                                  :rear (rotate-vector-d
                                                         (make-vector 0 1 0) 35
                                                         (make-vector 0 0 1)))
                                       (alignment :top up))
                                   :width 0.045 :length 0.045 :height 0.045
                                   :display-controls (list :color "#b04040"))))
            (write-the-object cord (cad-output))
            (write-the-object die (cad-output))))))))

;; A gauge needle, cut per render: hub on the panel face, tip swung
;; PHI degrees clockwise from straight up as the driver sees it.
(defun gauge-needle-x3d (hub-y hub-z phi-deg len)
  (let* ((phi (deg->rad phi-deg))
         (needle (make-object 'c-cylinder
                              :start (make-point 0.7135 hub-y hub-z)
                              :end (make-point 0.7135
                                               (- hub-y (* len (sin phi)))
                                               (+ hub-z (* len (cos phi))))
                              :radius 0.003
                              :display-controls (list :color +needle+))))
    (with-output-to-string (s)
      (with-format (geom-base::x3d s)
        (write-the-object needle (geom-base::cad-output))))))

;; The port eye's feed: a live scene-to-texture render on the port
;; flatscreen.  The camera rides outside the cab to port, looking
;; along the ship's own flank -- so the screen shows you your own
;; hull against the stars, the way a hull eye would.  The quad sits
;; just proud of the dark glass; solid=false spares us the winding
;; argument.
(defun eye-feed-x3d (camera-position orientation y-left y-right)
  (format nil
   "<Shape><Appearance><RenderedTexture update=\"always\" dimensions=\"512 512 4\"><Viewpoint position=\"~a\" orientation=\"~a\" fieldOfView=\"0.9\" zNear=\"0.05\" zFar=\"8000\" containerField=\"viewpoint\"></Viewpoint></RenderedTexture></Appearance><IndexedFaceSet solid=\"false\" coordIndex=\"0 1 2 3 -1\"><Coordinate point=\"0.7135 ~,3f 0.345, 0.7135 ~,3f 0.345, 0.7135 ~,3f 0.495, 0.7135 ~,3f 0.495\"></Coordinate><TextureCoordinate point=\"0 0, 1 0, 1 1, 0 1\"></TextureCoordinate></IndexedFaceSet></Shape>"
   camera-position orientation y-left y-right y-right y-left))

(defun port-eye-feed-x3d ()
  (eye-feed-x3d "-2.0 2.0 0.9" "0.24354 -0.62610 -0.74074 2.58052" 0.46 0.22))

(defun starboard-eye-feed-x3d ()
  (eye-feed-x3d "-2.0 -2.72 0.9" "0.85652 -0.33317 -0.39418 1.55754" -0.63 -0.87))

;; The cab is the same for every session, so like the starfield its
;; markup is cut once and shared across all cockpits.
(defvar *cockpit-x3d-cache* nil)

(defun cockpit-x3d ()
  (or *cockpit-x3d-cache*
      (setf *cockpit-x3d-cache*
            (let ((cab (make-object 'cockpit)))
              (string-append
               (with-output-to-string (s)
                 (with-format (geom-base::x3d s)
                   (write-the-object cab (geom-base::cad-output-tree))))
               (the-object cab wheel-rim-x3d)
               (wood-panel-x3d)
               (star-dome-x3d))))))

;; The page: the view from the driver's seat, the galaxy out past
;; where the glass will go.
(define-object cockpit-view (session-control-mixin base-html-page)

  :input-slots
  ((title "Galaxy World — the cockpit")
   (use-ajax? nil)
   (use-svgpanzoom? nil)
   (use-tailwind? nil)
   (favicon-type "image/svg+xml")
   (favicon-path "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'%3E%3Ccircle cx='32' cy='32' r='31' fill='%23000003'/%3E%3Ccircle cx='32' cy='32' r='24' fill='%23e8c839' stroke='%237a6a1f' stroke-width='2'/%3E%3Cellipse cx='32' cy='32' rx='7' ry='19' fill='%23050505'/%3E%3Ccircle cx='25' cy='23' r='4.5' fill='%23fff8d8' opacity='.75'/%3E%3C/svg%3E")

   ;; which eye the page binds at load; x3dom binds the first in
   ;; document order
   (bound-eye :drivers-seat))

  :computed-slots
  (;; The ship's state, held per session: where the nose points,
   ;; how fast and which way she falls.  Space Travel's plane, one
   ;; move per form post.  She starts on the ring, circular and
   ;; prograde, the world abeam to port.
   (heading-deg 90 :settable)
   (vel-x 0 :settable)
   (vel-y +ring-speed+ :settable)
   (pos-x +ring-radius+ :settable)
   (pos-y 0 :settable)
   (moves-count 0 :settable)
   (last-burn :none :settable)
   (last-move-note "riding the ring prograde, the world abeam to port -- coast, and watch the continents go by"
                   :settable)

   (dice-lean (ecase (the last-burn)
                (:none 0) (:forward 38) (:retro -38)))

   (speed (sqrt (+ (* (the vel-x) (the vel-x))
                   (* (the vel-y) (the vel-y)))))
   (radius (sqrt (+ (* (the pos-x) (the pos-x))
                    (* (the pos-y) (the pos-y)))))
   (altitude (- (the radius) +planet-radius+))

   ;; the shape of the road she is on: vis-viva for the size,
   ;; angular momentum for the roundness
   (specific-h (- (* (the pos-x) (the vel-y))
                  (* (the pos-y) (the vel-x))))
   (semi-major (let ((inv (- (/ 2 (the radius))
                             (/ (* (the speed) (the speed)) +mu+))))
                 (when (plusp inv) (/ 1 inv))))
   (eccentricity (when (the semi-major)
                   (sqrt (max 0 (- 1 (/ (* (the specific-h) (the specific-h))
                                        (* +mu+ (the semi-major))))))))
   (apoapsis-alt (when (the semi-major)
                   (- (* (the semi-major) (+ 1 (the eccentricity)))
                      +planet-radius+)))
   (periapsis-alt (when (the semi-major)
                    (- (* (the semi-major) (- 1 (the eccentricity)))
                       +planet-radius+)))
   ;; where the world stands off the nose
   (planet-bearing (- (atan (- (the pos-y)) (- (the pos-x)))
                      (deg->rad (the heading-deg))))
   ;; the speedo sweeps 8 o'clock to 4 o'clock, full scale 8 km/s
   (speedo-phi (+ -120 (* 240 (min 1 (/ (the speed) 8.0)))))

   (helm-form-html
    (with-form-string ()
      (:div :style "display:flex;flex-direction:column;gap:4px;"
        (:div (str (the wheel-control html-string)))
        (:div (str (the gear-control html-string)))
        (:div (str (the pedal-control html-string))))
      (:input :type "submit" :value "make the move"
       :style "margin-top:8px;background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:4px 12px;font-size:12px;cursor:pointer;")))

   (viewpoints-x3d
    (let* ((up (make-vector 0 0 1))
           (eyes
            (list
             (cons :drivers-seat
                   (viewpoint-x3d "drivers-seat" "Driver's seat"
                                  (make-point -0.02 0 0.78)
                                  (make-vector 0.97 0 -0.24)
                                  "1.15" :z-near "0.05" :z-far "8000" :up up))
             (cons :jump-seat
                   (viewpoint-x3d "jump-seat" "Jump seat"
                                  (make-point 0.05 -0.72 0.75)
                                  (unitize-vector (make-vector 0.37 0.72 -0.04))
                                  "1.2" :z-near "0.05" :z-far "8000" :up up))
             (cons :back-seat
                   (viewpoint-x3d "back-seat" "Back seat"
                                  (make-point -1.05 -0.36 0.78)
                                  (unitize-vector (make-vector 1.0 0.18 -0.14))
                                  "1.15" :z-near "0.05" :z-far "8000" :up up))
             (cons :walkaround
                   (viewpoint-x3d "walkaround" "Walkaround"
                                  (make-point -3.3 2.3 2.1)
                                  (unitize-vector (make-vector 3.0 -2.66 -1.55))
                                  "1.0" :z-near "0.05" :z-far "8000" :up up))))
           (chosen (or (assoc (the bound-eye) eyes) (first eyes))))
      (apply #'string-append
             (mapcar #'cdr (cons chosen (remove chosen eyes))))))

   (body
    (with-lhtml-string ()
      (:div :style "position:fixed;inset:0;background:#000;"
        (:|x3d| :id "cockpit-x3d" :width "100%" :height "100%"
          :style "width:100%;height:100%;display:block;"
          (:|Scene|
            (:|Background| :|skyColor| "0 0 0.012")
            (str (the viewpoints-x3d))
            ;; the universe turns around the ship, never the ship
            ;; around the universe -- and inside the heading, the
            ;; night drifts on its own clock
            (:|Transform| :rotation (format nil "0 0 1 ~,5f"
                                            (- (deg->rad (the heading-deg))))
              (:|Transform| :|DEF| "sky-drift"
                (str (starfield-x3d :radius 5000.0d0))))
            (str *sky-drift-x3d*)
            (str (planet-x3d (the planet-bearing) (the radius)))
            (str (cockpit-x3d))
            (str (dice-x3d (the dice-lean)))
            (str (gauge-needle-x3d 0 0.46 (the speedo-phi) 0.055))
            (str (gauge-needle-x3d 0.19 0.45 (the heading-deg) 0.042))
            (str (port-eye-feed-x3d))
            (str (starboard-eye-feed-x3d)))))
      (:div :style "position:fixed;top:14px;left:14px;z-index:10;display:flex;gap:10px;font-family:sans-serif;"
        (:button :id "drivers-seat-btn" :type "button" :onclick "bindEye('drivers-seat')"
          :style (the eye-button-style) "driver's seat")
        (:button :id "jump-seat-btn" :type "button" :onclick "bindEye('jump-seat')"
          :style (the eye-button-style) "jump seat")
        (:button :id "back-seat-btn" :type "button" :onclick "bindEye('back-seat')"
          :style (the eye-button-style) "back seat")
        (:button :id "walkaround-btn" :type "button" :onclick "bindEye('walkaround')"
          :style (the eye-button-style) "walkaround")
        (:a :href "/bridge" :style (string-append (the eye-button-style)
                                                  "text-decoration:none;display:inline-block;")
          "⊙ to the bridge"))
      ;; the helm card: the controls, and what the ship is doing
      (:div :style "position:fixed;bottom:14px;right:14px;z-index:10;background:rgba(16,16,16,0.88);border:1px solid #e8c839;border-radius:10px;padding:12px 16px;font-family:sans-serif;color:#e8c839;font-size:13px;min-width:250px;"
        (:div :style "font-size:14px;margin-bottom:8px;letter-spacing:0.06em;" "THE HELM")
        (str (the helm-form-html))
        (:div :style "margin-top:10px;border-top:1px solid #7a6a1f;padding-top:8px;line-height:1.5;"
          (:div (fmt "heading: ~3,'0d" (mod (round (the heading-deg)) 360)))
          (:div (fmt "speed: ~,2f km/s" (the speed)))
          (:div (fmt "altitude: ~,0f km" (the altitude)))
          (if (the semi-major)
              (htm
               (:div (fmt "high point of the road: ~,0f km" (the apoapsis-alt)))
               (if (< (the periapsis-alt) 100)
                   (htm (:div :style "color:#e07050;"
                          (fmt "low point: ~,0f km — this road meets the sky"
                               (the periapsis-alt))))
                   (htm (:div (fmt "low point of the road: ~,0f km"
                                   (the periapsis-alt))))))
              (htm (:div :style "color:#e07050;"
                     "the road is unbound — the deep dark has you")))
          (:div (fmt "moves made: ~d" (the moves-count)))
          (:div :style "margin-top:6px;font-size:11px;font-style:italic;color:#c9a227;"
            (str (the last-move-note)))))
      (:div :style "position:fixed;bottom:12px;left:14px;z-index:10;color:#c9a227;font-family:sans-serif;font-size:13px;opacity:0.85;"
        "Galaxy World — the cockpit")
      ;; the paint shop: the world's face, the leather, and the wood
      ;; are all painted onto canvases here and handed to the scene's
      ;; ImageTextures as data URLs.  This inline script runs before
      ;; x3dom's load-time init, so the textures are in place when
      ;; the scene first builds.  Deterministic hash noise -- the
      ;; same grain on every visit.
      (:script (str "
function bindEye (id) {
  document.getElementById(id).setAttribute('set_bind','true');
}
(function () {
  function rnd (n) { var x = Math.sin(n * 12.9898 + 78.233) * 43758.5453; return x - Math.floor(x); }
  function tex (id, w, h, draw) {
    var el = document.getElementById(id);
    if (!el) return;
    var c = document.createElement('canvas'); c.width = w; c.height = h;
    draw(c.getContext('2d'), w, h);
    el.setAttribute('url', c.toDataURL());
  }
  tex('earth-tex', 1024, 512, function (g, w, h) {
    var sea = g.createLinearGradient(0, 0, 0, h);
    sea.addColorStop(0, '#16305a'); sea.addColorStop(0.5, '#1d4f8a'); sea.addColorStop(1, '#16305a');
    g.fillStyle = sea; g.fillRect(0, 0, w, h);
    function land (pts, fill) {
      g.beginPath();
      for (var i = 0; i < pts.length; i++) {
        var x = (pts[i][0] + 180) / 360 * w, y = (90 - pts[i][1]) / 180 * h;
        if (i) g.lineTo(x, y); else g.moveTo(x, y);
      }
      g.closePath(); g.fillStyle = fill; g.fill();
    }
    var green = '#3f6f33', dry = '#8a7a4a';
    land([[-165,65],[-130,70],[-95,72],[-75,68],[-58,52],[-70,44],[-76,35],[-81,25],[-97,27],[-92,17],[-84,10],[-92,15],[-105,22],[-117,33],[-124,42],[-150,60]], green);
    land([[-80,9],[-72,11],[-60,5],[-50,0],[-35,-8],[-40,-22],[-55,-35],[-62,-41],[-71,-52],[-75,-45],[-70,-30],[-70,-18],[-78,-5]], green);
    land([[-52,60],[-42,62],[-25,70],[-35,78],[-55,76]], '#dde6ec');
    land([[-17,15],[-10,32],[10,37],[32,31],[43,12],[51,10],[40,-5],[35,-20],[27,-34],[18,-34],[12,-18],[8,-1],[-8,5]], dry);
    land([[-10,36],[-8,43],[-2,48],[-5,58],[5,62],[15,68],[40,70],[70,73],[100,77],[140,72],[170,67],[178,64],[160,60],[152,48],[140,42],[128,38],[122,30],[108,18],[104,8],[98,12],[92,22],[86,20],[80,12],[76,8],[72,20],[66,24],[57,22],[50,28],[42,32],[35,36],[25,36],[15,38],[3,36]], green);
    land([[113,-22],[122,-17],[135,-12],[142,-11],[147,-19],[153,-27],[150,-37],[140,-38],[131,-32],[115,-34]], dry);
    g.fillStyle = '#e8eef2';
    g.fillRect(0, 0, w, h * 0.045); g.fillRect(0, h * 0.94, w, h * 0.06);
    g.fillStyle = 'rgba(255,255,255,0.30)';
    for (var i = 0; i < 26; i++) {
      var cx = rnd(i) * w, cy = (0.15 + 0.7 * rnd(i + 40)) * h, rx = 30 + 60 * rnd(i + 80);
      g.beginPath(); g.ellipse(cx, cy, rx, rx * 0.28, 0, 0, 6.2832); g.fill();
    }
  });
  tex('leather-tex', 256, 256, function (g, w, h) {
    g.fillStyle = '#6b4423'; g.fillRect(0, 0, w, h);
    for (var i = 0; i < 2200; i++) {
      var x = rnd(i) * w, y = rnd(i + 9000) * h, r = 0.6 + 1.8 * rnd(i + 5000);
      g.fillStyle = rnd(i + 700) > 0.5 ? 'rgba(30,15,5,0.16)' : 'rgba(210,160,110,0.10)';
      g.beginPath(); g.arc(x, y, r, 0, 6.2832); g.fill();
    }
    g.strokeStyle = 'rgba(25,12,4,0.28)'; g.lineWidth = 0.7;
    for (var j = 0; j < 130; j++) {
      var x0 = rnd(j + 300) * w, y0 = rnd(j + 400) * h, a = rnd(j + 500) * 6.2832, l = 4 + 10 * rnd(j + 600);
      g.beginPath(); g.moveTo(x0, y0); g.lineTo(x0 + Math.cos(a) * l, y0 + Math.sin(a) * l); g.stroke();
    }
  });
  tex('wood-tex', 512, 128, function (g, w, h) {
    g.fillStyle = '#7a4f28'; g.fillRect(0, 0, w, h);
    for (var y = 0; y < h; y++) {
      var t = 0.5 + 0.5 * Math.sin(y * 0.55 + 3.5 * Math.sin(y * 0.061));
      g.fillStyle = 'rgba(46,24,8,' + (0.10 + 0.22 * t).toFixed(3) + ')';
      g.fillRect(0, y, w, 1);
    }
    g.strokeStyle = 'rgba(30,15,5,0.5)';
    for (var k = 0; k < 7; k++) {
      var yy = (0.12 + 0.76 * rnd(k + 50)) * h;
      g.lineWidth = 0.6 + rnd(k) * 1.2;
      g.beginPath(); g.moveTo(0, yy);
      for (var x = 0; x <= w; x += 16) g.lineTo(x, yy + 3 * Math.sin(x * 0.02 + k * 7));
      g.stroke();
    }
    g.fillStyle = 'rgba(255,220,170,0.05)'; g.fillRect(0, 0, w, h * 0.25);
  });
})();"))))

   (eye-button-style
    "background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:6px 14px;font-size:13px;cursor:pointer;"))

  :objects
  ((wheel-control :type 'gwl:menu-form-control
                  :prompt "wheel: "
                  :default :amidships
                  :choice-plist (list :hard-port "hard over, to port"
                                      :easy-port "easy, to port"
                                      :amidships "amidships"
                                      :easy-starboard "easy, to starboard"
                                      :hard-starboard "hard over, to starboard"))

   (gear-control :type 'gwl:menu-form-control
                 :prompt "gear: "
                 :default :first
                 :choice-plist (list :first "first — close work"
                                     :second "second — approach"
                                     :third "third — cruise"
                                     :reverse "reverse — nose-about"))

   (pedal-control :type 'gwl:menu-form-control
                  :prompt "pedal: "
                  :default :coast
                  :choice-plist (list :coast "clutch in — coast"
                                      :gas "gas — burn"
                                      :brake "brake")))

  :functions
  (;; Fall through DT seconds of gravity: velocity Verlet in
   ;; one-minute substeps.  Returns the new (vx vy px py).
   (fall
    (vx vy px py dt)
    (let ((h 60.0d0))
      (flet ((accel (x y)
               (let* ((r2 (+ (* x x) (* y y)))
                      (r (sqrt r2))
                      (a (- (/ +mu+ r2))))
                 (values (* a (/ x r)) (* a (/ y r))))))
        (dotimes (step (max 1 (round dt h)))
          (multiple-value-bind (ax0 ay0) (accel px py)
            (setq px (+ px (* vx h) (* 0.5 ax0 h h))
                  py (+ py (* vy h) (* 0.5 ay0 h h)))
            (multiple-value-bind (ax1 ay1) (accel px py)
              (setq vx (+ vx (* 0.5 (+ ax0 ax1) h))
                    vy (+ vy (* 0.5 (+ ay0 ay1) h))))))
        (list vx vy px py))))

   ;; One move of the game, folded into the ship's state when the
   ;; helm form posts.  Turn the wheel, then burn (or don't), then
   ;; FALL as long as the gear holds the clutch out -- a minute in
   ;; first, ten in second, an hour in third.  The brake is heard
   ;; and changes nothing.  Meet the sky of the world and you are
   ;; set back on the ring.
   (after-set!
    ()
    (let* ((turn (ecase (the wheel-control value)
                   (:hard-port 30) (:easy-port 10) (:amidships 0)
                   (:easy-starboard -10) (:hard-starboard -30)))
           (gear (the gear-control value))
           (pedal (the pedal-control value))
           (heading (mod (+ (the heading-deg) turn) 360))
           (rad (deg->rad heading))
           (flip (if (eql gear :reverse) -1 1))
           (dv (if (eql pedal :gas) 0.5 0))
           (dt (ecase gear (:first 60) (:second 600) (:third 3600) (:reverse 60)))
           (state (the (fall (+ (the vel-x) (* dv flip (cos rad)))
                             (+ (the vel-y) (* dv flip (sin rad)))
                             (the pos-x) (the pos-y) dt)))
           (r (sqrt (+ (* (third state) (third state))
                       (* (fourth state) (fourth state)))))
           (crashed? (< r +sky-radius+))
           ;; how the burn lay against the road: along it, against
           ;; it, or a sideways shove
           (v0 (sqrt (+ (* (the vel-x) (the vel-x))
                        (* (the vel-y) (the vel-y)))))
           (alignment (if (or (zerop dv) (< v0 0.1))
                          0
                          (/ (+ (* dv flip (cos rad) (the vel-x))
                                (* dv flip (sin rad) (the vel-y)))
                             (* dv v0))))
           (note (cond (crashed?
                        "the world came up to meet you -- back on the ring, falling clean")
                       ((eql pedal :brake)
                        "the brake presses beautifully and does nothing — space doesn't brake")
                       ((and (eql pedal :gas) (> alignment 0.5))
                        "burn along the road — more speed, and the far side of the orbit rises")
                       ((and (eql pedal :gas) (< alignment -0.5))
                        "burn against the road — less speed, and the far side falls")
                       ((eql pedal :gas)
                        "a sideways shove — the road tilts; speed hardly changes")
                       (t "coasting — falling around the world; that curve IS the orbit"))))
      (cond (crashed?
             (the (set-slot! :heading-deg 90))
             (the (set-slot! :vel-x 0))
             (the (set-slot! :vel-y +ring-speed+))
             (the (set-slot! :pos-x +ring-radius+))
             (the (set-slot! :pos-y 0)))
            (t
             (the (set-slot! :heading-deg heading))
             (the (set-slot! :vel-x (first state)))
             (the (set-slot! :vel-y (second state)))
             (the (set-slot! :pos-x (third state)))
             (the (set-slot! :pos-y (fourth state)))))
      (the (set-slot! :last-burn (cond ((or crashed? (not (eql pedal :gas))) :none)
                                       ((eql gear :reverse) :retro)
                                       (t :forward))))
      (the (set-slot! :moves-count (1+ (the moves-count))))
      (the (set-slot! :last-move-note note))
      (tally! :moves)
      (tally! (ecase pedal (:gas :burns) (:coast :coasts) (:brake :brakes)))
      (when crashed? (tally! :crashes))))))
