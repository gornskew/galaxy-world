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

;; The moon: he rides the moon-road (the same ring the bridge chart
;; draws), and for now he stands at its far node, a stand-off
;; outboard of the arrival point -- so a ship taking the road takes
;; it with him close aboard to starboard.
(defparameter +moon-radius+ 1737.0)
(defparameter +moon-road-radius+ 384400.0)
(defparameter +moon-x+ -396400.0)

;; Mars, and the sun's road between worlds: real figures again.
;; Home rides the sun at one au, Mars at 1.524; the leg between them
;; is the sun's own Hohmann, with an escape kick at this end and a
;; capture kick at that one -- patched conics at teaching grain, the
;; same grain as the moon standing still.
(defparameter +mu-sun+ (coerce bsk-astro:+mu-sun+ 'double-float))
(defparameter +au+ (coerce bsk-astro:+au+ 'double-float))
(defparameter +mars-sun-radius+ (* 1.52366d0 +au+))
(defparameter +mars-radius+ (coerce bsk-astro:+req-mars+ 'double-float))
(defparameter +mu-mars+ (coerce bsk-astro:+mu-mars+ 'double-float))
;; the ring she keeps over Mars: the same berth ratio she keeps over
;; home, 3.26 radii up
(defparameter +mars-ring-radius+ 11058.0)

;; The worlds a cockpit can fall around.  Until the road to Mars
;; there was only home; now it is a table.  Each entry: the name the
;; card shows, mu, the body's radius, the sky (meet it and the
;; flight is over), the ring she keeps (and is set back on), the
;; face the page paints, and the material under the face.
(defparameter *worlds*
  (list (list :home :name "home" :mu +mu+ :radius +planet-radius+
              :sky +sky-radius+ :ring +ring-radius+
              :texture "earth-tex"
              :diffuse "0.10 0.18 0.85" :emissive "0.05 0.07 0.12")
        (list :mars :name "Mars" :mu +mu-mars+ :radius +mars-radius+
              :sky (+ +mars-radius+ 100)
              :ring +mars-ring-radius+
              :texture "mars-tex"
              :diffuse "0.62 0.32 0.18" :emissive "0.10 0.05 0.03")))

(defun world-figure (world key)
  (getf (cdr (assoc world *worlds*)) key))

;; The berth keeps a tally of what the helms aboard actually do --
;; the seed of the play-feeds-the-buildout channel.  One image, one
;; table, every cockpit.
(defvar *helm-tallies* (make-hash-table :test 'eq))

(defun tally! (key)
  (incf (gethash key *helm-tallies* 0)))

;; The heavenly bodies as seen from the ship: each drawn at a fixed
;; scene distance with the scale that subtends its true angle, so he
;; grows as you fall toward him and shrinks as you climb away.  The
;; faces are ImageTextures the page paints onto canvases client-side
;; (see the texture script in cockpit-view).  The world also turns:
;; a TimeSensor spins him about his pole, so the continents file
;; past the glass and the orbit FEELS like an orbit.  Sphere poles
;; lie on local y, so an inner rotation stands them up along the
;; scene's z before the spin.  The spin is NEGATIVE about the pole:
;; the ring runs counterclockwise seen from +z and the ship rides
;; it nose-in, so in her frame the world turns clockwise -- the
;; near face scrolls starboard-to-port across the windshield -- and
;; the sky wheels clockwise with it.

(defun scene-body-frame (bearing-rad distance-km body-radius-km)
  "Scene translation (two values) and uniform scale for a body at
the fixed scene distance, subtending its true angle."
  (let* ((scene-d 3000.0)
         (scene-r (* scene-d (tan (asin (min 0.999 (/ body-radius-km
                                                      (max distance-km 1.0))))))))
    (values (* scene-d (cos bearing-rad))
            (* scene-d (sin bearing-rad))
            scene-r)))

(defun bearing-to (px py heading-rad tx ty)
  "Bearing of the point TX TY off the nose of a ship at PX PY with
the given heading; positive to port."
  (- (atan (- ty py) (- tx px)) heading-rad))

(defun dist-to (px py tx ty)
  (sqrt (+ (expt (- tx px) 2) (expt (- ty py) 2))))

(defun body-x3d (prefix texture-id bearing-rad distance-km body-radius-km
                 &key spin? diffuse emissive scale-override)
  "One body: a unit sphere under a DEF'd frame transform carrying
translation and scale, so a voyage can fly both.  SPIN? adds the
pole-spin clock.  SCALE-OVERRIDE authors a different scale than the
subtended one -- a voyage page hides a body until its clock's first
key sets the true size."
  (multiple-value-bind (tx ty s)
      (scene-body-frame bearing-rad distance-km body-radius-km)
    (when scale-override (setq s scale-override))
    (string-append
     (format nil "<Transform DEF=\"~a-frame\" id=\"~a-frame\" translation=\"~,1f ~,1f 0\" scale=\"~,2f ~,2f ~,2f\"><Transform rotation=\"1 0 0 1.5708\"><Transform DEF=\"~a-spin\"><Shape><Appearance><ImageTexture id=\"~a\" url=\"\"></ImageTexture><Material diffuseColor=\"~a\" emissiveColor=\"~a\"></Material></Appearance><Sphere radius=\"1\"></Sphere></Shape></Transform></Transform></Transform>"
             prefix prefix tx ty s s s prefix texture-id diffuse emissive)
     (if spin?
         (format nil "<TimeSensor DEF=\"~a-clock\" cycleInterval=\"240\" loop=\"true\"></TimeSensor><OrientationInterpolator DEF=\"~a-swing\" key=\"0 0.25 0.5 0.75 1\" keyValue=\"0 -1 0 0 0 -1 0 1.5708 0 -1 0 3.14159 0 -1 0 4.71239 0 -1 0 6.28319\"></OrientationInterpolator><ROUTE fromNode=\"~a-clock\" fromField=\"fraction_changed\" toNode=\"~a-swing\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"~a-swing\" fromField=\"value_changed\" toNode=\"~a-spin\" toField=\"set_rotation\"></ROUTE>"
                 prefix prefix prefix prefix prefix prefix)
         ""))))

;; The parked orbit: once she has taken the moon's ring she rides
;; it NOSE-IN, the way she rode at home -- the moon square in the
;; windshield while his face slowly turns, home and the whole sky
;; wheeling once around per orbit (the ship's frame turns with her
;; ring; everything far away appears to turn the other way).  One
;; looping clock, three tracks.  ENABLED? nil parks the clock so a
;; voyage can finish first -- parked BOTH ways (enabled=false AND a
;; far-future startTime), so the watch stays down even where one
;; guard is ignored; the page's script throws both switches.
(defun moon-ambient-x3d (home-bearing sky-angle &key (period 240) enabled?)
  (let ((homes (make-string-output-stream))
        (skys (make-string-output-stream))
        (spins (make-string-output-stream)))
    (dotimes (k 9)
      (let ((b (- home-bearing (* k (/ pi 4)))))
        (format homes "~,1f ~,1f 0 " (* 3000 (cos b)) (* 3000 (sin b)))))
    (dotimes (k 5)
      (format skys "0 0 1 ~,5f " (- sky-angle (* k (/ pi 2))))
      (format spins "0 -1 0 ~,5f " (* k (/ pi 2))))
    (format nil "<TimeSensor DEF=\"ambient-clock\" id=\"ambient-clock\" cycleInterval=\"~d\" loop=\"true\" enabled=\"~a\"~:[ startTime=\"8000000000\"~;~]></TimeSensor><PositionInterpolator DEF=\"amb-home\" key=\"0 0.125 0.25 0.375 0.5 0.625 0.75 0.875 1\" keyValue=\"~a\"></PositionInterpolator><OrientationInterpolator DEF=\"amb-sky\" key=\"0 0.25 0.5 0.75 1\" keyValue=\"~a\"></OrientationInterpolator><OrientationInterpolator DEF=\"amb-moonspin\" key=\"0 0.25 0.5 0.75 1\" keyValue=\"~a\"></OrientationInterpolator><ROUTE fromNode=\"ambient-clock\" fromField=\"fraction_changed\" toNode=\"amb-home\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"ambient-clock\" fromField=\"fraction_changed\" toNode=\"amb-sky\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"ambient-clock\" fromField=\"fraction_changed\" toNode=\"amb-moonspin\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"amb-home\" fromField=\"value_changed\" toNode=\"planet-frame\" toField=\"set_translation\"></ROUTE><ROUTE fromNode=\"amb-sky\" fromField=\"value_changed\" toNode=\"sky-heading\" toField=\"set_rotation\"></ROUTE><ROUTE fromNode=\"amb-moonspin\" fromField=\"value_changed\" toNode=\"moon-spin\" toField=\"set_rotation\"></ROUTE>"
            period (if enabled? "true" "false") enabled?
            (get-output-stream-string homes)
            (get-output-stream-string skys)
            (get-output-stream-string spins))))

;; The voyage, flown by the scene: the sampled road becomes
;; interpolator tracks -- every riding body's frame and the sky's
;; heading -- driven by one one-shot clock.  SAMPLES is a list of
;; (key heading-rad pos-x pos-y).  BODIES is a list of body specs as
;; plists (see the transit-bodies slot): :prefix names the DEF'd
;; frame the tracks drive, :radius the body's true radius, :targets
;; the body's position, one (x y) per sample -- a body standing
;; still repeats one target; a body riding its own road brings a
;; different one for every key.
(defun transit-anim-x3d (samples bodies &key (duration 90))
  (let ((keys (make-string-output-stream))
        (nose (make-string-output-stream))
        (tracks nil))
    (dolist (s samples)
      (destructuring-bind (key h px py) s
        (declare (ignore px py))
        (format keys "~,4f " key)
        (format nose "0 0 1 ~,5f " (- h))))
    (dolist (body bodies)
      (let ((pos (make-string-output-stream))
            (scl (make-string-output-stream)))
        (loop for s in samples
              for target in (getf body :targets)
              do (destructuring-bind (key h px py) s
                   (declare (ignore key))
                   (destructuring-bind (tx ty) target
                     (multiple-value-bind (sx sy sc)
                         (scene-body-frame (bearing-to px py h tx ty)
                                           (dist-to px py tx ty)
                                           (getf body :radius))
                       (format pos "~,1f ~,1f 0 " sx sy)
                       (format scl "~,2f ~,2f ~,2f " sc sc sc)))))
        (push (list (getf body :prefix)
                    (get-output-stream-string pos)
                    (get-output-stream-string scl))
              tracks)))
    (let ((k (get-output-stream-string keys)))
      (with-output-to-string (out)
        (format out "<TimeSensor DEF=\"voyage-clock\" id=\"voyage-clock\" cycleInterval=\"~d\" loop=\"false\"></TimeSensor>"
                duration)
        (dolist (track (nreverse tracks))
          (destructuring-bind (prefix pos scl) track
            (format out "<PositionInterpolator DEF=\"vy-~a-pos\" key=\"~a\" keyValue=\"~a\"></PositionInterpolator><PositionInterpolator DEF=\"vy-~a-scl\" key=\"~a\" keyValue=\"~a\"></PositionInterpolator><ROUTE fromNode=\"voyage-clock\" fromField=\"fraction_changed\" toNode=\"vy-~a-pos\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"voyage-clock\" fromField=\"fraction_changed\" toNode=\"vy-~a-scl\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"vy-~a-pos\" fromField=\"value_changed\" toNode=\"~a-frame\" toField=\"set_translation\"></ROUTE><ROUTE fromNode=\"vy-~a-scl\" fromField=\"value_changed\" toNode=\"~a-frame\" toField=\"set_scale\"></ROUTE>"
                    prefix k pos prefix k scl
                    prefix prefix prefix prefix prefix prefix)))
        (format out "<OrientationInterpolator DEF=\"vy-nose\" key=\"~a\" keyValue=\"~a\"></OrientationInterpolator><ROUTE fromNode=\"voyage-clock\" fromField=\"fraction_changed\" toNode=\"vy-nose\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"vy-nose\" fromField=\"value_changed\" toNode=\"sky-heading\" toField=\"set_rotation\"></ROUTE>"
                k (get-output-stream-string nose))))))

;; The night itself drifts: the whole starfield swings slowly about
;; the scene's zenith, the way the sky wheels past a ship falling
;; around a world.  One revolution in twenty minutes -- game time
;; runs generous.
(defparameter *sky-drift-x3d*
  "<TimeSensor DEF=\"sky-clock\" cycleInterval=\"1200\" loop=\"true\"></TimeSensor><OrientationInterpolator DEF=\"sky-swing\" key=\"0 0.25 0.5 0.75 1\" keyValue=\"0 0 -1 0 0 0 -1 1.5708 0 0 -1 3.14159 0 0 -1 4.71239 0 0 -1 6.28319\"></OrientationInterpolator><ROUTE fromNode=\"sky-clock\" fromField=\"fraction_changed\" toNode=\"sky-swing\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"sky-swing\" fromField=\"value_changed\" toNode=\"sky-drift\" toField=\"set_rotation\"></ROUTE>")

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

;; One die about the origin: a cube reading its six faces off the
;; dice-tex atlas the page paints client-side -- black pips on
;; white, opposite faces summing to seven, 1 up, 6 down, 2 forward.
;; N tells the two dice apart so each ImageTexture keeps its own id.
(defun die-x3d (n)
  (let ((h 0.0225)
        ;; atlas cells (col row) in face-emission order:
        ;; top 1, bottom 6, fore 2, aft 5, port 3, starboard 4
        (cells '((0 0) (2 1) (1 0) (1 1) (2 0) (0 1))))
    (format nil "<Shape><Appearance><ImageTexture id=\"dice-tex-~d\" url=\"\"></ImageTexture><Material diffuseColor=\"0.93 0.91 0.86\"></Material></Appearance><IndexedFaceSet solid=\"false\" coordIndex=\"4 5 6 7 -1 1 0 3 2 -1 5 1 2 6 -1 0 4 7 3 -1 6 2 3 7 -1 0 1 5 4 -1\" texCoordIndex=\"~{~a -1 ~}\"><Coordinate point=\"~{~{~,4f~^ ~}~^, ~}\"></Coordinate><TextureCoordinate point=\"~a\"></TextureCoordinate></IndexedFaceSet></Shape>"
            n
            (loop for f below 6
                  collect (format nil "~d ~d ~d ~d"
                                  (* f 4) (+ 1 (* f 4)) (+ 2 (* f 4)) (+ 3 (* f 4))))
            (list (list (- h) (- h) (- h)) (list h (- h) (- h))
                  (list h h (- h)) (list (- h) h (- h))
                  (list (- h) (- h) h) (list h (- h) h)
                  (list h h h) (list (- h) h h))
            (with-output-to-string (ts)
              (dolist (cell cells)
                (let* ((u0 (/ (first cell) 3.0)) (u1 (/ (1+ (first cell)) 3.0))
                       (v1 (- 1.0 (/ (second cell) 2.0)))
                       (v0 (- 1.0 (/ (1+ (second cell)) 2.0))))
                  (format ts "~,4f ~,4f, ~,4f ~,4f, ~,4f ~,4f, ~,4f ~,4f, "
                          u0 v0 u1 v0 u1 v1 u0 v1)))))))

;; The cosmic dice, hanging from the mirror on their cords -- the
;; ship's free inertial indicator.  Under thrust they lean away from
;; it: aft on a burn, forward on a retro burn, plumb on a coast.
;; The lean is a rotation about y (the swing lies in the x-z plane),
;; and the second die wears a 35-degree twist about its own pole so
;; the pair doesn't hang in lockstep.
(defun dice-x3d (lean-deg)
  (let* ((lam (deg->rad lean-deg))
         (dir (make-vector (- (sin lam)) 0 (- (cos lam)))))
    (with-output-to-string (s)
      (let ((i -1))
        (dolist (y '(-0.325 -0.395))
          (incf i)
          (let* ((pivot (make-point 0.77 y 0.91))
                 (center (add-vectors pivot (scalar*vector 0.083 dir))))
            (with-format (geom-base::x3d s)
              (write-the-object
               (make-object 'c-cylinder
                            :start pivot
                            :end (add-vectors pivot (scalar*vector 0.06 dir))
                            :radius 0.0018
                            :display-controls (list :color +chrome+))
               (cad-output)))
            (format s "<Transform translation=\"~,4f ~,4f ~,4f\"><Transform rotation=\"0 1 0 ~,5f\">~a~a~a</Transform></Transform>"
                    (get-x center) (get-y center) (get-z center)
                    lam
                    (if (= i 1) "<Transform rotation=\"0 0 1 0.61087\">" "")
                    (die-x3d i)
                    (if (= i 1) "</Transform>" ""))))))))

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

;; The eye feeds: live scene-to-texture renders on the two
;; flatscreens.  Each camera stands off outboard of the cab and
;; gazes dead abeam AWAY from the ship, so the frustum holds only
;; sky -- and whatever rides it: the port eye catches the home
;; world when he stands abeam to port, the starboard eye frames the
;; moon close aboard on arrival.  The quad sits just proud of the
;; dark glass; solid=false spares us the winding argument.
(defun eye-feed-x3d (camera-position orientation y-left y-right)
  (format nil
   "<Shape><Appearance><RenderedTexture update=\"always\" dimensions=\"512 512 4\"><Viewpoint position=\"~a\" orientation=\"~a\" fieldOfView=\"0.9\" zNear=\"0.05\" zFar=\"8000\" containerField=\"viewpoint\"></Viewpoint></RenderedTexture></Appearance><IndexedFaceSet solid=\"false\" coordIndex=\"0 1 2 3 -1\"><Coordinate point=\"0.7135 ~,3f 0.345, 0.7135 ~,3f 0.345, 0.7135 ~,3f 0.495, 0.7135 ~,3f 0.495\"></Coordinate><TextureCoordinate point=\"0 0, 1 0, 1 1, 0 1\"></TextureCoordinate></IndexedFaceSet></Shape>"
   camera-position orientation y-left y-right y-right y-left))

(defun port-eye-feed-x3d ()
  (eye-feed-x3d "0.0 2.0 0.8"
                (look-at-orientation (make-vector 0 1 0) (make-vector 0 0 1))
                0.46 0.22))

(defun starboard-eye-feed-x3d ()
  (eye-feed-x3d "0.0 -2.8 0.8"
                (look-at-orientation (make-vector 0 -1 0) (make-vector 0 0 1))
                -0.63 -0.87))

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
   ;; document order.  Nose-in, the world fills the windshield, so
   ;; you land in the driver's seat looking straight down at him.
   (bound-eye :drivers-seat))

  :computed-slots
  (;; The ship's state, held per session: where the nose points,
   ;; how fast and which way she falls.  Space Travel's plane, one
   ;; move per form post.  She starts on the ring, circular and
   ;; prograde, riding NOSE-IN: the nose held on the planet's
   ;; center, the world square in the windshield, the ring flown
   ;; sideways.
   (heading-deg 180 :settable)
   (vel-x 0 :settable)
   (vel-y +ring-speed+ :settable)
   (pos-x +ring-radius+ :settable)
   (pos-y 0 :settable)
   (moves-count 0 :settable)
   (last-burn :none :settable)
   (last-move-note "nose-in on the world, falling sideways past him -- coast, and watch the continents slide by"
                   :settable)

   ;; the world she falls around -- home until a road leads
   ;; elsewhere.  Every figure of the fall reads the world's row in
   ;; the table: the mu, the radius, the sky, the ring she is set
   ;; back on.
   (world :home :settable)
   (world-name (world-figure (the world) :name))
   (world-mu (world-figure (the world) :mu))
   (world-radius (world-figure (the world) :radius))
   (world-sky (world-figure (the world) :sky))
   (world-ring (world-figure (the world) :ring))
   (world-ring-speed (sqrt (/ (the world-mu) (the world-ring))))

   (dice-lean (ecase (the last-burn)
                (:none 0) (:forward 38) (:retro -38)))

   (speed (sqrt (+ (* (the vel-x) (the vel-x))
                   (* (the vel-y) (the vel-y)))))
   (radius (sqrt (+ (* (the pos-x) (the pos-x))
                    (* (the pos-y) (the pos-y)))))
   (altitude (- (the radius) (the world-radius)))

   ;; the shape of the road she is on: vis-viva for the size,
   ;; angular momentum for the roundness
   (specific-h (- (* (the pos-x) (the vel-y))
                  (* (the pos-y) (the vel-x))))
   (semi-major (let ((inv (- (/ 2 (the radius))
                             (/ (* (the speed) (the speed)) (the world-mu)))))
                 (when (plusp inv) (/ 1 inv))))
   (eccentricity (when (the semi-major)
                   (sqrt (max 0 (- 1 (/ (* (the specific-h) (the specific-h))
                                        (* (the world-mu) (the semi-major))))))))
   (apoapsis-alt (when (the semi-major)
                   (- (* (the semi-major) (+ 1 (the eccentricity)))
                      (the world-radius))))
   (periapsis-alt (when (the semi-major)
                    (- (* (the semi-major) (- 1 (the eccentricity)))
                       (the world-radius))))
   ;; where the world stands off the nose
   (planet-bearing (- (atan (- (the pos-y)) (- (the pos-x)))
                      (deg->rad (the heading-deg))))
   ;; and the moon
   (moon-bearing (bearing-to (the pos-x) (the pos-y)
                             (deg->rad (the heading-deg)) +moon-x+ 0))
   (moon-distance (dist-to (the pos-x) (the pos-y) +moon-x+ 0))

   ;; the sampled road of a just-flown programmed voyage, or nil.
   ;; Set by the road functions, cleared by the next hand-flown
   ;; move; while set, the page carries the voyage animation.
   (transit-samples nil :settable)

   ;; which road was flown -- :moon or :mars -- while samples stand
   (transit-target nil :settable)

   ;; the bodies riding the voyage page: specs for body-x3d and
   ;; transit-anim-x3d, set by the road function alongside the
   ;; samples so page and animation author from one list.  Each is a
   ;; plist: :prefix :texture :radius :targets :diffuse :emissive
   ;; :spin? :scale-override.
   (transit-bodies nil :settable)

   ;; she stands the moon watch: set on arrival, cleared by the
   ;; next hand-flown move; while set, the scene loops the parked
   ;; orbit around the moon
   (moon-orbit? nil :settable)

   ;; the sky's authored heading: mid-voyage pages author the scene
   ;; at the road's start and let the clock fly it forward
   (sky-authored-heading-rad (if (the transit-samples)
                                 (second (first (the transit-samples)))
                                 (deg->rad (the heading-deg))))

   (bodies-x3d
    (let ((samples (the transit-samples)))
      (if samples
          ;; a voyage page: every riding body authors from the road's
          ;; first sample, and the one-shot clock flies them all
          (destructuring-bind (key0 h0 px0 py0) (first samples)
            (declare (ignore key0))
            (string-append
             (let ((markup ""))
               (dolist (body (the transit-bodies) markup)
                 (destructuring-bind (tx0 ty0) (first (getf body :targets))
                   (setq markup
                         (string-append markup
                          (body-x3d (getf body :prefix) (getf body :texture)
                                    (bearing-to px0 py0 h0 tx0 ty0)
                                    (dist-to px0 py0 tx0 ty0)
                                    (getf body :radius)
                                    :spin? (getf body :spin?)
                                    :diffuse (getf body :diffuse)
                                    :emissive (getf body :emissive)
                                    :scale-override (getf body :scale-override)))))))
             (transit-anim-x3d samples (the transit-bodies)
                               :duration (if (eql (the transit-target) :mars)
                                             120 90))
             ;; the watch waits, parked, for the moon road to land;
             ;; at Mars the plain scene already stands the watch
             (if (eql (the transit-target) :mars)
                 ""
                 (moon-ambient-x3d (the planet-bearing)
                                   (- (deg->rad (the heading-deg)))
                                   :enabled? nil))))
          (string-append
           (body-x3d "planet" (world-figure (the world) :texture)
                     (the planet-bearing) (the radius) (the world-radius)
                     :spin? t :diffuse (world-figure (the world) :diffuse)
                     :emissive (world-figure (the world) :emissive))
           (if (eql (the world) :home)
               (string-append
                (body-x3d "moon" "moon-tex"
                          (the moon-bearing) (the moon-distance) +moon-radius+
                          :diffuse "0.75 0.74 0.70" :emissive "0.10 0.10 0.09")
                (if (the moon-orbit?)
                    (moon-ambient-x3d (the planet-bearing)
                                      (- (deg->rad (the heading-deg)))
                                      :enabled? t)
                    ""))
               "")))))

   ;; the voyage's page script: first load arms the one-shot clock;
   ;; a reload of the same arrival fast-forwards the scene to the
   ;; road's end instead of replaying five days
   ;; the ARRIVED pose of every voyage body, for the fast-forward
   ;; path below: the world dead ahead at its ring distance, and on
   ;; the mars road the farewell bodies shrunk out of sight (they
   ;; stand au-scale astern -- no visible size at all)
   (voyage-fin-js
    (with-output-to-string (out)
      (let ((fins
             (ecase (the transit-target)
               (:moon
                (append
                 (multiple-value-bind (tx ty sc)
                     (scene-body-frame (the planet-bearing) (the radius)
                                       +planet-radius+)
                   (list (list "planet-frame" tx ty sc)))
                 (multiple-value-bind (tx ty sc)
                     (scene-body-frame (the moon-bearing) (the moon-distance)
                                       +moon-radius+)
                   (list (list "moon-frame" tx ty sc)))))
               (:mars
                (append
                 (multiple-value-bind (tx ty sc)
                     (scene-body-frame (the planet-bearing) (the radius)
                                       (the world-radius))
                   (list (list "planet-frame" tx ty sc)))
                 (list (list "home-frame" -3000.0 0.0 0.001)
                       (list "moon-frame" -3000.0 0.0 0.001)))))))
        (format out "[")
        (loop for (id tx ty sc) in fins
              for first = t then nil
              do (format out "~:[,~%           ~;~]['~a', '~,1f ~,1f 0', '~,2f ~,2f ~,2f']"
                         first id tx ty sc sc sc))
        (format out "]"))))

   (voyage-script-js
    (if (the transit-samples)
        ;; the one-shot key carries the SESSION's identity: a
        ;; bare moves-count key collides across sessions in one
        ;; browser tab -- every fresh session's first voyage
        ;; would read as already-played and snap to arrival
        (format nil "
(function () {
  var key = 'gw-voyage-~a-~d', played = false;
  try { played = !!sessionStorage.getItem(key); } catch (e) {}
  if (played) {
    var fin = ~a;
    fin.forEach(function (f) {
      var el = document.getElementById(f[0]);
      if (el) { el.setAttribute('translation', f[1]); el.setAttribute('scale', f[2]); }
    });
    var sky = document.getElementById('sky-heading');
    if (sky) sky.setAttribute('rotation', '0 0 1 ~,5f');
    var amb = document.getElementById('ambient-clock');
    if (amb) {
      amb.setAttribute('startTime', '' + (Date.now() / 1000));
      amb.setAttribute('enabled', 'true');
    }
  } else {
    try { sessionStorage.setItem(key, '1'); } catch (e) {}
    window.addEventListener('load', function () {
      var ts = document.getElementById('voyage-clock');
      if (ts) ts.setAttribute('startTime', '' + (Date.now() / 1000 + 1));
      setTimeout(function () {
        var amb = document.getElementById('ambient-clock');
        if (amb) {
          amb.setAttribute('startTime', '' + (Date.now() / 1000));
          amb.setAttribute('enabled', 'true');
        }
      }, ~d);
    });
  }
})();"
                (or (the instance-id) "local") (the moves-count)
                (the voyage-fin-js)
                (- (deg->rad (the heading-deg)))
                (if (eql (the transit-target) :mars) 122000 92000))
        ""))
   ;; the speedo sweeps 8 o'clock to 4 o'clock, full scale 8 km/s
   (speedo-phi (+ -120 (* 240 (min 1 (/ (the speed) 8.0)))))

   (helm-form-html
    (with-form-string ()
      (:div :style "display:flex;flex-direction:column;gap:4px;"
        (:div (str (the voyage-control html-string)))
        (:div (str (the wheel-control html-string)))
        (:div (str (the gear-control html-string)))
        (:div (str (the pedal-control html-string))))
      (:input :type "submit" :value "make the move"
       :style "margin-top:8px;background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:4px 12px;font-size:12px;cursor:pointer;")))

   (viewpoints-x3d
    (let* ((up (make-vector 0 0 1))
           (eyes
            (list
             ;; the landing view: same seat, head turned to port,
             ;; the world in the side glass with the A-pillar and
             ;; cowl framing it
             (cons :port-lookout
                   (viewpoint-x3d "port-lookout" "Port lookout"
                                  (make-point -0.02 0 0.78)
                                  (unitize-vector (make-vector 0.26 0.97 0))
                                  "1.15" :z-near "0.05" :z-far "8000" :up up))
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
            (:|Transform| :|DEF| "sky-heading" :|id| "sky-heading"
              :rotation (format nil "0 0 1 ~,5f"
                                (- (the sky-authored-heading-rad)))
              (:|Transform| :|DEF| "sky-drift"
                (str (starfield-x3d :radius 5000.0d0))))
            (str *sky-drift-x3d*)
            (str (the bodies-x3d))
            (str (cockpit-x3d))
            (str (dice-x3d (the dice-lean)))
            (str (gauge-needle-x3d 0 0.46 (the speedo-phi) 0.055))
            (str (gauge-needle-x3d 0.19 0.45 (the heading-deg) 0.042))
            (str (port-eye-feed-x3d))
            (str (starboard-eye-feed-x3d)))))
      (:div :style "position:fixed;top:14px;left:14px;z-index:10;display:flex;gap:10px;font-family:sans-serif;"
        (:button :id "port-lookout-btn" :type "button" :onclick "bindEye('port-lookout')"
          :style (the eye-button-style) "◐ port lookout")
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
      ;; the helm card: the controls, and what the ship is doing.
      ;; Glassy, and the title bar folds it away; the fold survives
      ;; the re-render a move posts (sessionStorage, like the view).
      (:style (str "
#helm-body select { background:rgba(16,16,16,0.6); color:#e8c839; border:1px solid #7a6a1f; border-radius:6px; padding:2px 4px; font-size:12px; }
#helm-body select option { background:#1a1a1a; color:#e8c839; }"))
      (:div :style "position:fixed;bottom:14px;right:14px;z-index:10;background:rgba(16,16,16,0.45);border:1px solid #e8c839;border-radius:10px;padding:10px 16px;font-family:sans-serif;color:#e8c839;font-size:13px;min-width:250px;"
        (:div :style "font-size:14px;letter-spacing:0.06em;cursor:pointer;display:flex;justify-content:space-between;align-items:center;gap:10px;"
              :onclick "toggleHelm()"
          (:span "THE HELM")
          (:span :id "helm-caret" "▾"))
        (:div :id "helm-body" :style "margin-top:8px;"
        (str (the helm-form-html))
        (:div :style "margin-top:10px;border-top:1px solid #7a6a1f;padding-top:8px;line-height:1.5;"
          (:div (fmt "falling around: ~a" (the world-name)))
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
            (str (the last-move-note))))))
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
function toggleHelm () {
  var b = document.getElementById('helm-body');
  var collapsed = b.style.display === 'none';
  b.style.display = collapsed ? '' : 'none';
  document.getElementById('helm-caret').textContent = collapsed ? '\\u25be' : '\\u25b8';
  try { sessionStorage.setItem('gw-helm-collapsed', collapsed ? '0' : '1'); } catch (e) {}
}
(function () {
  try {
    if (sessionStorage.getItem('gw-helm-collapsed') === '1') {
      document.getElementById('helm-body').style.display = 'none';
      document.getElementById('helm-caret').textContent = '\\u25b8';
    }
  } catch (e) {}
})();
(function () {
  function rnd (n) { var x = Math.sin(n * 12.9898 + 78.233) * 43758.5453; return x - Math.floor(x); }
  function tex (ids, w, h, draw) {
    var c = document.createElement('canvas'); c.width = w; c.height = h;
    draw(c.getContext('2d'), w, h);
    var u = c.toDataURL();
    ids.forEach(function (id) {
      var el = document.getElementById(id);
      if (el) el.setAttribute('url', u);
    });
  }
  tex(['earth-tex'], 1024, 512, function (g, w, h) {
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
  tex(['leather-tex'], 256, 256, function (g, w, h) {
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
  tex(['wood-tex'], 512, 128, function (g, w, h) {
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
  tex(['moon-tex'], 512, 256, function (g, w, h) {
    g.fillStyle = '#b6b2a8'; g.fillRect(0, 0, w, h);
    for (var i = 0; i < 9; i++) {
      var x = rnd(i + 20) * w, y = (0.2 + 0.6 * rnd(i + 60)) * h, r = 20 + 55 * rnd(i + 100);
      g.fillStyle = 'rgba(90,88,82,0.35)';
      g.beginPath(); g.ellipse(x, y, r, r * 0.7, rnd(i) * 3, 0, 6.2832); g.fill();
    }
    for (var j = 0; j < 90; j++) {
      var x = rnd(j + 200) * w, y = rnd(j + 300) * h, r = 1.5 + 7 * rnd(j + 400);
      g.fillStyle = 'rgba(70,68,62,0.5)';
      g.beginPath(); g.arc(x, y, r, 0, 6.2832); g.fill();
      g.strokeStyle = 'rgba(235,232,225,0.55)'; g.lineWidth = 1;
      g.beginPath(); g.arc(x - r * 0.15, y - r * 0.15, r * 0.8, 0, 6.2832); g.stroke();
    }
  });
  tex(['mars-tex'], 512, 256, function (g, w, h) {
    var dust = g.createLinearGradient(0, 0, 0, h);
    dust.addColorStop(0, '#8a4a26'); dust.addColorStop(0.5, '#b06034'); dust.addColorStop(1, '#8a4a26');
    g.fillStyle = dust; g.fillRect(0, 0, w, h);
    for (var i = 0; i < 7; i++) {
      var x = rnd(i + 500) * w, y = (0.25 + 0.5 * rnd(i + 550)) * h, r = 30 + 70 * rnd(i + 600);
      g.fillStyle = 'rgba(70,35,20,0.35)';
      g.beginPath(); g.ellipse(x, y, r, r * 0.5, rnd(i + 650) * 3, 0, 6.2832); g.fill();
    }
    for (var j = 0; j < 60; j++) {
      var x = rnd(j + 700) * w, y = rnd(j + 800) * h, r = 1.5 + 6 * rnd(j + 900);
      g.fillStyle = 'rgba(60,28,14,0.45)';
      g.beginPath(); g.arc(x, y, r, 0, 6.2832); g.fill();
    }
    g.fillStyle = '#e8e2d8';
    g.fillRect(0, 0, w, h * 0.03); g.fillRect(0, h * 0.965, w, h * 0.035);
  });
  tex(['dice-tex-0', 'dice-tex-1'], 192, 128, function (g, w, h) {
    g.fillStyle = '#f2efe6'; g.fillRect(0, 0, w, h);
    var pips = [[[0.5,0.5]],
                [[0.28,0.28],[0.72,0.72]],
                [[0.28,0.28],[0.5,0.5],[0.72,0.72]],
                [[0.28,0.28],[0.72,0.28],[0.28,0.72],[0.72,0.72]],
                [[0.28,0.28],[0.72,0.28],[0.5,0.5],[0.28,0.72],[0.72,0.72]],
                [[0.28,0.25],[0.28,0.5],[0.28,0.75],[0.72,0.25],[0.72,0.5],[0.72,0.75]]];
    g.fillStyle = '#151515';
    for (var n = 0; n < 6; n++) {
      var x0 = (n % 3) * 64, y0 = Math.floor(n / 3) * 64;
      pips[n].forEach(function (p) {
        g.beginPath(); g.arc(x0 + p[0] * 64, y0 + p[1] * 64, 6, 0, 6.2832); g.fill();
      });
    }
  });
})();
// The view keeper: mouse navigation saves the camera pose, and any
// reload of the page -- including the full re-render after a helm
// move -- puts you back where you left it, by injecting the saved
// pose as the first viewpoint in the scene before x3dom binds one.
(function () {
  try {
    var saved = sessionStorage.getItem('gw-cockpit-view');
    if (saved) {
      var v = JSON.parse(saved);
      var scene = document.querySelector('#cockpit-x3d scene');
      if (scene && v.p && v.o) {
        var vp = document.createElement('viewpoint');
        vp.setAttribute('id', 'restored-view');
        vp.setAttribute('description', 'Where you left it');
        vp.setAttribute('position', v.p);
        vp.setAttribute('orientation', v.o);
        vp.setAttribute('fieldOfView', '1.15');
        vp.setAttribute('zNear', '0.05');
        vp.setAttribute('zFar', '8000');
        scene.insertBefore(vp, scene.firstChild);
      }
    }
  } catch (e) {}
  window.addEventListener('load', function () {
    document.querySelectorAll('#cockpit-x3d viewpoint').forEach(function (vp) {
      vp.addEventListener('viewpointChanged', function (e) {
        try {
          var p = e.detail.position, o = e.detail.orientation;
          sessionStorage.setItem('gw-cockpit-view', JSON.stringify({
            p: p.x + ' ' + p.y + ' ' + p.z,
            o: o[0].x + ' ' + o[0].y + ' ' + o[0].z + ' ' + o[1]
          }));
        } catch (err) {}
      });
    });
  });
})();")
        (str (the voyage-script-js)))))

   (eye-button-style
    "background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:6px 14px;font-size:13px;cursor:pointer;"))

  :objects
  (;; size 1 = popup menus, not open list boxes
   ;; the road: a fresh session comes with the trip to the moon
   ;; already programmed, so the FIRST move flies it -- then the
   ;; helm is yours, and the astrodynamics lessons start from the
   ;; moon's road
   ;; the roads on offer read the world: home offers the moon and
   ;; Mars; at Mars the helm is yours (the road home is a later
   ;; lesson)
   (voyage-control :type 'gwl:menu-form-control
                   :prompt "the road: "
                   :size 1
                   :default :moon
                   :choice-plist (ecase (the world)
                                   (:home
                                    (list :hand "fly her by hand"
                                          :moon "the programmed road to the moon"
                                          :mars "the programmed road to Mars"))
                                   (:mars
                                    (list :hand "fly her by hand"))))

   (wheel-control :type 'gwl:menu-form-control
                  :prompt "wheel: "
                  :size 1
                  :default :amidships
                  :choice-plist (list :hard-port "hard over, to port"
                                      :easy-port "easy, to port"
                                      :amidships "amidships"
                                      :easy-starboard "easy, to starboard"
                                      :hard-starboard "hard over, to starboard"))

   (gear-control :type 'gwl:menu-form-control
                 :prompt "gear: "
                 :size 1
                 :default :first
                 :choice-plist (list :first "first — close work"
                                     :second "second — approach"
                                     :third "third — cruise"
                                     :reverse "reverse — nose-about"))

   (pedal-control :type 'gwl:menu-form-control
                  :prompt "pedal: "
                  :size 1
                  :default :coast
                  :choice-plist (list :coast "clutch in — coast"
                                      :gas "gas — burn"
                                      :brake "brake")))

  :functions
  (;; Fall through DT seconds of gravity: velocity Verlet in
   ;; one-minute substeps.  Returns the new (vx vy px py).
   (fall
    (vx vy px py dt)
    (let ((h 60.0d0)
          (mu (the world-mu)))
      (flet ((accel (x y)
               (let* ((r2 (+ (* x x) (* y y)))
                      (r (sqrt r2))
                      (a (- (/ mu r2))))
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
   ;; helm form posts.  With the programmed road selected, the move
   ;; IS the voyage; otherwise turn the wheel, then burn (or don't),
   ;; then FALL as long as the gear holds the clutch out -- a minute
   ;; in first, ten in second, an hour in third.  The brake is heard
   ;; and changes nothing.  Meet the sky of the world and you are
   ;; set back on the ring.
   (after-set!
    ()
    (case (the voyage-control value)
      (:moon (the fly-moon-road!))
      (:mars (the fly-mars-road!))
      (t (the make-helm-move!))))

   ;; The programmed road to the moon: the least-fuel two-burn
   ;; Hohmann from the home ring to the moon-road.  The ship's state
   ;; jumps to the arrival ring; the SCENE flies the whole road --
   ;; the sampled transfer becomes the page's voyage animation, the
   ;; nose swinging from nose-in to prograde at the first kick and
   ;; tracking the direction of travel the rest of the way.  Samples
   ;; run uniform in eccentric anomaly; the first key holds the
   ;; pre-burn nose-in pose so the swing reads as its own beat, and
   ;; the last key swings the nose BACK to nose-in on the moon --
   ;; the arrival mirrors the departure, the new world square in
   ;; the windshield.
   (fly-moon-road!
    ()
    (let* ((r1 +ring-radius+) (r2 +moon-road-radius+)
           (a (* 0.5 (+ r1 r2)))
           (e (/ (- r2 r1) (+ r2 r1)))
           (p (* a (- 1 (* e e))))
           (vc2 (sqrt (/ +mu+ r2)))
           (dv1 (- (sqrt (* +mu+ (- (/ 2 r1) (/ 1 a)))) (sqrt (/ +mu+ r1))))
           (dv2 (- vc2 (sqrt (* +mu+ (- (/ 2 r2) (/ 1 a))))))
           (tof-days (/ (* pi (sqrt (/ (* a a a) +mu+))) 86400))
           (vcoeff (sqrt (/ +mu+ p)))
           (n 32)
           (samples (list (list 0.0 pi r1 0)))  ; pre-burn, nose-in
           (prev-h nil))
      (dotimes (i (1+ n))
        (let* ((ecc-anom (* pi (/ i n)))
               (r (* a (- 1 (* e (cos ecc-anom)))))
               (theta (atan (* (sqrt (- 1 (* e e))) (sin ecc-anom))
                            (- (cos ecc-anom) e)))
               (h (atan (* vcoeff (+ e (cos theta)))
                        (* vcoeff (- (sin theta))))))
          ;; unwrap the heading so the nose track never jumps a lap
          (when prev-h
            (loop while (> (- h prev-h) pi) do (decf h (* 2 pi)))
            (loop while (< (- h prev-h) (- pi)) do (incf h (* 2 pi))))
          (setq prev-h h)
          (push (list (+ 0.07 (* 0.86 (/ i n))) h
                      (* r (cos theta)) (* r (sin theta)))
                samples)))
      ;; the closing beat: nose-in on the moon
      (push (list 1.0 pi (- r2) 0) samples)
      (setq samples (nreverse samples))
      (the (set-slot! :heading-deg 180))
      (the (set-slot! :vel-x 0))
      (the (set-slot! :vel-y (- vc2)))
      (the (set-slot! :pos-x (- r2)))
      (the (set-slot! :pos-y 0))
      (the (set-slot! :last-burn :none))
      (the (set-slot! :moves-count (1+ (the moves-count))))
      (the (set-slot! :transit-samples samples))
      (the (set-slot! :transit-target :moon))
      (the (set-slot! :transit-bodies
            (list (list :prefix "planet" :texture "earth-tex"
                        :radius +planet-radius+
                        :targets (make-list (length samples)
                                            :initial-element (list 0 0))
                        :spin? t
                        :diffuse "0.10 0.18 0.85" :emissive "0.05 0.07 0.12")
                  ;; the moon authors HIDDEN: he and home share the
                  ;; same spot at departure, and if home's big
                  ;; texture is still decoding at first paint the
                  ;; moon dot peeks through -- the clock's first key
                  ;; restores his true size the moment the road
                  ;; starts
                  (list :prefix "moon" :texture "moon-tex"
                        :radius +moon-radius+
                        :targets (make-list (length samples)
                                            :initial-element (list +moon-x+ 0))
                        :diffuse "0.75 0.74 0.70" :emissive "0.10 0.10 0.09"
                        :scale-override 0.001))))
      (the (set-slot! :moon-orbit? t))
      (the (set-slot! :last-move-note
                      (format nil "the programmed road: a kick at perigee (+~,2f km/s), ~,1f days falling uphill, a second kick (+~,2f) -- and she settles into the watch around the moon.  The helm is yours."
                              dv1 tof-days dv2)))
      (the voyage-control (set-slot! :value :hand))
      (tally! :moves)
      (tally! :voyages)))

   ;; The programmed road to Mars: the first change of worlds.  Out
   ;; of home's grip on an escape kick, the sun's own Hohmann from
   ;; home's road to Mars's road, and a capture kick onto the ring
   ;; over Mars -- patched conics at teaching grain, real figures
   ;; throughout.  And the window is real: this road only exists
   ;; when Mars stands the right few dozen degrees ahead of home
   ;; along the sun's road, so she waits for it (a departure board
   ;; is a later lesson).  The scene flies the leg heliocentric --
   ;; home and the moon fall astern and shrink to sparks while both
   ;; worlds keep riding their own roads, Mars rises ahead and grows
   ;; -- and the arrival mirrors the departure, nose-in on the new
   ;; world with him close aboard, the moon canon over Mars.
   (fly-mars-road!
    ()
    (let* ((re +au+) (rm +mars-sun-radius+)
           (a (* 0.5 (+ re rm)))
           (e (/ (- rm re) (+ rm re)))
           (p (* a (- 1 (* e e))))
           (vcoeff (sqrt (/ +mu-sun+ p)))
           (n-tr (sqrt (/ +mu-sun+ (* a a a))))
           (tof (/ pi n-tr))
           (tof-months (/ tof (* 86400 30.44)))
           (n-e (sqrt (/ +mu-sun+ (* re re re))))
           (n-m (sqrt (/ +mu-sun+ (* rm rm rm))))
           ;; the window: where Mars must stand at departure so ship
           ;; and world arrive at the same node together
           (mars0 (- pi (* n-m tof)))
           (window-deg (* mars0 (/ 180 pi)))
           ;; the escape kick, from wherever she rides around home
           (r1 (the radius))
           (v-inf-dep (- (* vcoeff (+ 1 e)) (sqrt (/ +mu-sun+ re))))
           (dv1 (- (sqrt (+ (/ (* 2 +mu+) r1) (* v-inf-dep v-inf-dep)))
                   (sqrt (/ +mu+ r1))))
           ;; the capture kick, onto the ring over Mars
           (r2 +mars-ring-radius+)
           (v-circ2 (sqrt (/ +mu-mars+ r2)))
           (v-inf-arr (- (sqrt (/ +mu-sun+ rm)) (* vcoeff (- 1 e))))
           (dv2 (- (sqrt (+ (/ (* 2 +mu-mars+) r2) (* v-inf-arr v-inf-arr)))
                   v-circ2))
           ;; Mars's track carries him one ring outboard of the
           ;; road's end, so the arrival closes with him close aboard
           (rm+ (+ rm r2))
           (n 40)
           ;; where she stands off home's center at departure: the
           ;; transfer's perihelion sits AT home in the sun's frame,
           ;; so this offset fades out over the early samples -- the
           ;; escape unwinding -- keeping home astern at the swing
           ;; beat and the arrival exact
           (ox (the pos-x)) (oy (the pos-y))
           ;; pre-burn beat: wherever and however she stands, in the
           ;; sun's frame -- home rides at (re, 0) at departure
           (samples (list (list 0.0 (deg->rad (the heading-deg))
                                (+ re ox) oy)))
           (earths (list (list re 0)))
           (moons (list (list (+ re +moon-x+) 0)))
           (marses (list (list (* rm+ (cos mars0)) (* rm+ (sin mars0)))))
           (prev-h nil))
      (dotimes (i (1+ n))
        (let* ((ecc-anom (* pi (/ i n)))
               (r (* a (- 1 (* e (cos ecc-anom)))))
               (theta (atan (* (sqrt (- 1 (* e e))) (sin ecc-anom))
                            (- (cos ecc-anom) e)))
               (h (atan (* vcoeff (+ e (cos theta)))
                        (* vcoeff (- (sin theta)))))
               (tim (/ (- ecc-anom (* e (sin ecc-anom))) n-tr))
               (th-e (* n-e tim))
               (th-m (+ mars0 (* n-m tim)))
               (fade (expt (- 1 (/ i n)) 3)))
          ;; unwrap the heading so the nose track never jumps a lap
          (when prev-h
            (loop while (> (- h prev-h) pi) do (decf h (* 2 pi)))
            (loop while (< (- h prev-h) (- pi)) do (incf h (* 2 pi))))
          (setq prev-h h)
          (push (list (+ 0.07 (* 0.86 (/ i n))) h
                      (+ (* r (cos theta)) (* ox fade))
                      (+ (* r (sin theta)) (* oy fade)))
                samples)
          (push (list (* re (cos th-e)) (* re (sin th-e))) earths)
          (push (list (+ (* re (cos th-e)) +moon-x+) (* re (sin th-e)))
                moons)
          (push (list (* rm+ (cos th-m)) (* rm+ (sin th-m))) marses)))
      ;; the closing beat: nose-in on Mars, home dead astern of the sun
      (push (list 1.0 pi (- rm) 0) samples)
      (let ((th-e (* n-e tof)))
        (push (list (* re (cos th-e)) (* re (sin th-e))) earths)
        (push (list (+ (* re (cos th-e)) +moon-x+) (* re (sin th-e))) moons))
      (push (list (- rm+) 0) marses)
      (setq samples (nreverse samples)
            earths (nreverse earths)
            moons (nreverse moons)
            marses (nreverse marses))
      (the (set-slot! :world :mars))
      (the (set-slot! :heading-deg 180))
      (the (set-slot! :vel-x 0))
      (the (set-slot! :vel-y v-circ2))
      (the (set-slot! :pos-x r2))
      (the (set-slot! :pos-y 0))
      (the (set-slot! :last-burn :none))
      (the (set-slot! :moon-orbit? nil))
      (the (set-slot! :moves-count (1+ (the moves-count))))
      (the (set-slot! :transit-samples samples))
      (the (set-slot! :transit-target :mars))
      (the (set-slot! :transit-bodies
            (list (list :prefix "planet" :texture "mars-tex"
                        :radius +mars-radius+
                        :targets marses
                        :diffuse "0.62 0.32 0.18" :emissive "0.10 0.05 0.03"
                        ;; no visible size at au range anyway; the
                        ;; first key sets him true -- the same guard
                        ;; the moon road gives its hidden moon
                        :scale-override 0.001)
                  (list :prefix "home" :texture "earth-tex"
                        :radius +planet-radius+
                        :targets earths
                        :spin? t
                        :diffuse "0.10 0.18 0.85" :emissive "0.05 0.07 0.12")
                  (list :prefix "moon" :texture "moon-tex"
                        :radius +moon-radius+
                        :targets moons
                        :diffuse "0.75 0.74 0.70" :emissive "0.10 0.10 0.09"))))
      (the (set-slot! :last-move-note
            (format nil "the sun's road: she waited on the window -- Mars standing ~d degrees ahead of home -- then a kick out of home's grip (+~,2f km/s), ~,1f months falling around the sun, and a capture kick (+~,2f) onto the ring over Mars.  A new world turns in the glass; the helm is yours."
                    (round window-deg) dv1 tof-months dv2)))
      (the voyage-control (set-slot! :value :hand))
      (tally! :moves)
      (tally! :voyages)
      (tally! :mars-voyages)))

   (make-helm-move!
    ()
    (the (set-slot! :transit-samples nil))
    (the (set-slot! :transit-target nil))
    (the (set-slot! :transit-bodies nil))
    (the (set-slot! :moon-orbit? nil))
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
           (crashed? (< r (the world-sky)))
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
             ;; set back on the ring of whatever world came up
             (the (set-slot! :heading-deg 180))
             (the (set-slot! :vel-x 0))
             (the (set-slot! :vel-y (the world-ring-speed)))
             (the (set-slot! :pos-x (the world-ring)))
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
