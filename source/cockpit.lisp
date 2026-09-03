;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

;; The cockpit: the room the player plays from, holding the HELM.
;; The idiom is a concours-kept classic pickup cab -- stitched bench,
;; painted metal and chrome instrument panel, thin-rim wheel, column
;; shift.  She is an AUTOMATIC (ruled 2026-09-01): no clutch anywhere
;; on this truck, the way American pickups actually are.  How the
;; hull achieves steering and propulsion stays unsaid; the truck
;; needs no explanation.
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
;; The glass is UNLIT (2026-09-02, the sun's arrival): a lit pane at
;; twelve to fifteen percent opacity lays a flat grey haze over
;; whatever stands behind it -- invisible over a headlit globe,
;; plain as day over the night side of one.  Black diffuse, no
;; specular, a faint emissive tint: the tinted window of a car at
;; night, reading only against the stars.
(defparameter +glass-tint+ "#1c2426")
(defparameter +glass-controls+
  (list :color "#000000" :emissive-color +glass-tint+
        :specular-color "#000000" :transparency 0.85))

(defparameter +wheel-rake+ (deg->rad 38))

(define-object cockpit (base-object)

  :computed-slots
  (;; the column's axis, pointing up and back toward the driver
   (column-axis (let ((theta +wheel-rake+))
                  (make-vector (- (cos theta)) 0 (sin theta))))
   ;; the wheel sits smaller and lower than the classic it started
   ;; as: the dials read through it and the glass gets its vertical
   (wheel-center (make-point 0.42 0 0.41))
   ;; in-plane frame of the wheel: across, and up-forward
   (wheel-across (make-vector 0 1 0))
   (wheel-up (cross-vectors (the wheel-across) (the column-axis)))
   (wheel-radius 0.16)
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

   ;; the rim, translucent now: the leather wrap is gone so the
   ;; gauges read straight through the wheel.  Still a fine
   ;; smooth-shaded mesh so the silhouette stays round.
   (wheel-rim-x3d
    ()
    (torus-x3d :center (the wheel-center)
               :axis (the column-axis)
               :across (the wheel-across)
               :major-radius (the wheel-radius)
               :minor-radius 0.011
               :fallback-color "0.72 0.82 0.88"
               :transparency 0.55)))

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
   ;; the starboard gauge's needle is NOT here any more: it is the
   ;; climb gauge now, reading the session's own radial speed, so
   ;; like the speedo and compass needles it renders per cockpit

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

   ;; the wheel, shifter and pedals are NOT here: they are the
   ;; grabbable helm controls, so they render inside the sensor
   ;; rigs -- see helm-rigs-x3d -- and live in :hidden-objects
   ;; below, out of the baked cab tree.

   (column :type 'c-cylinder
           :start (add-vectors (the wheel-center)
                               (scalar*vector -0.07 (the column-axis)))
           :end (add-vectors (the wheel-center)
                             (scalar*vector -0.60 (the column-axis)))
           :radius 0.024
           :display-controls (list :color +paint+))

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
                     :pseudo-inputs (theta sill head)
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
                     :display-controls +glass-controls+)

   ;; the cowl deck closes the gap between panel top and glass base
   (cowl-deck :type 'box
              :sequence (:size 5)
              :pseudo-inputs (theta)
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
               :display-controls +glass-controls+)

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

   ;; the cowl closeout: the sheet metal between the firewall top
   ;; and the windshield sill.  Without it the front corners past
   ;; the panel's ends opened straight to the stars -- the void
   ;; that looked like a malformed third flatscreen.
   (cowl-closeout :type 'box
                  :center (make-point 0.90 -0.36 0.415)
                  :width 0.11
                  :length 1.70
                  :height 0.28
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
               :display-controls +glass-controls+)

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
              :display-controls (list :color +paint+)))

  ;; The grabbable helm: these leaves stay out of the baked cab tree
  ;; (hidden objects never reach cad-output-tree) and are emitted one
  ;; by one inside the DEF'd sensor rigs -- see helm-rigs-x3d.
  :hidden-objects
  (;; the wheel is dished: the hub sits recessed down the column and
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

   ;; the brody knob's stalk: the knob itself is hand-cut markup in
   ;; helm-rigs-x3d, because it PULSES -- a slow ember glow that
   ;; says "grab me" without waiting on anyone's cursor
   (brody-stalk :type 'c-cylinder
                :start (the (rim-point (deg->rad 45)))
                :end (add-vectors (the (rim-point (deg->rad 45)))
                                  (scalar*vector 0.035 (the column-axis)))
                :radius 0.005
                :display-controls (list :color +chrome+))

   ;; the column shift of an automatic: forward, neutral, reverse
   ;; -- down for forward, up for reverse, the way the lever falls
   ;; in a real truck.  No clutch on this vessel.
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

   ;; the pedals: feather, brake, gas -- no clutch, she's an
   ;; automatic, and the light foot gets the third pedal instead.
   ;; They HANG from the dash, the way a real truck hangs them --
   ;; floor-mounted they sat below the glass line and no pointer
   ;; could reach them.  The brake is fully present and does
   ;; nothing whatsoever -- space doesn't brake, and the pedal is
   ;; how the cockpit says so.
   (pedal-plates :type 'box
                 :sequence (:size 3)
                 :center (make-point 0.695
                                     (ecase (the-child index)
                                       (0 0.22) (1 0.05) (2 -0.14))
                                     0.18)
                 :width 0.02
                 :length (ecase (the-child index) (0 0.09) (1 0.09) (2 0.06))
                 :height (ecase (the-child index) (0 0.08) (1 0.08) (2 0.15))
                 :display-controls (list :color +rubber+))
   (pedal-stalks :type 'c-cylinder
                 :sequence (:size 3)
                 :start (make-point 0.72
                                    (ecase (the-child index)
                                      (0 0.22) (1 0.05) (2 -0.14))
                                    0.27)
                 :end (make-point 0.70
                                  (ecase (the-child index)
                                    (0 0.22) (1 0.05) (2 -0.14))
                                  0.21)
                 :radius 0.008
                 :display-controls (list :color +chrome+))))

;; The world the cockpit falls around: the home planet, at the
;; origin of the plane.  Real figures -- km, km/s, km^3/s^2.
(defparameter +mu+ 398600.0d0)
(defparameter +planet-radius+ 6371.0)
(defparameter +sky-radius+ 6471.0)      ; meet this and the flight is over
(defparameter +ring-radius+ 20742.0)    ; the ship's ring, same as the chart's
(defparameter +ring-speed+ (sqrt (/ +mu+ +ring-radius+)))

;; Meet the sky gently and you are not wrecked -- you are DOWN.
;; The cap is the game's landing lesson: shed the road's speed
;; before the surface has to shed it for you.
(defparameter +landing-speed-cap+ 0.6)  ; km/s at touch

;; The moon: he rides the moon-road (the same ring the bridge chart
;; draws), and for now he stands at its far node, a stand-off
;; outboard of the arrival point -- so a ship taking the road takes
;; it with him close aboard to starboard.
(defparameter +moon-radius+ 1737.0)
(defparameter +moon-road-radius+ 384400.0)
;; the moon is a WORLD now, not scenery: his own mu, his own solid
;; sky, and the watch ring the road settles her onto -- the standoff
;; between his node and the road's arrival point
(defparameter +mu-moon+ 4902.8d0)
(defparameter +moon-watch-radius+ 12000.0)
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

;; Jupiter, out at 5.2 au: the same berth ratio again -- and over a
;; well this deep that berth is dearly bought, which is its own
;; lesson
(defparameter +jupiter-sun-radius+ (* 5.2044d0 +au+))
(defparameter +jupiter-radius+ (coerce bsk-astro:+req-jupiter+ 'double-float))
(defparameter +mu-jupiter+ (coerce bsk-astro:+mu-jupiter+ 'double-float))
(defparameter +jupiter-ring-radius+ 232760.0)

;; Saturn, out at 9.58 au, wearing the rings that make him Saturn.
;; bsk-astro's constant sheet stops at Jupiter, so his figures are
;; stated here: JPL values, km^3/s^2 and km.  The berth ratio puts
;; her ring at 196,210 km -- outside the A ring, where the view is.
(defparameter +mu-saturn+ 37931207.0d0)
(defparameter +saturn-radius+ 60268.0)
(defparameter +saturn-sun-radius+ (* 9.5826d0 +au+))
(defparameter +saturn-ring-radius+ 196210.0)
;; the lean that shows the rings: ~26.7 degrees off the plane
(defparameter +saturn-tilt+ 0.4665)

;; One flat annulus in a body frame's equatorial plane, radii in
;; body-radius units so it rides the frame's scale and travels with
;; the body on a voyage.
(defun ring-annulus-x3d (inner outer diffuse emissive transparency
                         &key (sectors 48))
  (let ((points (make-string-output-stream))
        (idx (make-string-output-stream)))
    (dotimes (s (1+ sectors))
      (let* ((th (* 2 pi (/ s sectors)))
             (c (cos th)) (sn (sin th)))
        (format points "~,4f ~,4f 0, ~,4f ~,4f 0, "
                (* inner c) (* inner sn) (* outer c) (* outer sn))))
    (dotimes (s sectors)
      (let ((p (* 2 s)))
        (format idx "~d ~d ~d ~d -1 " p (+ p 1) (+ p 3) (+ p 2))))
    (format nil "<Shape><Appearance><Material diffuseColor=\"~a\" emissiveColor=\"~a\" transparency=\"~,2f\"></Material></Appearance><IndexedFaceSet solid=\"false\" coordIndex=\"~a\"><Coordinate point=\"~a\"></Coordinate></IndexedFaceSet></Shape>"
            diffuse emissive transparency
            (get-output-stream-string idx)
            (get-output-stream-string points))))

;; Saturn's rings at teaching grain: the dim C band inboard, the
;; bright B band, the Cassini gap, the A band outboard -- real radii
;; in units of his own radius.
(defun saturn-rings-x3d ()
  (string-append
   (ring-annulus-x3d 1.24 1.52 "0.55 0.50 0.42" "0.22 0.20 0.17" 0.55)
   (ring-annulus-x3d 1.53 1.95 "0.82 0.75 0.62" "0.35 0.32 0.26" 0.15)
   (ring-annulus-x3d 2.03 2.27 "0.72 0.66 0.55" "0.28 0.26 0.21" 0.30)))

;; The worlds a cockpit can fall around.  Until the road to Mars
;; there was only home; now it is a table, ordered outbound along
;; the sun's road.  Each entry: the name the card shows, mu, the
;; body's radius, the sky (meet it and the flight is over), the
;; ring she keeps (and is set back on), where the world rides the
;; sun, the face the page paints, and the material under the face.
(defparameter *worlds*
  (list (list :home :name "home" :mu +mu+ :radius +planet-radius+
              :sky +sky-radius+ :ring +ring-radius+
              :sun-radius +au+
              :texture "earth-tex" :day-seconds 86164
              :diffuse "0.10 0.18 0.85" :emissive "0.02 0.03 0.05")
        ;; the moon rides HOME, not the sun: no :sun-radius, so the
        ;; outbound popup never offers a sun road from or to him.
        ;; He stands at his node (no clock yet); dominance handoff
        ;; (maybe-hand-off!) is how a ship enters and leaves him.
        (list :moon :name "the moon" :mu +mu-moon+ :radius +moon-radius+
              :sky (+ +moon-radius+ 100)
              :ring +moon-watch-radius+
              :texture "moon-tex" :day-seconds 2360591
              :diffuse "0.75 0.74 0.70" :emissive "0.04 0.04 0.03")
        (list :mars :name "Mars" :mu +mu-mars+ :radius +mars-radius+
              :sky (+ +mars-radius+ 100)
              :ring +mars-ring-radius+
              :sun-radius +mars-sun-radius+
              :texture "mars-tex" :day-seconds 88643
              :diffuse "0.62 0.32 0.18" :emissive "0.04 0.02 0.01")
        (list :jupiter :name "Jupiter" :mu +mu-jupiter+
              :radius +jupiter-radius+
              :sky (+ +jupiter-radius+ 100)
              :ring +jupiter-ring-radius+
              :sun-radius +jupiter-sun-radius+
              :texture "jupiter-tex" :day-seconds 35730
              :diffuse "0.72 0.60 0.44" :emissive "0.04 0.03 0.02")
        (list :saturn :name "Saturn" :mu +mu-saturn+
              :radius +saturn-radius+
              :sky (+ +saturn-radius+ 100)
              :ring +saturn-ring-radius+
              :sun-radius +saturn-sun-radius+
              :texture "saturn-tex" :day-seconds 38362
              :diffuse "0.78 0.68 0.50" :emissive "0.05 0.04 0.03"
              :tilt +saturn-tilt+
              :adornment (saturn-rings-x3d))))

(defun world-figure (world key)
  (getf (cdr (assoc world *worlds*)) key))

;; The view from the ground: parked, the world is not a sphere in
;; the glass any more -- it is the GROUND, a plain out to the
;; horizon in the world's own colors, the stars standing above it.
;; A disk under the cab does the whole job at this grain: wide
;; enough that its rim rides the eye's own horizon, inside the
;; starfield so the night still wraps the upper bowl.  A dim
;; horizon band ties ground to sky the way dusty air does.
(defun ground-x3d (world)
  (let ((diffuse (world-figure world :diffuse))
        (emissive (world-figure world :emissive)))
    (string-append
     ;; the plain itself, just under the cab's floor
     (format nil "<Transform translation=\"0 0 -0.55\"><Transform rotation=\"1 0 0 1.5708\"><Shape><Appearance sortType=\"opaque\"><Material diffuseColor=\"~a\" emissiveColor=\"~a\"></Material></Appearance><Cylinder radius=\"4800\" height=\"0.2\"></Cylinder></Shape></Transform></Transform>"
             diffuse emissive)
     ;; the horizon band: a faint ring standing on the rim
     (format nil "<Transform translation=\"0 0 -0.4\">~a</Transform>"
             (ring-annulus-x3d 4400 4790 diffuse emissive 0.45
                               :sectors 64)))))

;; The berth keeps a LOG BOOK of what the helms aboard actually do
;; and who flies them -- the play-feeds-the-buildout channel.  The
;; tallies, the pilot book, and the stats feed all live in
;; log-book.lisp; the cockpit's whole duty to it is calling TALLY!
;; on each deed and binding *CURRENT-PILOT* around each move.

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

;; The real faces: home and the moon wear NASA imagery -- the Blue
;; Marble composite and the LROC color mosaic -- served as static
;; routes (see publish.lisp), courtesy NASA, public domain.  URLs
;; in the markup mean the faces ride with the scene under EITHER
;; renderer, present at first paint; the remaining worlds keep the
;; page's own painted faces.
(defun texture-url-for (texture-id)
  (cond ((equal texture-id "earth-tex") "/gw-tex/earth.jpg")
        ((equal texture-id "moon-tex") "/gw-tex/moon.jpg")
        ((equal texture-id "mars-tex") "/gw-tex/mars.jpg")
        ((equal texture-id "jupiter-tex") "/gw-tex/jupiter.jpg")
        ((equal texture-id "saturn-tex") "/gw-tex/saturn.jpg")))

(defun earth-sun-local (heading-rad phase-rad &optional (elevation 0.0))
  "The direction TO the sun in the earth sphere's OWN object frame, as
an X3D \"x y z\" string.  The sun is fixed at world +y inside
sky-heading (rotated -HEADING about z); the sphere sits under
tip=Rx(pi/2) then spin=R(0 -1 0, PHASE).  Undoing those brings the
world sun direction into sphere space, closed-form:
sunLocal = (cos el * sin(H-p), sin el, -cos el * cos(H-p)).  The
scene is static between moves, so this bakes as a shader constant."
  (let* ((h heading-rad) (p phase-rad) (el elevation)
         (d (- h p)))
    (format nil "~,5f ~,5f ~,5f"
            (* (cos el) (sin d)) (sin el) (- (* (cos el) (cos d))))))

;; The city-lights shader (X_ITE / GLSL): the day face is lambert-lit
;; by the baked sun direction, the night face shows the Black Marble
;; lights, and smoothstep crosses them at the terminator.  Its own
;; lighting means the scene DirectionalLight does not touch this
;; Shape -- no double-lighting.  Newlines survive as %0A through the
;; data: URI; no // comments (they would swallow the rest of a line).
;; vUv flips T: a ComposedShader samples with the opposite vertical
;; convention from X_ITE's fixed-function ImageTexture, so without
;; the flip the globe rendered upside-down (2026-09-02).
(defparameter *earth-lights-vertex-glsl*
  "precision highp float; uniform mat4 x3d_ProjectionMatrix; uniform mat4 x3d_ModelViewMatrix; attribute vec4 x3d_Vertex; attribute vec3 x3d_Normal; attribute vec4 x3d_TexCoord0; varying vec3 vN; varying vec2 vUv; void main(){ vN = x3d_Normal; vUv = vec2(x3d_TexCoord0.s, 1.0 - x3d_TexCoord0.t); gl_Position = x3d_ProjectionMatrix * x3d_ModelViewMatrix * x3d_Vertex; }")

(defparameter *earth-lights-fragment-glsl*
  "precision highp float; uniform sampler2D dayTex; uniform sampler2D nightTex; uniform vec3 sunLocal; varying vec3 vN; varying vec2 vUv; void main(){ float l = dot(normalize(vN), normalize(sunLocal)); float t = smoothstep(-0.12, 0.12, l); vec3 dayC = texture2D(dayTex, vUv).rgb * (0.06 + 0.94 * max(l, 0.0)); vec3 nightC = texture2D(nightTex, vUv).rgb * 1.5; gl_FragColor = vec4(mix(nightC, dayC, t), 1.0); }")

(defun earth-lights-appearance (prefix day-url night-url sun-local)
  "A ComposedShader Appearance for the city-lights earth: DAY-URL and
NIGHT-URL are raw paths, SUN-LOCAL the baked \"x y z\" from
EARTH-SUN-LOCAL."
  (format nil "<Appearance sortType=\"opaque\"><ComposedShader DEF=\"~a-sh\" language=\"GLSL\"><field name=\"dayTex\" type=\"SFNode\" accessType=\"inputOutput\"><ImageTexture url=\"&quot;~a&quot;\"></ImageTexture></field><field name=\"nightTex\" type=\"SFNode\" accessType=\"inputOutput\"><ImageTexture url=\"&quot;~a&quot;\"></ImageTexture></field><field name=\"sunLocal\" type=\"SFVec3f\" accessType=\"inputOutput\" value=\"~a\"></field><ShaderPart type=\"VERTEX\" url=\"&quot;data:text/plain,~a&quot;\"></ShaderPart><ShaderPart type=\"FRAGMENT\" url=\"&quot;data:text/plain,~a&quot;\"></ShaderPart></ComposedShader></Appearance>"
          prefix day-url night-url sun-local
          (net.aserve:uriencode-string *earth-lights-vertex-glsl*)
          (net.aserve:uriencode-string *earth-lights-fragment-glsl*)))

;; THE SHIP'S CLOCK and the worlds' days.  Galaxy World is
;; turn-based: game time stands still between moves, and each move
;; spends a known stretch of it -- the cadence for a hand-flown
;; turn, the road's own time of flight for a programmed one.  A
;; world's pole angle is the clock read against his day, so every
;; world, parked or flown, wears the face that much time has turned
;; him to, and a flown road turns him by exactly the time it spends.
(defun day-seconds-of (world)
  (or (world-figure world :day-seconds) 86400))

(defun world-phase-rad (world game-seconds)
  "The pole angle WORLD stands at when the ship's clock reads
GAME-SECONDS."
  (mod (* 2 pi (/ game-seconds (day-seconds-of world))) (* 2 pi)))

(defun world-spin-rate (world)
  "Radians of pole turn per game-second."
  (/ (* 2 pi) (day-seconds-of world)))

(defun clock-face (game-seconds)
  "The ship's clock as the helm reads it: days, hours and minutes
since she first sailed; a rewind past the start runs it negative."
  (let* ((s (round game-seconds))
         (neg? (minusp s))
         (s (abs s)))
    (format nil "~:[~;-~]day ~d, ~2,'0d:~2,'0d"
            neg? (floor s 86400) (floor (mod s 86400) 3600)
            (floor (mod s 3600) 60))))

(defun unwrap-heading (h prev)
  "H shifted by whole laps to lie within half a lap of PREV, so a
nose track never jumps a lap."
  (loop while (> (- h prev) pi) do (decf h (* 2 pi)))
  (loop while (< (- h prev) (- pi)) do (incf h (* 2 pi)))
  h)

;; The coast round to a road's departure point.  A programmed road
;; leaves from ONE point on the ring -- the node square across from
;; the world it sails for -- and she may stand anywhere on the ring
;; when she buys it, so the clip opens with her gliding round to
;; that point in her own sense, nose-in, the radius easing onto the
;; ring; the wait is real game-time on the ship's clock, at the
;; ring's own pace.  (Before this the road simply began at the
;; node: the world snapped under her at the start of every voyage.)
;; HEADING0, POS-X, POS-Y and RADIUS are hers in the frame of the
;; world she rides; CENTER-X carries the samples into the scene's
;; frame when that world is not at the origin (the moon at his
;; node).  Returns the coast samples (keys 0..END-KEY), the seconds
;; spent, the last heading (unwrapped from HEADING0), and the angle
;; coasted.
(defun coast-to-node (heading0 pos-x pos-y radius ring mu sense
                      &key (center-x 0) (end-key 0.05) (n 8))
  (let* ((phi0 (atan pos-y pos-x))
         (d (mod (* (- sense) phi0) (* 2 pi)))
         (coast (if (> d (- (* 2 pi) 0.01)) 0.0 d))
         (wait (* coast (sqrt (/ (* ring ring ring) mu))))
         (prev-h heading0)
         (samples nil))
    (dotimes (k n)
      (let* ((f (/ (1+ k) n))
             (phi (+ phi0 (* sense coast f)))
             (r (+ radius (* (- ring radius) f)))
             (h (unwrap-heading (+ phi pi) prev-h)))
        (setq prev-h h)
        (push (list (* end-key f) h
                    (+ center-x (* r (cos phi))) (* r (sin phi)))
              samples)))
    (values (nreverse samples) wait prev-h coast)))

;; a clip compresses days to a breath: past this many full turns a
;; riding world's whirl reads as flicker, so the pole track keeps
;; the fractional turn (the endpoints exact) and this many whole ones
(defparameter +clip-turn-cap+ 6)

(defun body-x3d (prefix texture-id bearing-rad distance-km body-radius-km
                 &key phase diffuse emissive scale-override tilt adornment
                      night-url sun-local)
  "One body: a unit sphere under a DEF'd frame transform carrying
translation and scale, so a voyage can fly both.  PHASE is the pole
angle (radians) the body stands at: the scene is time-frozen
between moves (turn-based), each move advances the ship's clock by
the game-time it spent, and a body's face is that clock read
against his day (world-phase-rad).  On a flown road the voyage
clock turns him on from PHASE by the road's own time of flight
(the pole tracks in transit-anim-x3d).  SCALE-OVERRIDE authors a different scale than the
subtended one -- a voyage page hides a body until its clock's first
key sets the true size.  ADORNMENT is extra markup riding the frame
in body-radius units (Saturn's rings), and TILT leans body and
adornment together so a ring plane shows itself from the cockpit.
The sphere's appearance declares itself opaque: the painted canvas
faces are RGBA data urls, and without the declaration the renderer
sorts the globe among the transparent shapes -- whose order goes by
shape center, and a ring's center IS the globe's center, so the
near ring arc lost the draw and hid behind the globe."
  (multiple-value-bind (tx ty s)
      (scene-body-frame bearing-rad distance-km body-radius-km)
    (when scale-override (setq s scale-override))
    (let* ((day-url (texture-url-for texture-id))
           ;; NIGHT-URL + SUN-LOCAL => the city-lights shader stands in
           ;; for the plain lit Material (earth, X_ITE parked view).
           (appearance
            (if (and night-url sun-local day-url)
                (earth-lights-appearance prefix day-url night-url sun-local)
                (format nil "<Appearance sortType=\"opaque\"><ImageTexture DEF=\"~a\" id=\"~a\" url=\"~a\"></ImageTexture><Material DEF=\"~a-mat\" ambientIntensity=\"0\" diffuseColor=\"~a\" emissiveColor=\"~a\"></Material></Appearance>"
                        texture-id texture-id
                        (if day-url (format nil "&quot;~a&quot;" day-url) "")
                        prefix diffuse emissive))))
    (string-append
     (format nil "<Transform DEF=\"~a-frame\" id=\"~a-frame\" translation=\"~,1f ~,1f 0\" scale=\"~,2f ~,2f ~,2f\">~a<Transform rotation=\"1 0 0 1.5708\"><Transform DEF=\"~a-spin\" id=\"~a-spin\"~a><Shape>~a<Sphere radius=\"1\"></Sphere></Shape></Transform></Transform>~a~a</Transform>"
             prefix prefix tx ty s s s
             (if tilt (format nil "<Transform rotation=\"0 1 0 ~,4f\">" tilt) "")
             prefix prefix
             ;; the pole turns about 0 -1 0 in this inner frame; the
             ;; world stands at PHASE, and a flown road turns him on
             ;; from there by a voyage-clock track (transit-anim-x3d)
             (if phase (format nil " rotation=\"0 -1 0 ~,5f\"" phase) "")
             appearance
             (or adornment "")
             (if tilt "</Transform>" ""))
))))



;; The voyage, flown by the scene: the sampled road becomes
;; interpolator tracks -- every riding body's frame and the sky's
;; heading -- driven by one one-shot clock.  SAMPLES is a list of
;; (key heading-rad pos-x pos-y).  BODIES is a list of body specs as
;; plists (see the transit-bodies slot): :prefix names the DEF'd
;; frame the tracks drive, :radius the body's true radius, :targets
;; the body's position, one (x y) per sample -- a body standing
;; still repeats one target; a body riding its own road brings a
;; different one for every key.
;; TIME-MAP is the clip's clock: (key . game-seconds) pairs, the
;; game-time elapsed at each key; a body carrying :spin-phase and
;; :spin-rate (radians, radians per game-second) gets a pole track
;; read off it, his face turning on from the phase he left with.
;; START-TIME, when given, is unix epoch seconds baked into the
;; clock itself -- a scene DOCUMENT (the X_ITE road) flies the
;; voyage with no page script; the x3dom page keeps starting the
;; clock from JS instead.
(defun unix-now ()
  "Unix epoch seconds -- the time scale X3D SFTime clocks keep."
  (- (get-universal-time) 2208988800))

(defun transit-anim-x3d (samples bodies &key (duration 90) start-time time-map)
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
        (format out "<TimeSensor DEF=\"voyage-clock\" id=\"voyage-clock\" cycleInterval=\"~d\" loop=\"false\"~@[ startTime=\"~,1f\"~]></TimeSensor>"
                duration start-time)
        (dolist (track (nreverse tracks))
          (destructuring-bind (prefix pos scl) track
            (format out "<PositionInterpolator DEF=\"vy-~a-pos\" key=\"~a\" keyValue=\"~a\"></PositionInterpolator><PositionInterpolator DEF=\"vy-~a-scl\" key=\"~a\" keyValue=\"~a\"></PositionInterpolator><ROUTE fromNode=\"voyage-clock\" fromField=\"fraction_changed\" toNode=\"vy-~a-pos\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"voyage-clock\" fromField=\"fraction_changed\" toNode=\"vy-~a-scl\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"vy-~a-pos\" fromField=\"value_changed\" toNode=\"~a-frame\" toField=\"set_translation\"></ROUTE><ROUTE fromNode=\"vy-~a-scl\" fromField=\"value_changed\" toNode=\"~a-frame\" toField=\"set_scale\"></ROUTE>"
                    prefix k pos prefix k scl
                    prefix prefix prefix prefix prefix prefix)))
        ;; the pole tracks: a riding world's face turns on from its
        ;; departure phase by the game-time the road spends, read off
        ;; TIME-MAP, so the face at the clip's end is the face the
        ;; parked page then shows.  Keys land at least every quarter
        ;; turn (an orientation interpolator takes the short way
        ;; between neighbours), the angles reduced to one lap.
        (when time-map
          (let ((total (cdr (car (last time-map)))))
            (dolist (body bodies)
              (let ((phase (getf body :spin-phase))
                    (rate (getf body :spin-rate)))
                (when (and phase rate)
                  (let* ((turn (* rate total))
                         (turn (if (> turn (* 2 pi (1+ +clip-turn-cap+)))
                                   (+ (mod turn (* 2 pi)) (* 2 pi +clip-turn-cap+))
                                   turn))
                         (rate (if (zerop total) 0 (/ turn total)))
                         (skeys (make-string-output-stream))
                         (svals (make-string-output-stream))
                         (prev-key -1.0)
                         (first? t))
                    (loop for ((k0 . t0) (k1 . t1)) on time-map
                          while k1
                          do (let ((m (max 1 (ceiling (abs (* rate (- t1 t0))) (/ pi 2)))))
                               (loop for j from (if first? 0 1) to m
                                     do (let* ((f (/ j m))
                                               (key (+ k0 (* (- k1 k0) f)))
                                               (secs (+ t0 (* (- t1 t0) f))))
                                          (when (> key (+ prev-key 1e-5))
                                            (format skeys "~,5f " key)
                                            (format svals "0 -1 0 ~,5f "
                                                    (mod (+ phase (* rate secs)) (* 2 pi)))
                                            (setq prev-key key))))
                               (setq first? nil)))
                    (format out "<OrientationInterpolator DEF=\"vy-~a-spin\" key=\"~a\" keyValue=\"~a\"></OrientationInterpolator><ROUTE fromNode=\"voyage-clock\" fromField=\"fraction_changed\" toNode=\"vy-~a-spin\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"vy-~a-spin\" fromField=\"value_changed\" toNode=\"~a-spin\" toField=\"set_rotation\"></ROUTE>"
                            (getf body :prefix)
                            (get-output-stream-string skeys)
                            (get-output-stream-string svals)
                            (getf body :prefix) (getf body :prefix) (getf body :prefix))))))))
        (format out "<OrientationInterpolator DEF=\"vy-nose\" key=\"~a\" keyValue=\"~a\"></OrientationInterpolator><ROUTE fromNode=\"voyage-clock\" fromField=\"fraction_changed\" toNode=\"vy-nose\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"vy-nose\" fromField=\"value_changed\" toNode=\"sky-heading\" toField=\"set_rotation\"></ROUTE>"
                k (get-output-stream-string nose))))))

;; THE SUN.  Until 2026-09-02 the scene carried no light at all, so
;; the renderer's default HEADLIGHT lit every face from the camera
;; and every world glowed on its own emissive term: no day side, no
;; night side, the moon never anything but full.  Now the headlight
;; is off and one global DirectionalLight stands for the sun.  It
;; rides INSIDE the sky-heading transform, so it is fixed in the
;; sky like the stars: the nose swing of a flown road and the moon
;; watch's wheeling sky turn it for free, no interpolator of its
;; own.  Where the sun stands: the game keeps no ephemeris yet
;; (the departure-epochs talk), so the sun stands at a fixed world
;; heading: +y, square to the home--moon line.  From the starting
;; ring at +x, nose-in, that is a half-lit home with the
;; terminator running pole to pole down the glass -- the day side
;; to starboard -- and the moon a quarter from home's ring; every
;; lap runs her through full and new.  (Along the home--moon axis
;; the game would open on a dark home or a dark moon.)  Lights
;; carry no elevation in orbit (the ecliptic is the plane she
;; flies); on the ground the sun climbs so the plain is lit rather
;; than grazed.
;; THE FLOODLIGHT: a second global light from the opposite quarter,
;; dark (intensity 0) until the helm's "floodlight" button raises
;; it, to see the night side -- the pilot's lantern, not physics.
;; It is switched by INTENSITY, never by the on field: under X_ITE
;; writing on=false into the off light through the SAI after a
;; scene stood killed the sun's diffuse term for the whole scene
;; (2026-09-02 webshots -- served document fine in isolation, dark
;; disc through the page's wire), and intensity carries no such
;; trap.  Under x3dom the light is page DOM (intensity= attribute);
;; under X_ITE the scene is a document and the SAI sets the field
;; (see *flood-js*; a #gwflood hash forces it on, for webshots).
(defparameter *sun-world-heading* (/ pi 2)
  "World-frame angle the sun lies at, radians: +y, see above.")

(defun sun-light-x3d (&key (elevation 0.0))
  "The sun and the floodlight, for the inside of sky-heading.
ELEVATION lifts the sun above the plane (radians)."
  (let* ((c (cos elevation)) (z (- (sin elevation)))
         (sx (* c (cos *sun-world-heading*)))
         (sy (* c (sin *sun-world-heading*))))
    ;; direction is the way the light TRAVELS: from the sun in
    (format nil "<DirectionalLight DEF=\"sun-light\" id=\"sun-light\" global=\"true\" direction=\"~,4f ~,4f ~,4f\" intensity=\"1\" ambientIntensity=\"0.06\" color=\"1 0.98 0.94\"></DirectionalLight><DirectionalLight DEF=\"flood-light\" id=\"flood-light\" global=\"true\" direction=\"~,4f ~,4f ~,4f\" intensity=\"0\" ambientIntensity=\"0\" color=\"0.85 0.9 1\"></DirectionalLight>"
            (- sx) (- sy) z
            sx sy z)))

(defun cab-light-x3d ()
  "The cab's own lamp: a PointLight over the driver's shoulder whose
RADIUS ends a few units out, so the dash, the wheel and the dice stay
readable whichever way the sun stands while the worlds, thousands of
units off, never see it.  Radius, not scoping: a DirectionalLight
with global=false lit the whole sky under both renderers (2026-09-02
webshots -- x3dom treats every light as global, and X_ITE's ambient
term leaked too), and a point light's reach is honored by both.  No
ambient on either lamp: X_ITE spreads a light's ambient term over
the whole scene regardless of radius (the grey night side of the
second webshot round); a low fill from under the dash does the job
instead."
  "<PointLight DEF=\"cab-light\" location=\"-0.4 0.2 1.0\" radius=\"8\" attenuation=\"1 0 0\" intensity=\"0.9\" ambientIntensity=\"0\" color=\"1 0.97 0.9\"></PointLight><PointLight DEF=\"cab-fill\" location=\"0.3 -0.6 -0.2\" radius=\"6\" attenuation=\"1 0 0\" intensity=\"0.35\" ambientIntensity=\"0\" color=\"0.9 0.95 1\"></PointLight>")

(defparameter *flood-js*
  "window.GW_FLOOD_ON = function () { if ((location.hash + location.search).indexOf('gwflood') > -1) return true; try { return localStorage.getItem('gw-flood') === '1'; } catch (e) { return false; } };
window.GW_FLOOD_APPLY = function (browser) {
  var on = GW_FLOOD_ON();
  var b = document.getElementById('gw-flood-btn');
  if (b) { if (on) b.classList.add('lit'); else b.classList.remove('lit'); }
  var level = on ? 0.6 : 0;
  // x3dom: the light is page DOM (the live scene AND the hidden template both carry the id)
  var els = document.querySelectorAll('#flood-light');
  for (var i = 0; i < els.length; i++) { try { els[i].setAttribute('intensity', String(level)); } catch (e) {} }
  // X_ITE: the scene is a document; reach the node by SAI, and write
  // only on a real change (see the note at sun-light-x3d)
  try {
    var cv = document.querySelector('x3d-canvas');
    var br = browser || (cv && cv.browser);
    var n = br && br.currentScene && br.currentScene.getNamedNode('flood-light');
    if (n) {
      var f = n.getField('intensity');
      var cur = null; try { cur = f.getValue(); } catch (e) { try { cur = n.intensity; } catch (e2) {} }
      var same = (typeof cur === 'number') && Math.abs(cur - level) < 0.001;
      if (!same) { try { f.setValue(level); } catch (e) { n.intensity = level; } }
    }
  } catch (e) {}
};
window.GW_FLOOD_TOGGLE = function () {
  var on = !GW_FLOOD_ON();
  try { localStorage.setItem('gw-flood', on ? '1' : '0'); } catch (e) {}
  GW_FLOOD_APPLY();
};"
  "The floodlight switch: remembered per browser, re-applied after
every scene swap (the state script under x3dom, the wire under
X_ITE), because a fresh scene document stands with the light off.")



;; A torus as one smooth-shaded IndexedFaceSet in world coordinates.
;; The stock torus primitive facets visibly at the rim; this mesh
;; carries a large creaseAngle so the shading rounds over, and
;; texture coordinates so a grain can wrap the tube -- U-REPEATS
;; turns of the texture around the ring keep the texel density even.
(defun torus-x3d (&key center axis across major-radius minor-radius
                       (major-sections 96) (minor-sections 20)
                       (u-repeats 8) texture-id
                       (fallback-color "0.8 0.8 0.8")
                       (transparency 0))
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
    (format nil "<Shape><Appearance>~a<Material diffuseColor=\"~a\"~a></Material></Appearance><IndexedFaceSet solid=\"false\" creaseAngle=\"3.14159\" coordIndex=\"~a\"><Coordinate point=\"~a\"></Coordinate><TextureCoordinate point=\"~a\"></TextureCoordinate></IndexedFaceSet></Shape>"
            (if texture-id
                (format nil "<ImageTexture DEF=\"~a\" id=\"~a\" url=\"\"></ImageTexture>" texture-id texture-id)
                "")
            fallback-color
            (if (plusp transparency)
                (format nil " transparency=\"~,2f\" specularColor=\"0.5 0.55 0.6\" shininess=\"0.6\"" transparency)
                "")
            (get-output-stream-string idx)
            (get-output-stream-string points)
            (get-output-stream-string texs))))

;; The instrument panel, in wood veneer: same box the cab used to
;; carry in paint, now wearing the grain the page paints client-side.
(defun wood-panel-x3d ()
  "<Transform translation=\"0.79 -0.36 0.415\"><Shape><Appearance><ImageTexture DEF=\"wood-tex\" id=\"wood-tex\" url=\"\"></ImageTexture><Material diffuseColor=\"0.48 0.31 0.16\"></Material></Appearance><Box size=\"0.12 1.66 0.27\"></Box></Shape></Transform>")

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
    (format nil "<Shape><Appearance><Material diffuseColor=\"0 0 0\" emissiveColor=\"0.11 0.14 0.15\" transparency=\"0.88\"></Material></Appearance><IndexedFaceSet solid=\"false\" creaseAngle=\"3.14159\" coordIndex=\"~a\"><Coordinate point=\"~a\"></Coordinate></IndexedFaceSet></Shape>"
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
    (format nil "<Shape><Appearance><ImageTexture DEF=\"dice-tex-~d\" id=\"dice-tex-~d\" url=\"\"></ImageTexture><Material diffuseColor=\"0.93 0.91 0.86\"></Material></Appearance><IndexedFaceSet solid=\"false\" coordIndex=\"4 5 6 7 -1 1 0 3 2 -1 5 1 2 6 -1 0 4 7 3 -1 6 2 3 7 -1 0 1 5 4 -1\" texCoordIndex=\"~{~a -1 ~}\"><Coordinate point=\"~{~{~,4f~^ ~}~^, ~}\"></Coordinate><TextureCoordinate point=\"~a\"></TextureCoordinate></IndexedFaceSet></Shape>"
            n n
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
  ;; the render's dimensions match the screen's own 1.6:1 aspect --
  ;; a square render on this quad stretched every world it caught.
  ;; Kept small: each feed re-renders the scene every frame, and
  ;; the wheel's smoothness pays for every extra pixel.
  (format nil
   "<Shape><Appearance><RenderedTexture update=\"always\" dimensions=\"320 200 4\"><Viewpoint position=\"~a\" orientation=\"~a\" fieldOfView=\"0.9\" zNear=\"0.05\" zFar=\"8000\" containerField=\"viewpoint\"></Viewpoint></RenderedTexture></Appearance><IndexedFaceSet solid=\"false\" coordIndex=\"0 1 2 3 -1\"><Coordinate point=\"0.7135 ~,3f 0.345, 0.7135 ~,3f 0.345, 0.7135 ~,3f 0.495, 0.7135 ~,3f 0.495\"></Coordinate><TextureCoordinate point=\"0 0, 1 0, 1 1, 0 1\"></TextureCoordinate></IndexedFaceSet></Shape>"
   camera-position orientation y-left y-right y-right y-left))

(defun port-eye-feed-x3d ()
  (eye-feed-x3d "0.0 2.0 0.8"
                (look-at-orientation (make-vector 0 1 0) (make-vector 0 0 1))
                0.46 0.22))

(defun starboard-eye-feed-x3d ()
  (eye-feed-x3d "0.0 -2.8 0.8"
                (look-at-orientation (make-vector 0 -1 0) (make-vector 0 0 1))
                -0.63 -0.87))

;; The dash radio in the metal, with the starter beside it: the
;; whole turn drives from inside the scene now.  Four preset keys
;; and the tape transport sit on a plate under the speedo, the
;; playing channel lit; the red starter under the climb gauge posts
;; the move.  Renders per cockpit -- the lit key is session state
;; -- and the page's script wires the clicks into the same hidden
;; form controls everything else drives.
(defun dash-radio-x3d (cadence transport)
  (labels ((key-x3d (id mat-id y z len lit? description)
             ;; every key carries its own TouchSensor: sensor
             ;; output events are the reliable interaction channel
             ;; in x3dom, and the forgiving one under a touchpad
             (format nil "<Transform id=\"~a\" DEF=\"~a\"><TouchSensor id=\"~a-touch\" DEF=\"~a-touch\" description=\"~a\"></TouchSensor><Transform translation=\"0.7185 ~,4f ~,4f\"><Shape><Appearance><Material id=\"~a\" DEF=\"~a\" diffuseColor=\"~a\" emissiveColor=\"~a\"></Material></Appearance><Box size=\"0.009 ~,4f 0.028\"></Box></Shape></Transform></Transform>"
                     id id id id (or description "") y z mat-id mat-id
                     (if lit? "0.91 0.78 0.22" "0.13 0.15 0.17")
                     (if lit? "0.55 0.45 0.10" "0.05 0.06 0.07")
                     len)))
    (string-append
     ;; the plate
     "<Transform translation=\"0.7245 0 0.315\"><Shape><Appearance><Material diffuseColor=\"0.06 0.08 0.10\"></Material></Appearance><Box size=\"0.011 0.26 0.085\"></Box></Shape></Transform>"
     ;; the four channel keys, slow to fastest, port to starboard
     (with-output-to-string (out)
       (loop for val in '(:slow :medium :fast :fastest)
             for label in '("slow — a minute a turn" "medium — ten minutes"
                            "fast — an hour" "fastest — a day (goa 145)")
             for i from 0
             for y in '(0.09 0.03 -0.03 -0.09)
             do (format out "~a"
                        (key-x3d (format nil "radio-preset-~d" i)
                                 (format nil "radio-preset-mat-~d" i)
                                 y 0.331 0.048 (eql cadence val) label))))
     ;; the transport: rewind to port, play to starboard
     (key-x3d "radio-tpt-0" "radio-tpt-mat-0" 0.035 0.292 0.05
              (eql transport :rewind) "rewind — the turn falls into the past")
     (key-x3d "radio-tpt-1" "radio-tpt-mat-1" -0.035 0.292 0.05
              (eql transport :play) "play — the turn runs forward")
     ;; the starter, under the climb gauge: press it and the move
     ;; is made
     "<Transform id=\"starter-hit\" DEF=\"starter-hit\"><TouchSensor id=\"starter-touch\" DEF=\"starter-touch\" description=\"the starter — make the move\"></TouchSensor><Transform translation=\"0.722 -0.19 0.315\" rotation=\"0 0 1 1.5708\"><Shape><Appearance><Material diffuseColor=\"0.85 0.87 0.88\"></Material></Appearance><Cylinder radius=\"0.030\" height=\"0.012\"></Cylinder></Shape></Transform><Transform translation=\"0.7165 -0.19 0.315\" rotation=\"0 0 1 1.5708\"><Shape><Appearance><Material DEF=\"starter-mat\" id=\"starter-mat\" diffuseColor=\"0.85 0.29 0.16\" emissiveColor=\"0.30 0.08 0.05\"></Material></Appearance><Cylinder radius=\"0.022\" height=\"0.012\"></Cylinder></Shape></Transform></Transform>")))

;; One hidden leaf, cut through the same lens the cab tree uses, so
;; a rig's geometry matches the cab's finish exactly.
(defun leaf-x3d (leaf)
  (with-output-to-string (s)
    (with-format (geom-base::x3d s)
      (write-the-object leaf (geom-base::cad-output)))))

;; The grabbable helm: the wheel under a CylinderSensor whose axis
;; is the steering column, the shifter and pedals under DEF'd frames
;; the page's script can swing and press.  A drag that starts on any
;; rig belongs to the rig -- the pointing sensor captures it -- so
;; grabbing the wheel never moves the camera.  The wheel geometry is
;; authored in cab coordinates; the rig sandwiches it between a
;; frame that stands the sensor's y axis up the column and the
;; inverse, so the sensor's rotation output turns the wheel about
;; its own column through its own center.  Works with a plain mouse
;; or touch today; the same sensors answer VR controllers when a
;; headset binds.
(defun helm-rigs-x3d (cab)
  (let* ((wc (the-object cab wheel-center))
         (a (the-object cab column-axis))
         (sp (the-object cab shifter-pivot))
         ;; rotation standing local +y up the column axis: axis
         ;; y-cross-a, angle acos(y . a)
         (u (unitize-vector (cross-vectors (make-vector 0 1 0) a)))
         (phi (acos (get-y a))))
    (string-append
     ;; the wheel rig
     (format nil "<Transform translation=\"~,4f ~,4f ~,4f\" rotation=\"~,5f ~,5f ~,5f ~,5f\"><CylinderSensor DEF=\"wheel-sensor\" id=\"wheel-sensor\" diskAngle=\"1.2\" autoOffset=\"true\" description=\"the wheel\"></CylinderSensor><Transform DEF=\"wheel-turn\" id=\"wheel-turn\"><Transform rotation=\"~,5f ~,5f ~,5f ~,5f\"><Transform translation=\"~,4f ~,4f ~,4f\">~a<Group DEF=\"horn-hit\" id=\"horn-hit\"><TouchSensor id=\"horn-touch\" DEF=\"horn-touch\" description=\"the horn — wheel amidships\"></TouchSensor>~a</Group>~a~a</Transform></Transform></Transform></Transform><ROUTE fromNode=\"wheel-sensor\" fromField=\"rotation_changed\" toNode=\"wheel-turn\" toField=\"set_rotation\"></ROUTE>"
             (get-x wc) (get-y wc) (get-z wc)
             (get-x u) (get-y u) (get-z u) phi
             (get-x u) (get-y u) (get-z u) (- phi)
             (- (get-x wc)) (- (get-y wc)) (- (get-z wc))
             (leaf-x3d (the-object cab wheel-hub))
             (leaf-x3d (the-object cab horn-button))
             (string-append
              (apply #'string-append
                     (mapcar #'leaf-x3d
                             (list-elements (the-object cab spokes))))
              (leaf-x3d (the-object cab brody-stalk))
              ;; the brody knob, pulsing like an ember: the glow is
              ;; the affordance -- it turns with the wheel and grabs
              ;; like the wheel, riding inside the same sensor rig
              (let ((kc (add-vectors
                         (the-object cab (rim-point (deg->rad 45)))
                         (scalar*vector 0.048
                                        (the-object cab column-axis)))))
                (format nil "<TimeSensor DEF=\"brody-pulse\" cycleInterval=\"2.6\" loop=\"true\"></TimeSensor><ColorInterpolator DEF=\"brody-glow\" key=\"0 0.5 1\" keyValue=\"0.45 0.10 0.06 0.95 0.42 0.18 0.45 0.10 0.06\"></ColorInterpolator><Transform translation=\"~,4f ~,4f ~,4f\"><Shape><Appearance><Material DEF=\"brody-mat\" diffuseColor=\"0.85 0.29 0.16\" emissiveColor=\"0.45 0.10 0.06\" specularColor=\"0.6 0.4 0.3\" shininess=\"0.5\"></Material></Appearance><Sphere radius=\"0.022\"></Sphere></Shape></Transform><ROUTE fromNode=\"brody-pulse\" fromField=\"fraction_changed\" toNode=\"brody-glow\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"brody-glow\" fromField=\"value_changed\" toNode=\"brody-mat\" toField=\"set_emissiveColor\"></ROUTE>"
                        (get-x kc) (get-y kc) (get-z kc))))
             (the-object cab wheel-rim-x3d))
     ;; the shifter rig: swings about the column axis at the pivot.
     ;; A TouchSensor makes the grab RELIABLE -- x3dom's bare DOM
     ;; click dispatch on scene elements is best-effort, but sensor
     ;; output events are first-class, the same channel the wheel's
     ;; CylinderSensor speaks.
     ;; the knob is the gear indicator too: green forward, white
     ;; neutral, amber reverse -- the state reads off the helm
     ;; itself, no card required.  DEF'd material, lit by script
     ;; under either renderer.
     (format nil "<Transform DEF=\"shifter-rig\" id=\"shifter-rig\" center=\"~,4f ~,4f ~,4f\" rotation=\"~,5f ~,5f ~,5f 0\"><TouchSensor DEF=\"shifter-touch\" id=\"shifter-touch\" description=\"the shifter: forward, neutral, reverse\"></TouchSensor>~a~a</Transform>"
             (get-x sp) (get-y sp) (get-z sp)
             (get-x a) (get-y a) (get-z a)
             (leaf-x3d (the-object cab shifter-lever))
             (let ((kc (add-vectors (the-object cab shifter-pivot)
                                    (make-vector -0.06 -0.34 0.10))))
               (format nil "<Transform translation=\"~,4f ~,4f ~,4f\"><Shape><Appearance><Material id=\"shifter-knob-mat\" DEF=\"shifter-knob-mat\" diffuseColor=\"0.85 0.87 0.88\"></Material></Appearance><Sphere radius=\"0.018\"></Sphere></Shape></Transform>"
                       (get-x kc) (get-y kc) (get-z kc))))
     ;; the pedal rigs, each with its own touch
     (apply #'string-append
            (mapcar (lambda (i)
                      (format nil "<Transform DEF=\"pedal-rig-~d\" id=\"pedal-rig-~d\"><TouchSensor DEF=\"pedal-touch-~d\" id=\"pedal-touch-~d\" description=\"~a\"></TouchSensor>~a~a</Transform>"
                              i i i i
                              (ecase i (0 "feather — a breath of gas")
                                       (1 "the brake") (2 "gas — burn"))
                              (leaf-x3d (the-object cab (pedal-plates i)))
                              (leaf-x3d (the-object cab (pedal-stalks i)))))
                    (list 0 1 2)))
     ;; the steerage lamps: five ticks under the compass, the
     ;; active band lit -- script-lit under either renderer, so
     ;; the wheel's five in-place orientation calls read off the
     ;; helm itself
     (with-output-to-string (lamps)
       (dotimes (i 5)
         (format lamps "<Transform translation=\"0.7185 ~,4f 0.385\"><Shape><Appearance><Material id=\"steer-lamp-mat-~d\" DEF=\"steer-lamp-mat-~d\" diffuseColor=\"0.15 0.16 0.18\" emissiveColor=\"0.04 0.04 0.05\"></Material></Appearance><Box size=\"0.008 0.018 0.012\"></Box></Shape></Transform>"
                 (- 0.235 (* i 0.0225)) i i))))))

;; The plot: draws GW_PLAN on the plan-view canvas.  Orbit pages
;; draw the world, the ring, the road's conic and the ship once;
;; voyage pages fly the dot along the sampled road, following the
;; scene's own voyage clock, and write the ship's coordinates onto
;; the helm as they change -- then settle onto the arrival orbit.
(defparameter *plan-view-js* "
window.togglePlot = function () {
  var b = document.getElementById('plot-body');
  var collapsed = b.style.display === 'none';
  b.style.display = collapsed ? '' : 'none';
  document.getElementById('plot-caret').textContent = collapsed ? '\\u25be' : '\\u25b8';
  try { sessionStorage.setItem('gw-plot-collapsed', collapsed ? '0' : '1'); } catch (e) {}
};
(function () {
  try {
    if (sessionStorage.getItem('gw-plot-collapsed') === '1') {
      document.getElementById('plot-body').style.display = 'none';
      document.getElementById('plot-caret').textContent = '\\u25b8';
    }
  } catch (e) {}
})();
window.GW_DRAW = function () {
  GW_DRAW.gen = (GW_DRAW.gen || 0) + 1;
  var gen = GW_DRAW.gen;
  var P = window.GW_PLAN; if (!P) return;
  var cv = document.getElementById('plot-canvas'); if (!cv) return;
  var ctx = cv.getContext('2d');
  var W = cv.width, H = cv.height, cx = W / 2, cy = H / 2;
  function dims () { W = cv.width; H = cv.height; cx = W / 2; cy = H / 2; }
  // THE PLOT promotes to a HEADS-UP while a voyage flies: centered
  // on the glass, enlarged, translucent -- there is little to see
  // out the windshield mid-road, and the plot IS the show.  The
  // card returns to its corner when the clip lands.
  function hudOn () {
    if (GW_DRAW.hud) return;
    var pc = document.getElementById('plot-card');
    var b = document.getElementById('plot-body');
    if (!pc) return;
    GW_DRAW.hud = { css: pc.style.cssText, w: cv.width, h: cv.height,
                    bodyDisp: b ? b.style.display : '', cv: cv };
    if (b) b.style.display = '';
    pc.style.cssText = 'position:fixed;left:50%;top:45%;transform:translate(-50%,-50%);z-index:9;background:rgba(8,10,16,0.5);border:1px solid rgba(232,200,57,0.85);border-radius:12px;padding:10px 14px;font-family:sans-serif;color:#e8c839;';
    var s = Math.floor(Math.min(window.innerWidth, window.innerHeight) * 0.6);
    cv.width = cv.height = Math.max(300, Math.min(640, s));
    // the state block describes the road's END while the clip
    // flies: dim it and say so, in the plotted blue -- gold and
    // full strength come back with the arrival
    var hs = document.getElementById('helm-state');
    if (hs && !document.getElementById('gw-anticipated')) {
      GW_DRAW.hud.hsOp = hs.style.opacity;
      hs.style.opacity = '0.55';
      var tag = document.createElement('div');
      tag.id = 'gw-anticipated';
      tag.style.cssText = 'font-size:10px;letter-spacing:0.08em;color:#8ea0cf;font-style:italic;';
      tag.textContent = '\\u2014 the road\\u2019s end, as plotted \\u2014';
      hs.insertBefore(tag, hs.firstChild);
    }
  }
  function hudOff () {
    var hstate = GW_DRAW.hud; if (!hstate) return;
    GW_DRAW.hud = null;
    var pc = document.getElementById('plot-card');
    var b = document.getElementById('plot-body');
    if (pc) pc.style.cssText = hstate.css;
    if (b) b.style.display = hstate.bodyDisp;
    if (hstate.cv === cv) { cv.width = hstate.w; cv.height = hstate.h; }
    var hs = document.getElementById('helm-state');
    if (hs) hs.style.opacity = hstate.hsOp || '';
    var tag = document.getElementById('gw-anticipated');
    if (tag && tag.parentNode) tag.parentNode.removeChild(tag);
  }
  function col (s) {
    var p = s.split(/\\s+/).map(parseFloat);
    return 'rgb(' + Math.round(p[0]*255) + ',' + Math.round(p[1]*255) + ',' + Math.round(p[2]*255) + ')';
  }
  function fmtKm (v, mkm) {
    if (mkm) return (v / 1e6).toFixed(2) + ' Mkm';
    return Math.round(v).toLocaleString('en-US') + ' km';
  }
  function coords (x, y, mkm, frame) {
    var line = 'x ' + fmtKm(x, mkm) + ' \\u00b7 y ' + fmtKm(y, mkm);
    var pc = document.getElementById('plot-coords');
    if (pc) pc.textContent = line;
    var el = document.getElementById('coords-line');
    if (el) el.textContent = line + ' \\u2014 ' + frame + ' frame';
  }
  function label (t) {
    var el = document.getElementById('plot-frame-label');
    if (el) el.textContent = t;
  }
  function disk (x, y, r, s, fill, minPx) {
    ctx.beginPath();
    ctx.arc(cx + x*s, cy - y*s, Math.max(r*s, minPx || 1.5), 0, 2*Math.PI);
    ctx.fillStyle = fill; ctx.fill();
  }
  function ship (x, y, hRad, s) {
    // nose first, dot on top: the tick reads as a pointer out of
    // the bow, not a spike through the ship
    var px = cx + x*s, py = cy - y*s;
    ctx.beginPath(); ctx.moveTo(px, py);
    ctx.lineTo(px + 9*Math.cos(hRad), py - 9*Math.sin(hRad));
    ctx.strokeStyle = '#ffffff'; ctx.lineWidth = 1; ctx.stroke();
    ctx.beginPath(); ctx.arc(px, py, 3, 0, 2*Math.PI);
    ctx.fillStyle = '#ffffff'; ctx.fill();
  }
  var O = P.orbit;
  function drawOrbit () {
    dims();
    ctx.clearRect(0, 0, W, H);
    var ext = O.ringR * 1.15;
    if (O.a !== null && O.a !== undefined) {
      var apo = O.a * (1 + O.e);
      if (apo > 0 && apo * 1.08 > ext) ext = apo * 1.08;
    }
    var rr = Math.sqrt(O.x*O.x + O.y*O.y);
    if (rr * 1.15 > ext) ext = rr * 1.15;
    var s = (W/2 - 8) / ext;
    ctx.beginPath(); ctx.arc(cx, cy, O.ringR*s, 0, 2*Math.PI);
    ctx.setLineDash([3, 3]); ctx.strokeStyle = 'rgba(232,200,57,0.45)';
    ctx.lineWidth = 1; ctx.stroke(); ctx.setLineDash([]);
    disk(0, 0, O.worldR, s, col(O.color), 2.5);
    if (O.moon && Math.abs(O.moon.x)*s < W/2) {
      disk(O.moon.x, O.moon.y, O.moon.r, s, '#c8c8c2', 3);
      // close aboard the moon, the watch shows itself: a faint
      // ring of exactly her standoff radius, the ship dot on it
      var dxm = O.x - O.moon.x, dym = O.y - O.moon.y;
      var dm = Math.sqrt(dxm*dxm + dym*dym);
      if (dm < 30000) {
        ctx.beginPath();
        ctx.arc(cx + O.moon.x*s, cy - O.moon.y*s, dm*s, 0, 2*Math.PI);
        ctx.setLineDash([2, 3]); ctx.strokeStyle = 'rgba(200,200,194,0.5)';
        ctx.lineWidth = 1; ctx.stroke(); ctx.setLineDash([]);
      }
    }
    if (O.a !== null && O.a !== undefined && !O.landed) {
      var p = O.a * (1 - O.e*O.e);
      ctx.beginPath(); var started = false;
      for (var nu = 0; nu <= 2*Math.PI + 0.001; nu += 0.02) {
        var denom = 1 + O.e * Math.cos(nu);
        if (denom <= 0.05) { started = false; continue; }
        var r = p / denom, th = O.peri + nu;
        var px = cx + r*Math.cos(th)*s, py = cy - r*Math.sin(th)*s;
        if (started) ctx.lineTo(px, py); else { ctx.moveTo(px, py); started = true; }
      }
      ctx.strokeStyle = '#e8c839'; ctx.lineWidth = 1.2; ctx.stroke();
    }
    ship(O.x, O.y, O.heading * Math.PI/180, s);
    // close aboard the moon the main view cannot separate ship and
    // moon (the standoff is a few px); a magnifier inset shows the
    // neighborhood -- moon to scale, the watch ring, the nose on him
    if (O.moon) {
      var dxm = O.x - O.moon.x, dym = O.y - O.moon.y;
      var dm = Math.sqrt(dxm*dxm + dym*dym);
      if (dm < 30000) {
        var icx = W - 46, icy = 46, ir = 40, si = 26 / dm;
        ctx.beginPath(); ctx.arc(icx, icy, ir, 0, 2*Math.PI);
        ctx.fillStyle = 'rgba(6,6,10,0.92)'; ctx.fill();
        ctx.strokeStyle = 'rgba(232,200,57,0.6)'; ctx.lineWidth = 1; ctx.stroke();
        ctx.beginPath();
        ctx.arc(icx, icy, Math.max(O.moon.r*si, 2), 0, 2*Math.PI);
        ctx.fillStyle = '#c8c8c2'; ctx.fill();
        ctx.beginPath(); ctx.arc(icx, icy, dm*si, 0, 2*Math.PI);
        ctx.setLineDash([2, 3]); ctx.strokeStyle = 'rgba(200,200,194,0.5)';
        ctx.stroke(); ctx.setLineDash([]);
        var sx = icx + dxm*si, sy = icy - dym*si;
        var hh = O.heading * Math.PI/180;
        ctx.beginPath(); ctx.moveTo(sx, sy);
        ctx.lineTo(sx + 8*Math.cos(hh), sy - 8*Math.sin(hh));
        ctx.strokeStyle = '#ffffff'; ctx.lineWidth = 1; ctx.stroke();
        ctx.beginPath(); ctx.arc(sx, sy, 2.5, 0, 2*Math.PI);
        ctx.fillStyle = '#ffffff'; ctx.fill();
      }
    }
    // on final the whole-world view has no pixels for the last few
    // hundred km -- the landing inset zooms on the neighborhood:
    // the surface, the sky line, the nose, and the velocity arrow
    // (green while she is under the speed the surface forgives)
    if (!O.landed && O.alt !== undefined && O.alt < O.closeAlt) {
      var lr = Math.round(W * 0.26), lcx = lr + 8, lcy = lr + 8;
      var lview = Math.max(O.alt * 2.5, 150);
      var ls = (lr - 8) / lview;
      function lpx (x) { return lcx + (x - O.x) * ls; }
      function lpy (y) { return lcy - (y - O.y) * ls; }
      ctx.save();
      ctx.beginPath(); ctx.arc(lcx, lcy, lr, 0, 2*Math.PI);
      ctx.fillStyle = 'rgba(6,6,10,0.92)'; ctx.fill();
      ctx.strokeStyle = 'rgba(232,200,57,0.6)'; ctx.lineWidth = 1; ctx.stroke();
      ctx.save(); ctx.clip();
      ctx.beginPath(); ctx.arc(lpx(0), lpy(0), O.worldR * ls, 0, 2*Math.PI);
      ctx.fillStyle = col(O.color); ctx.fill();
      ctx.beginPath(); ctx.arc(lpx(0), lpy(0), O.skyR * ls, 0, 2*Math.PI);
      ctx.setLineDash([2, 3]); ctx.strokeStyle = 'rgba(224,112,80,0.8)';
      ctx.lineWidth = 1; ctx.stroke(); ctx.setLineDash([]);
      var spd = Math.sqrt(O.vx*O.vx + O.vy*O.vy);
      if (spd > 0.001) {
        var al = 10 + Math.min(26, spd * 24);
        var ux = O.vx / spd, uy = O.vy / spd;
        ctx.beginPath(); ctx.moveTo(lcx, lcy);
        ctx.lineTo(lcx + al*ux, lcy - al*uy);
        ctx.strokeStyle = spd <= O.capKps ? '#5fbf6f' : '#e07050';
        ctx.lineWidth = 2; ctx.stroke();
      }
      var lh = O.heading * Math.PI/180;
      ctx.beginPath(); ctx.moveTo(lcx, lcy);
      ctx.lineTo(lcx + 9*Math.cos(lh), lcy - 9*Math.sin(lh));
      ctx.strokeStyle = '#ffffff'; ctx.lineWidth = 1; ctx.stroke();
      ctx.beginPath(); ctx.arc(lcx, lcy, 2.5, 0, 2*Math.PI);
      ctx.fillStyle = '#ffffff'; ctx.fill();
      ctx.restore();
      ctx.fillStyle = '#e8c839'; ctx.font = '10px sans-serif';
      ctx.textAlign = 'center';
      ctx.fillText(Math.round(O.alt).toLocaleString('en-US') + ' km up, ' +
                   spd.toFixed(2) + ' km/s', lcx, lcy + lr - 6);
      ctx.textAlign = 'start';
      ctx.restore();
    }
    label(O.frame + ' frame \\u2014 km');
    coords(O.x, O.y, false, O.frame);
  }
  var V = P.voyage;
  if (!V) { hudOff(); drawOrbit(); return; }
  // frame the whole road and every rider: the view centers on the
  // track's own bounding box, not the frame's origin
  var S = V.samples;
  var minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
  function grow (x, y) {
    if (x < minX) minX = x; if (x > maxX) maxX = x;
    if (y < minY) minY = y; if (y > maxY) maxY = y;
  }
  S.forEach(function (sm) { grow(sm[2], sm[3]); });
  // off-chart riders must not flatten the road to a speck: the
  // moon rides along during a home climb at 396,000 km out, and
  // framing him would shrink a 14,000 km lift-off to nothing.
  // Only bodies near the road stretch the frame; the far ones
  // still ride the SCENE, just off this chart's edge.
  var rcx = (minX + maxX) / 2, rcy = (minY + maxY) / 2;
  var rspan = Math.max(maxX - minX, maxY - minY, 1);
  V.bodies.forEach(function (b) {
    b.targets.forEach(function (t) {
      if (Math.abs(t[0] - rcx) < rspan * 4 && Math.abs(t[1] - rcy) < rspan * 4)
        grow(t[0], t[1]);
    });
  });
  var oxW = (minX + maxX) / 2, oyW = (minY + maxY) / 2;
  var span = Math.max(maxX - minX, maxY - minY, 1) / 2;
  var vs = (W/2 - 10) / (span * 1.12), mkm = span > 2.5e6;
  function vpx (x) { return cx + (x - oxW) * vs; }
  function vpy (y) { return cy - (y - oyW) * vs; }
  function vDisk (x, y, r, fill, minPx) {
    ctx.beginPath();
    ctx.arc(vpx(x), vpy(y), Math.max(r * vs, minPx || 1.5), 0, 2*Math.PI);
    ctx.fillStyle = fill; ctx.fill();
  }
  function seg (f) {
    if (f <= S[0][0]) return { i: 0, t: 0 };
    for (var i = 0; i < S.length - 1; i++) {
      if (f <= S[i+1][0]) {
        var span = S[i+1][0] - S[i][0];
        return { i: i, t: span < 1e-9 ? 0 : (f - S[i][0]) / span };
      }
    }
    return { i: S.length - 2, t: 1 };
  }
  function drawVoyage (f) {
    dims();
    ctx.clearRect(0, 0, W, H);
    ctx.beginPath();
    S.forEach(function (sm, i) {
      if (i) ctx.lineTo(vpx(sm[2]), vpy(sm[3]));
      else ctx.moveTo(vpx(sm[2]), vpy(sm[3]));
    });
    ctx.strokeStyle = 'rgba(232,200,57,0.5)'; ctx.lineWidth = 1; ctx.stroke();
    if (mkm) vDisk(0, 0, 1, '#ffd76a', 2.5);
    var st = seg(f);
    V.bodies.forEach(function (b) {
      var t0 = b.targets[st.i],
          t1 = b.targets[Math.min(st.i + 1, b.targets.length - 1)];
      vDisk(t0[0] + (t1[0]-t0[0])*st.t, t0[1] + (t1[1]-t0[1])*st.t,
            b.r, '#9a9a94', 2.2);
    });
    var s0 = S[st.i], s1 = S[Math.min(st.i + 1, S.length - 1)];
    var x = s0[2] + (s1[2]-s0[2])*st.t, y = s0[3] + (s1[3]-s0[3])*st.t,
        h = s0[1] + (s1[1]-s0[1])*st.t;
    var px = vpx(x), py = vpy(y);
    ctx.beginPath(); ctx.arc(px, py, 3, 0, 2*Math.PI);
    ctx.fillStyle = '#ffffff'; ctx.fill();
    ctx.beginPath(); ctx.moveTo(px, py);
    ctx.lineTo(px + 8*Math.cos(h), py - 8*Math.sin(h));
    ctx.strokeStyle = '#ffffff'; ctx.lineWidth = 1; ctx.stroke();
    label(V.frame + ' frame \\u2014 ' + (mkm ? 'Mkm' : 'km'));
    coords(x, y, mkm, V.frame);
    // the live readout on the vertical clips: altitude off the
    // ground datum always; vis-viva speed only where a coasting
    // conic stands behind the road (the descent -- watch the fall
    // GAIN the speed the big brake must shed; the powered climb
    // carries altitude alone)
    if (V.live) {
      var rr = Math.sqrt(x*x + y*y);
      var alt = Math.max(0, rr - V.live.ground);
      var suffix = ' \\u00b7 ' + Math.round(alt).toLocaleString('en-US') + ' km up';
      if (V.live.a) {
        var vv = Math.sqrt(Math.max(0, V.live.mu * (2/rr - 1/V.live.a)));
        suffix += ' \\u00b7 ' + vv.toFixed(2) + ' km/s';
      }
      var cl = document.getElementById('coords-line');
      if (cl) cl.textContent += suffix;
      var pc = document.getElementById('plot-coords');
      if (pc) pc.textContent += suffix;
    }
  }
  // ride the same wall clock the voyage script starts the scene's
  // TimeSensor on: GW_VOYAGE_T0 appears at window load on a fresh
  // voyage, and is null when the scene snapped to arrival
  function animate () {
    hudOn();
    function step () {
      if (gen !== GW_DRAW.gen) return;
      var f = (Date.now() - window.GW_VOYAGE_T0) / (V.cycle * 1000);
      if (f >= 1) {
        drawVoyage(1);
        setTimeout(function () {
          if (gen === GW_DRAW.gen) { hudOff(); drawOrbit(); }
        }, 1200);
        return;
      }
      drawVoyage(f < 0 ? 0 : f);
      requestAnimationFrame(step);
    }
    requestAnimationFrame(step);
  }
  var polled = Date.now();
  drawVoyage(1);
  (function waitClock () {
    if (gen !== GW_DRAW.gen) return;
    if (typeof window.GW_VOYAGE_T0 === 'number') { animate(); return; }
    if (window.GW_VOYAGE_T0 === null || Date.now() - polled > 4000) { hudOff(); drawOrbit(); return; }
    setTimeout(waitClock, 100);
  })();
};
if (!window.GW_AUTOED && (location.search + location.hash).indexOf('gwauto') > -1) {
  window.GW_AUTOED = true;
  setTimeout(function () {
    var b = document.getElementById('gw-move-btn');
    if (window.GW_DIAG) GW_DIAG('gwauto: btn=' + !!b + ' gdlAjax=' + (typeof gdlAjax) + ' onclick=' + (b && (b.getAttribute('onclick') || '').slice(0, 40)));
    if (b) b.click();
    if (window.GW_DIAG) GW_DIAG('gwauto: clicked');
  }, 9000);
}")

;; The voice of the road: while a programmed voyage flies, the beats
;; speak into the move-note line in step with the scene's own clock
;; -- the window, the kicks, the long coast -- and the full arrival
;; note returns when the road is flown.
(defparameter *voyage-beats-js* "
window.GW_BEATS = function () {
  GW_BEATS.gen = (GW_BEATS.gen || 0) + 1;
  var gen = GW_BEATS.gen;
  var V = (typeof GW_PLAN === 'object' && GW_PLAN && GW_PLAN.voyage) || null;
  if (!V || !V.beats || !V.beats.length) return;
  var el = document.getElementById('move-note');
  if (!el) return;
  var finale = el.textContent;
  function step () {
    if (gen !== GW_BEATS.gen) return;
    if (typeof window.GW_VOYAGE_T0 !== 'number') {
      if (window.GW_VOYAGE_T0 === null) return;
      setTimeout(step, 100); return;
    }
    var f = (Date.now() - window.GW_VOYAGE_T0) / (V.cycle * 1000);
    if (f >= 1) { el.textContent = finale; return; }
    var text = null;
    for (var i = 0; i < V.beats.length; i++)
      if (V.beats[i][0] <= f) text = V.beats[i][1];
    if (f >= 0 && text) el.textContent = text;
    setTimeout(step, 250);
  }
  step();
};")

;; The hands on the helm: drags and clicks on the rigs mirror into
;; the form controls, which stay the readout and the fallback -- the
;; move still posts through make the move and the same after-set!
;; game step.  Grab the wheel and it turns under the pointer (the
;; sensor route), let go and the wheel card shows the band you left
;; her in; click the shifter through the gate -- forward, neutral,
;; reverse; press a pedal and it gives under the click.  The brake
;; presses beautifully.
(defparameter *helm-hands-js* "
window.GW_WIRE = function () {
  function findSel (opt) {
    var sels = document.querySelectorAll('#helm-body select');
    for (var i = 0; i < sels.length; i++)
      if (sels[i].querySelector('option[value=\"' + opt + '\"]')) return sels[i];
    return null;
  }
  var wheelSel = findSel(':AMIDSHIPS'),
      gearSel  = findSel(':NEUTRAL'),
      pedalSel = findSel(':BURN'),
      roadSel  = findSel(':HAND');
  // The lockouts: what the move ignores, the helm greys.  Neutral
  // sends no pedal to the engines; rewind cools pedal and shifter both
  // (the engines don't light, the sign of dv is moot); a programmed
  // road takes every hand off the wheel; on the ground the wheel
  // steers nothing (takeoff sets its own heading).  The wheel stays
  // LIVE in neutral -- turning in neutral spins the ship, which is
  // its own lesson.  The road select is never locked: it is the way
  // back.  Server truth is unchanged; the greys just say it.
  function lockState () {
    var road = roadSel && roadSel.value !== ':HAND';
    var rw = false;
    var tb = document.querySelectorAll('.transport-btn.lit');
    for (var i = 0; i < tb.length; i++)
      if (tb[i].getAttribute('data-val') === ':REWIND') rw = true;
    var neutral = gearSel && gearSel.value === ':NEUTRAL';
    var landed = false;
    try { landed = !!(window.GW_PLAN && GW_PLAN.orbit && GW_PLAN.orbit.landed); } catch (e) {}
    return { wheel: road || landed,
             gear:  road || rw,
             pedal: road || rw || neutral,
             radio: road };
  }
  function sensorEnable (id, on) {
    var s = document.getElementById(id);
    if (s) s.setAttribute('enabled', on ? 'true' : 'false');
  }
  function dimSel (sel, off) {
    if (!sel) return;
    sel.disabled = off;
    sel.style.opacity = off ? '0.35' : '';
  }
  function applyLockouts () {
    var L = lockState();
    dimSel(wheelSel, L.wheel);
    dimSel(gearSel, L.gear);
    dimSel(pedalSel, L.pedal);
    sensorEnable('wheel-sensor', !L.wheel);
    sensorEnable('horn-touch', !L.wheel);
    sensorEnable('shifter-touch', !L.gear);
    [0, 1, 2].forEach(function (i) { sensorEnable('pedal-touch-' + i, !L.pedal); });
    Array.prototype.forEach.call(
      document.querySelectorAll('.cadence-btn, .transport-btn'),
      function (b) { b.disabled = L.radio; b.style.opacity = L.radio ? '0.35' : ''; });
  }
  window.GW_LOCKS = lockState;
  var wheelPose = { ':HARD-PORT': 1.3, ':EASY-PORT': 0.55, ':AMIDSHIPS': 0,
                    ':EASY-STARBOARD': -0.55, ':HARD-STARBOARD': -1.3 };
  // down for forward, up for reverse -- the way a column shift
  // falls in a real truck
  var gearPose  = { ':FORWARD': -0.5, ':NEUTRAL': 0, ':REVERSE': 0.5 };
  var gearCycle = [':FORWARD', ':NEUTRAL', ':REVERSE'];
  function setWheelPose (ang) {
    var t = document.getElementById('wheel-turn');
    var s = document.getElementById('wheel-sensor');
    if (t) t.setAttribute('rotation', '0 1 0 ' + ang);
    if (s) s.setAttribute('offset', String(ang));
  }
  function setShifterPose (ang) {
    var r = document.getElementById('shifter-rig');
    if (!r) return;
    var rot = r.getAttribute('rotation').split(/\\s+/);
    r.setAttribute('rotation', rot[0] + ' ' + rot[1] + ' ' + rot[2] + ' ' + ang);
  }
  function bandFor (ang) {
    if (ang > 0.9) return ':HARD-PORT';
    if (ang > 0.22) return ':EASY-PORT';
    if (ang < -0.9) return ':HARD-STARBOARD';
    if (ang < -0.22) return ':EASY-STARBOARD';
    return ':AMIDSHIPS';
  }
  function norm (a) {
    while (a > Math.PI) a -= 2 * Math.PI;
    while (a < -Math.PI) a += 2 * Math.PI;
    return a;
  }
  function rotAngle (v) {
    try {
      if (v && typeof v.angle === 'number')
        return norm(((v.y !== undefined && v.y < 0) ? -1 : 1) * v.angle);
      if (v && v.toAxisAngle) {
        var aa = v.toAxisAngle();
        return norm((aa[0].y < 0 ? -1 : 1) * aa[1]);
      }
    } catch (e) {}
    return 0;
  }
  var suppress = 0;
  var lastAng = wheelSel ? (wheelPose[wheelSel.value] || 0) : 0;
  if (wheelSel) setWheelPose(lastAng);
  if (gearSel) setShifterPose(gearPose[gearSel.value] || 0);
  var sensor = document.getElementById('wheel-sensor');
  if (sensor) sensor.addEventListener('outputchange', function (e) {
    var d = e.detail; if (!d) return;
    if (d.fieldName === 'rotation_changed') { lastAng = rotAngle(d.value); }
    else if (d.fieldName === 'isActive' &&
             (d.value === false || d.value === 'false')) {
      // commit a beat late: a horn press releases through this
      // same event, and its suppress must land first
      setTimeout(function () {
        if (Date.now() < suppress) return;
        if (lockState().wheel) return;
        if (wheelSel) { wheelSel.value = bandFor(lastAng); readout(); }
      }, 60);
    }
  });
  // sensor output events are the reliable interaction channel:
  // x3dom's synthetic clicks on scene elements die to pointer
  // jitter (a touchpad tap rarely survives its movement
  // threshold), but TouchSensor state is first-class.  Firing on
  // the RELEASE transition is the forgiving semantic: it lands
  // even when the pointer wandered a few pixels mid-press.
  function touch (sensorId, fn) {
    var s = document.getElementById(sensorId);
    if (!s) return;
    s.addEventListener('outputchange', function (e) {
      var d = e.detail; if (!d) return;
      if (d.fieldName === 'isActive' &&
          (d.value === false || d.value === 'false')) fn();
    });
  }
  // the big readout in the title bar: gear letter, steerage band,
  // and the rewind arrows when the tape runs backward
  function readout () {
    var el = document.getElementById('helm-readout');
    if (!el) return;
    var gmap = { ':FORWARD': 'D', ':NEUTRAL': 'N', ':REVERSE': 'R' };
    var wmap = { ':HARD-PORT': 'hard port', ':EASY-PORT': 'easy port',
                 ':AMIDSHIPS': 'amidships', ':EASY-STARBOARD': 'easy stbd',
                 ':HARD-STARBOARD': 'hard stbd' };
    var rw = false;
    var tb = document.querySelectorAll('.transport-btn.lit');
    for (var i = 0; i < tb.length; i++)
      if (tb[i].getAttribute('data-val') === ':REWIND') rw = true;
    el.textContent = (gearSel ? (gmap[gearSel.value] || '·') : '·')
      + ' · ' + (wheelSel ? (wmap[wheelSel.value] || '') : '')
      + (rw ? ' ◀◀' : '');
    var kc = { ':FORWARD': '0.30 0.75 0.35', ':NEUTRAL': '0.85 0.87 0.88',
               ':REVERSE': '0.91 0.60 0.18' };
    var km = document.getElementById('shifter-knob-mat');
    if (km && gearSel)
      km.setAttribute('diffuseColor', kc[gearSel.value] || '0.85 0.87 0.88');
    if (wheelSel) {
      var order = [':HARD-PORT', ':EASY-PORT', ':AMIDSHIPS',
                   ':EASY-STARBOARD', ':HARD-STARBOARD'];
      var idx = order.indexOf(wheelSel.value);
      for (var i = 0; i < 5; i++) {
        var lm = document.getElementById('steer-lamp-mat-' + i);
        if (!lm) continue;
        lm.setAttribute('diffuseColor', i === idx ? '0.91 0.78 0.22' : '0.15 0.16 0.18');
        lm.setAttribute('emissiveColor', i === idx ? '0.55 0.45 0.10' : '0.04 0.04 0.05');
      }
    }
    applyLockouts();
  }
  if (wheelSel) wheelSel.addEventListener('change', readout);
  if (gearSel) gearSel.addEventListener('change', readout);
  if (roadSel) roadSel.addEventListener('change', readout);
  touch('horn-touch', function () {
    if (lockState().wheel) return;
    suppress = Date.now() + 400;
    lastAng = 0;
    setWheelPose(0);
    if (wheelSel) wheelSel.value = ':AMIDSHIPS';
    readout();
  });
  touch('shifter-touch', function () {
    if (!gearSel || lockState().gear) return;
    var next = gearCycle[(gearCycle.indexOf(gearSel.value) + 1) % gearCycle.length];
    gearSel.value = next;
    setShifterPose(gearPose[next]);
    readout();
  });
  var pedalVals = [':FEATHER', ':BRAKE', ':BURN'];
  [0, 1, 2].forEach(function (i) {
    touch('pedal-touch-' + i, function () {
      if (lockState().pedal) return;
      var rig = document.getElementById('pedal-rig-' + i);
      if (rig) {
        rig.setAttribute('translation', '0.025 0 -0.012');
        setTimeout(function () { rig.setAttribute('translation', '0 0 0'); }, 160);
      }
      if (pedalSel) pedalSel.value = pedalVals[i];
    });
  });
  // the dash radio: preset buttons drive the hidden radio inputs,
  // and the station readout names the channel playing
  var stations = { ':SLOW': 'drone 33', ':MEDIUM': 'dub 72',
                   ':FAST': 'house 120', ':FASTEST': 'goa 145' };
  function radioByVal (v) {
    var ins = document.querySelectorAll('#helm-body input');
    for (var i = 0; i < ins.length; i++)
      if (ins[i].value === v) return ins[i];
    return null;
  }
  function wireFace (cls, after) {
    var btns = document.querySelectorAll(cls);
    Array.prototype.forEach.call(btns, function (b) {
      var r = radioByVal(b.getAttribute('data-val'));
      if (!r) return;
      if (r.checked) { b.classList.add('lit'); if (after) after(b); }
      b.addEventListener('click', function () {
        r.checked = true;
        Array.prototype.forEach.call(btns, function (o) { o.classList.remove('lit'); });
        b.classList.add('lit');
        if (after) after(b);
      });
    });
  }
  // the dash radio in the metal mirrors the same values; a click
  // on a 3D key clicks the matching faceplate button, and both
  // relight together
  var cadVals = [':SLOW', ':MEDIUM', ':FAST', ':FASTEST'];
  var tptVals = [':REWIND', ':PLAY'];
  function relight3d (prefix, vals, active) {
    vals.forEach(function (v, i) {
      var m = document.getElementById(prefix + '-mat-' + i);
      if (!m) return;
      var lit = v === active;
      m.setAttribute('diffuseColor', lit ? '0.91 0.78 0.22' : '0.13 0.15 0.17');
      m.setAttribute('emissiveColor', lit ? '0.55 0.45 0.10' : '0.05 0.06 0.07');
    });
  }
  wireFace('.cadence-btn', function (b) {
    var v = b.getAttribute('data-val');
    var s = document.getElementById('radio-station');
    if (s) s.textContent = stations[v] || '';
    relight3d('radio-preset', cadVals, v);
  });
  wireFace('.transport-btn', function (b) {
    relight3d('radio-tpt', tptVals, b.getAttribute('data-val'));
    readout();
  });
  readout();
  function faceClick (cls, val) {
    var btns = document.querySelectorAll(cls);
    for (var i = 0; i < btns.length; i++)
      if (btns[i].getAttribute('data-val') === val) { btns[i].click(); return; }
  }
  cadVals.forEach(function (v, i) {
    touch('radio-preset-' + i + '-touch',
          function () { faceClick('.cadence-btn', v); });
  });
  tptVals.forEach(function (v, i) {
    touch('radio-tpt-' + i + '-touch',
          function () { faceClick('.transport-btn', v); });
  });
  // the starter posts the move -- the whole turn drives from
  // inside the scene
  touch('starter-touch', function () {
    var st = document.getElementById('starter-mat');
    if (st) st.setAttribute('emissiveColor', '0.75 0.25 0.12');
    var b = document.getElementById('gw-move-btn'); if (b) b.click();
  });
  // what is grabbable says so, straight from the sensors' own
  // isOver: an arrow everywhere, the grab hand over wheel,
  // shifter, pedals and horn, the plain pointer over the keys
  function setCur (c) {
    var x = document.querySelector('x3d'); if (!x) return;
    x.style.cursor = c;
    var cv = x.querySelector('canvas'); if (cv) cv.style.cursor = c;
  }
  window.addEventListener('load', function () {
    setTimeout(function () { setCur('default'); }, 600);
  });
  function overCursor (sensorId, cur) {
    var s = document.getElementById(sensorId);
    if (!s) return;
    s.addEventListener('outputchange', function (e) {
      var d = e.detail; if (!d || d.fieldName !== 'isOver') return;
      setCur((d.value === true || d.value === 'true') ? cur : 'default');
    });
  }
  ['wheel-sensor', 'shifter-touch', 'pedal-touch-0', 'pedal-touch-1',
   'pedal-touch-2', 'horn-touch'].forEach(function (id) {
    overCursor(id, 'grab');
  });
  ['radio-preset-0-touch', 'radio-preset-1-touch', 'radio-preset-2-touch',
   'radio-preset-3-touch', 'radio-tpt-0-touch', 'radio-tpt-1-touch',
   'starter-touch'].forEach(function (id) {
    overCursor(id, 'pointer');
  });
};")

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
               (helm-rigs-x3d cab)
               (wood-panel-x3d)
               (star-dome-x3d))))))

;; The page: the view from the driver's seat, the galaxy out past
;; where the glass will go.
;; The paint shop, now a standing department: the worlds' faces,
;; the wood and the dice painted client-side onto canvases.  Every
;; painted url is also stashed on window.GW_TEX, so a renderer
;; whose scene cannot be reached by DOM id (X_ITE's src-loaded
;; document) can take the same faces through the SAI.
(defparameter *paint-shop-js*
  "(function () {
  function rnd (n) { var x = Math.sin(n * 12.9898 + 78.233) * 43758.5453; return x - Math.floor(x); }
  function tex (ids, w, h, draw) {
    var c = document.createElement('canvas'); c.width = w; c.height = h;
    draw(c.getContext('2d'), w, h);
    var u = c.toDataURL();
    window.GW_TEX = window.GW_TEX || {};
    ids.forEach(function (id) { window.GW_TEX[id] = u; });
    ids.forEach(function (id) {
      var el = document.getElementById(id);
      if (el) el.setAttribute('url', u);
    });
  }
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
")

(define-object cockpit-view (session-control-mixin base-html-page)

  :input-slots
  ((title "Galaxy World — the cockpit (x3dom build)")
   ;; t on the X_ITE scout: the scene renders as a standalone X3D
   ;; document (self-igniting voyage clock, no x3dom extensions)
   (xr-scene? nil)
   ;; the console tap: scene and script complaints land on the
   ;; plot's frame label, where a headless webshot can read them
   (additional-header-content
    "<script>(function () { function diag (s) { try { if (!window.GW_DIAG_ON) return; var d = document.getElementById('gw-diag'); if (!d) { d = document.createElement('div'); d.id = 'gw-diag'; d.style.cssText = 'position:fixed;top:60px;left:14px;z-index:99;color:#ff9090;font:11px monospace;background:rgba(0,0,0,0.75);padding:4px;max-width:1100px;'; document.body.appendChild(d); } d.textContent = (d.textContent + ' || ' + s).slice(-500); } catch (e) {} } window.GW_DIAG = diag; window.GW_DIAG_ON = (location.hash + location.search).indexOf('gwauto') > -1 || (location.hash + location.search).indexOf('gwdiag') > -1; var orig = console.error; console.error = function () { try { diag('ERR ' + Array.prototype.join.call(arguments, ' ')); } catch (e) {} orig.apply(console, arguments); }; window.addEventListener('error', function (ev) { try { diag('THROW ' + ev.message + ' @' + (ev.filename || '').split('/').pop() + ':' + ev.lineno); } catch (e) {} }); var oOpen = XMLHttpRequest.prototype.open, oSend = XMLHttpRequest.prototype.send; XMLHttpRequest.prototype.open = function (m, u) { this._gwu = m + ' ' + u; return oOpen.apply(this, arguments); }; XMLHttpRequest.prototype.send = function () { var x = this; if ((x._gwu || '').indexOf('gdlAjax') > -1) { x.addEventListener('loadend', function () { var len = -1; try { if (!x.responseType || x.responseType === 'text') len = (x.responseText || '').length; } catch (e) {} diag('AJAX ' + x._gwu + ' -> ' + x.status + ' len ' + len); }); } return oSend.apply(this, arguments); }; })();</script>")
   (use-ajax? t)
   (use-svgpanzoom? nil)
   (use-tailwind? nil)
   ;; the GALAXY on the cockpit's tab (swapped by ruling,
   ;; 2026-09-01 -- the cockpit looks OUT at the galaxy; the
   ;; bridge wears the ship's own two-eyed face): a tiny tilted
   ;; milky way, bright core, dim arms, field stars.
   (favicon-type "image/svg+xml")
   (favicon-path "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'%3E%3Ccircle cx='16' cy='16' r='15.5' fill='%23000008'/%3E%3Cg transform='rotate(-28 16 16)'%3E%3Cellipse cx='16' cy='16' rx='12.5' ry='4.6' fill='%23232c48'/%3E%3Cellipse cx='16' cy='16' rx='9' ry='3.1' fill='%233c4a74'/%3E%3Cellipse cx='16' cy='16' rx='5.5' ry='1.9' fill='%238ea0cf'/%3E%3Cellipse cx='16' cy='16' rx='2.6' ry='1.1' fill='%23e8ecf8'/%3E%3Ccircle cx='16' cy='16' r='1.1' fill='%23fff6d8'/%3E%3C/g%3E%3Ccircle cx='7' cy='8' r='0.55' fill='%23cfd8ee'/%3E%3Ccircle cx='25.5' cy='6.5' r='0.5' fill='%23cfd8ee'/%3E%3Ccircle cx='26.5' cy='25' r='0.55' fill='%23cfd8ee'/%3E%3Ccircle cx='6' cy='24.5' r='0.45' fill='%23cfd8ee'/%3E%3C/svg%3E")

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
   ;; THE SHIP'S CLOCK, in game-seconds: time is frozen between
   ;; moves, and each move adds the seconds it spent (advance-clock!
   ;; -- the cadence for a hand-flown turn, the road's own time of
   ;; flight for a programmed one, the coast to the departure point
   ;; where a road demands one).  world-spin-rad reads it against
   ;; the world's day for the parked body's pole angle -- the
   ;; day/night terminator the sun casts then sits still until the
   ;; pilot moves, and the face he sees lights and darkens as he
   ;; goes AROUND the orbit, not as the world whirls in place.
   (game-seconds 0 :settable)
   (world-spin-rad (world-phase-rad (the world) (the game-seconds)))
   ;; the clock as the helm reads it
   (clock-string (clock-face (the game-seconds)))
   (vel-x 0 :settable)
   (vel-y +ring-speed+ :settable)
   (pos-x +ring-radius+ :settable)
   (pos-y 0 :settable)
   (moves-count 0 :settable)
   ;; the scope of a hand-flown turn, seconds: a minute of close
   ;; work up to a full day on the fastest channel
   (cadence-seconds (ecase (the cadence-control value)
                      (:slow 60) (:medium 600)
                      (:fast 3600) (:fastest 86400)))
   ;; the pilot this cockpit's hands belong to, once the browser
   ;; signs the log book at boarding (check-in!).  NIL until then --
   ;; an unsigned hand flies fine; only the book ignores him.
   (pilot-id nil :settable)
   ;; down on the world's surface, engines cold.  Landing sets it,
   ;; the climb back to the ring (gas while parked) or a programmed
   ;; road clears it.
   (landed? nil :settable)
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
   (world-day-seconds (day-seconds-of (the world)))
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

   ;; where periapsis points: the eccentricity vector's angle, so
   ;; the plot can lay the road's true shape on the table
   (peri-angle (let* ((x (the pos-x)) (y (the pos-y))
                      (vx (the vel-x)) (vy (the vel-y))
                      (mu (the world-mu)) (r (the radius))
                      (v2 (+ (* vx vx) (* vy vy)))
                      (rv (+ (* x vx) (* y vy)))
                      (ex (/ (- (* (- v2 (/ mu r)) x) (* rv vx)) mu))
                      (ey (/ (- (* (- v2 (/ mu r)) y) (* rv vy)) mu))
                      (em (sqrt (+ (* ex ex) (* ey ey)))))
                 (if (< em 1e-6) 0.0 (atan ey ex))))

   ;; the plot's data, one JSON object per page: the orbit block is
   ;; the ship's state in the world's frame (Cartesian km, origin at
   ;; the world's center -- the truth the game integrates); a voyage
   ;; page adds the sampled road and every riding body's track in
   ;; the frame the road was cut in (home's for the moon road, the
   ;; sun's for the outbound legs).
   (plan-json
    (with-output-to-string (out)
      (format out "{\"orbit\":{\"frame\":~s,\"worldR\":~,1f,\"skyR\":~,1f,\"ringR\":~,1f,\"x\":~,1f,\"y\":~,1f,\"vx\":~,4f,\"vy\":~,4f,\"alt\":~,1f,\"closeAlt\":~,1f,\"capKps\":~,2f,\"heading\":~,2f,\"landed\":~a,\"color\":~s"
              (the world-name) (the world-radius) (the world-sky)
              (the world-ring) (the pos-x) (the pos-y)
              (the vel-x) (the vel-y) (the altitude)
              (* 0.5 (the world-radius)) +landing-speed-cap+
              (the heading-deg)
              (if (the landed?) "true" "false")
              (world-figure (the world) :diffuse))
      (if (and (the semi-major) (the eccentricity))
          (format out ",\"a\":~,1f,\"e\":~,5f,\"peri\":~,5f"
                  (the semi-major) (the eccentricity) (the peri-angle))
          (format out ",\"a\":null"))
      (when (eql (the world) :home)
        (format out ",\"moon\":{\"x\":~,1f,\"y\":0,\"r\":~,1f}"
                +moon-x+ +moon-radius+))
      (format out "}")
      (when (the transit-samples)
        (format out ",\"voyage\":{\"frame\":~s,\"cycle\":~d,\"samples\":["
                (case (the transit-target)
                  ((:moon :homeward) "home")
                  ((:down :up) (the world-name))
                  (t "the sun's"))
                (the transit-duration))
        (loop for s in (the transit-samples)
              for first = t then nil
              do (destructuring-bind (k h px py) s
                   (format out "~:[,~;~][~,4f,~,4f,~,1f,~,1f]"
                           first k h px py)))
        (format out "],\"bodies\":[")
        (loop for b in (the transit-bodies)
              for first = t then nil
              do (format out "~:[,~;~]{\"name\":~s,\"r\":~,1f,\"targets\":["
                         first (getf b :prefix) (getf b :radius))
                 (loop for tg in (getf b :targets)
                       for f2 = t then nil
                       do (format out "~:[,~;~][~,1f,~,1f]"
                                  f2 (first tg) (second tg)))
                 (format out "]}"))
        (format out "],\"beats\":[")
        (loop for beat in (the transit-beats)
              for first = t then nil
              do (format out "~:[,~;~][~,3f,~s]"
                         first (car beat) (cdr beat)))
        (format out "]")
        ;; the descent carries a LIVE readout: the conic's own
        ;; figures, so the plot can write altitude and vis-viva
        ;; speed onto the helm as the clip flies -- the falling
        ;; ship visibly gaining the speed the big brake must shed.
        ;; Ground datum is the sky: 0 km up at the touch.
        (case (the transit-target)
          (:down
           (destructuring-bind (key h px py) (first (the transit-samples))
             (declare (ignore key h))
             (let* ((r1 (sqrt (+ (* px px) (* py py))))
                    (a (* 0.5 (+ r1 (the world-sky)))))
               (format out ",\"live\":{\"mu\":~,4f,\"a\":~,1f,\"ground\":~,1f}"
                       (the world-mu) a (the world-sky)))))
          ;; the climb is powered, so vis-viva has nothing to say
          ;; -- altitude alone rides the live line
          (:up (format out ",\"live\":{\"ground\":~,1f}" (the world-sky))))
        (format out "}"))
      (format out "}")))

   ;; the coordinates line the helm carries at all times; the plot
   ;; script rewrites it live while a voyage flies
   (coords-line-html
    (format nil "x ~:d km &middot; y ~:d km &mdash; ~a frame"
            (round (the pos-x)) (round (the pos-y)) (the world-name)))

   ;; the sampled road of a just-flown programmed voyage, or nil.
   ;; Set by the road functions, cleared by the next hand-flown
   ;; move; while set, the page carries the voyage animation.
   (transit-samples nil :settable)

   ;; which road was flown -- :moon, :homeward (the moon road flown
   ;; back), :down (the yard's descent), or a world key on the
   ;; sun's road -- while samples stand
   (transit-target nil :settable)

   ;; how long the scene takes to fly it, seconds: the moon legs at
   ;; 90, the descent a tighter 45, the lift-off 30, the sun's
   ;; roads at 120
   (transit-duration (case (the transit-target)
                       ((:moon :homeward) 90)
                       ((:down) 45)
                       ((:up) 30)
                       (t 120)))

   ;; the bodies riding the voyage page: specs for body-x3d and
   ;; transit-anim-x3d, set by the road function alongside the
   ;; samples so page and animation author from one list.  Each is a
   ;; plist: :prefix :texture :radius :targets :diffuse :emissive
   ;; :spin-phase :spin-rate (the face he leaves with, and how fast
   ;; his day turns it) :scale-override.
   (transit-bodies nil :settable)

   ;; the running story of a programmed road: (fraction . line)
   ;; pairs the page speaks into the move-note while the voyage
   ;; clock flies, the full arrival note returning at the end
   (transit-beats nil :settable)

   ;; the clip's clock: (key . game-seconds) pairs along the road,
   ;; the game-time elapsed at each key -- what turns the riding
   ;; worlds' faces (transit-anim-x3d); set beside the samples
   (transit-time-map nil :settable)

   ;; the sky's authored heading: mid-voyage pages author the scene
   ;; at the road's start and let the clock fly it forward
   (sky-authored-heading-rad (if (the transit-samples)
                                 (second (first (the transit-samples)))
                                 (deg->rad (the heading-deg))))

   ;; the sun's height: in the plane aloft, climbed on the ground
   ;; (a grazing sun leaves the plain black) -- see sun-light-x3d
   (sun-elevation (if (the landed?) 0.6 0.0))

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
                                    :phase (getf body :spin-phase)
                                    :diffuse (getf body :diffuse)
                                    :emissive (getf body :emissive)
                                    :scale-override (getf body :scale-override)
                                    :tilt (getf body :tilt)
                                    :adornment (getf body :adornment)))))))
             (transit-anim-x3d samples (the transit-bodies)
                               :duration (the transit-duration)
                               :time-map (the transit-time-map)
                               ;; a scene document carries its own
                               ;; ignition; the x3dom page lights the
                               ;; clock from script instead
                               :start-time (when (the xr-scene?)
                                             (+ (unix-now) 2)))

             ;; the ground rides the clip's ends: the yard's
             ;; descent finishes ON the plain (it rises under the
             ;; cab over the last tenth, taking the view as the
             ;; swelling globe swallows the camera -- the ground
             ;; literally comes up to meet her), and the lift-off
             ;; STARTS on it (the plain falls away over the first
             ;; sixth while the globe emerges below).
             (if (member (the transit-target) '(:down :up))
                 (let ((down? (eql (the transit-target) :down)))
                   (format nil "<Transform DEF=\"ground-lift\" id=\"ground-lift\" translation=\"0 0 ~a\">~a</Transform><PositionInterpolator DEF=\"vy-ground-lift\" key=\"~a\" keyValue=\"~a\"></PositionInterpolator><ROUTE fromNode=\"voyage-clock\" fromField=\"fraction_changed\" toNode=\"vy-ground-lift\" toField=\"set_fraction\"></ROUTE><ROUTE fromNode=\"vy-ground-lift\" fromField=\"value_changed\" toNode=\"ground-lift\" toField=\"set_translation\"></ROUTE>"
                           (if down? "-6000" "0")
                           (ground-x3d (the world))
                           (if down? "0 0.88 0.97 1" "0 0.05 0.16 1")
                           (if down?
                               "0 0 -6000 0 0 -1800 0 0 -60 0 0 0"
                               "0 0 0 0 0 -80 0 0 -6000 0 0 -6000")))
                 "")))
          (string-append
           ;; parked, the world is the GROUND, not a globe: at
           ;; touchdown the subtended sphere swallowed the camera
           ;; (backface-culled from inside), so the landed page
           ;; stands on a plain instead, stars above, the sky's
           ;; other riders hanging at the horizon
           (if (the landed?)
               (ground-x3d (the world))
               (body-x3d "planet" (world-figure (the world) :texture)
                         (the planet-bearing) (the radius) (the world-radius)
                         :phase (the world-spin-rad)
                         ;; city lights: earth's night side, X_ITE only.
                         ;; the sun is baked into the shader in the
                         ;; sphere's own frame -- the parked scene is
                         ;; static between moves, so a constant serves.
                         :night-url (when (and (the xr-scene?) (eql (the world) :home))
                                      "/gw-tex/earth-night.jpg")
                         :sun-local (when (and (the xr-scene?) (eql (the world) :home))
                                      (earth-sun-local (the sky-authored-heading-rad)
                                                       (the world-spin-rad)))
                         :diffuse (world-figure (the world) :diffuse)
                         :emissive (world-figure (the world) :emissive)
                         :tilt (world-figure (the world) :tilt)
                         :adornment (world-figure (the world) :adornment)))
           (cond ((eql (the world) :home)
(body-x3d "moon" "moon-tex"
                            (the moon-bearing) (the moon-distance) +moon-radius+
                            :phase (world-phase-rad :moon (the game-seconds))
                            :diffuse "0.75 0.74 0.70" :emissive "0.04 0.04 0.03"))
                 ;; falling around the moon, home hangs in his sky:
                 ;; the whole blue marble at his true bearing
                 ((eql (the world) :moon)
                  (body-x3d "home-far" "earth-tex"
                            (bearing-to (the pos-x) (the pos-y)
                                        (deg->rad (the heading-deg))
                                        (- +moon-x+) 0)
                            (dist-to (the pos-x) (the pos-y) (- +moon-x+) 0)
                            +planet-radius+
                            :phase (world-phase-rad :home (the game-seconds))
                            :diffuse "0.10 0.18 0.85"
                            :emissive "0.02 0.03 0.05"))
                 (t ""))))))

   ;; the voyage's page script: first load arms the one-shot clock;
   ;; a reload of the same arrival fast-forwards the scene to the
   ;; road's end instead of replaying five days
   ;; the ARRIVED pose of every voyage body, for the fast-forward
   ;; path below.  The road's closing sample was authored to mirror
   ;; the arrived state, so each body's fin is simply its final
   ;; animation frame: the new world dead ahead at ring distance,
   ;; everything left behind shrunk to au-scale nothing.
   (voyage-fin-js
    (if (the transit-samples)
        (with-output-to-string (out)
          (destructuring-bind (key h px py)
              (car (last (the transit-samples)))
            (declare (ignore key))
            (format out "[")
            (loop for body in (the transit-bodies)
                  for first = t then nil
                  do (destructuring-bind (tx ty)
                         (car (last (getf body :targets)))
                       (multiple-value-bind (sx sy sc)
                           (scene-body-frame (bearing-to px py h tx ty)
                                             (dist-to px py tx ty)
                                             (getf body :radius))
                         (format out "~:[,~%           ~;~]['~a-frame', '~,1f ~,1f 0', '~,2f ~,2f ~,2f'~a]"
                                 first (getf body :prefix) sx sy sc sc sc
                                 ;; the face he lands with: the clock's
                                 ;; end read against his day
                                 (let ((ph (getf body :spin-phase))
                                       (rate (getf body :spin-rate))
                                       (secs (let ((tm (the transit-time-map)))
                                               (if tm (cdr (car (last tm))) 0))))
                                   (if (and ph rate)
                                       (format nil ", '0 -1 0 ~,5f'"
                                               (mod (+ ph (* rate secs)) (* 2 pi)))
                                       ""))))))
            (format out "]")))
        "[]"))

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
    window.GW_VOYAGE_T0 = null;
    var fin = ~a;
    fin.forEach(function (f) {
      var el = document.getElementById(f[0]);
      if (el) { el.setAttribute('translation', f[1]); el.setAttribute('scale', f[2]); }
      if (f[3]) { var sp = document.getElementById(f[0].replace('-frame', '-spin')); if (sp) sp.setAttribute('rotation', f[3]); }
    });
    var gl = document.getElementById('ground-lift');
    if (gl) gl.setAttribute('translation', '~a');
    var sky = document.getElementById('sky-heading');
    if (sky) sky.setAttribute('rotation', '0 0 1 ~,5f');
  } else {
    try { sessionStorage.setItem(key, '1'); } catch (e) {}
    window.addEventListener('load', function () {
      var ts = document.getElementById('voyage-clock');
      if (ts) ts.setAttribute('startTime', '' + (Date.now() / 1000 + 1));
      window.GW_VOYAGE_T0 = Date.now() + 1000;
    });
  }
})();"
                (or (the instance-id) "local") (the moves-count)
                (the voyage-fin-js)
                ;; where the ground stands when the clip is snapped
                ;; past: home for a landing, struck for a lift-off
                (if (eql (the transit-target) :up) "0 0 -6000" "0 0 0")
                (- (deg->rad (the heading-deg))))
        ""))
   ;; the speedo sweeps 8 o'clock to 4 o'clock; full scale reads the
   ;; world -- 8 km/s over home and Mars, opened up over a giant
   ;; whose ring alone runs past twenty (a proper instrument panel
   ;; recalibrates for the roads it serves)
   (speedo-full-scale (max 8.0 (* 1.5 (the world-ring-speed))))
   (speedo-phi (+ -120 (* 240 (min 1 (/ (the speed)
                                        (the speedo-full-scale))))))

   ;; the climb gauge (the starboard dial): radial speed, signed --
   ;; level rides 9 o'clock, climb swings toward noon, descent
   ;; toward the floor.  The landing instrument: hold the fall
   ;; gentle and the surface forgives the rest.
   (radial-speed (if (or (the landed?) (< (the radius) 1))
                     0
                     (/ (+ (* (the pos-x) (the vel-x))
                           (* (the pos-y) (the vel-y)))
                        (the radius))))
   (vario-phi (+ -90 (* 60 (min 1 (max -1 (/ (the radial-speed) 1.5))))))

   ;; close aboard: under half a world-radius of altitude the plot
   ;; grows and gains its landing inset, and the port flatscreen
   ;; becomes the landing eye
   (plot-close? (and (not (the landed?))
                     (< (the altitude) (* 0.5 (the world-radius)))))
   (plot-size (if (the plot-close?) 340 210))

   ;; the port flatscreen: the port reptile eye on the open road;
   ;; on final -- and on the ground -- it gazes at the world
   ;; instead, the landing eye watching the surface come up
   (port-feed-x3d (if (or (the plot-close?) (the landed?))
                     (eye-feed-x3d "0 0 0.6"
                                   (look-at-orientation
                                    (make-vector (cos (the planet-bearing))
                                                 (sin (the planet-bearing))
                                                 0)
                                    (make-vector 0 0 1))
                                   0.46 0.22)
                     (port-eye-feed-x3d)))

   ;; the big readout in the helm's title bar: gear letter and
   ;; steerage at a glance, visible even with the card folded.
   ;; The five wheel bands are finite in-place orientation calls
   ;; (the nose swings before the burn, no propellant charged),
   ;; and the driver should never need a popup to know which one
   ;; stands.
   ;; the per-turn state script: rides at the tail of the helm
   ;; section, so the stock section swap re-runs it after every
   ;; move (and once at load, where the guards no-op until the
   ;; library definitions at the body's tail have run).  Scene
   ;; refresh fires only on swaps.
   ;; the one source for the scene's children: the live x3d
   ;; renders it at load, the template section carries it per move
   (scene-contents-x3d
    (with-lhtml-string ()
            (:|Background| :|skyColor| "0 0 0.012")
            (str (the viewpoints-x3d))
            ;; the universe turns around the ship, never the ship
            ;; around the universe -- and nothing in it moves between
            ;; moves: the scene stands until the pilot acts
            (:|NavigationInfo| :headlight "false")
            (:|Transform| :|DEF| "sky-heading" :|id| "sky-heading"
              :rotation (format nil "0 0 1 ~,5f"
                                (- (the sky-authored-heading-rad)))
              (str (sun-light-x3d :elevation (the sun-elevation)))
              (:|Transform| :|DEF| "sky-drift"
                (str (starfield-x3d :radius 5000.0d0))))
            (str (the bodies-x3d))
            ;; the cab under its own lamp (see cab-light-x3d)
            (:|Group|
              (str (cab-light-x3d))
              (str (cockpit-x3d))
              (str (dice-x3d (the dice-lean)))
              (str (gauge-needle-x3d 0 0.46 (the speedo-phi) 0.055))
              (str (gauge-needle-x3d 0.19 0.45 (the heading-deg) 0.042))
              (str (gauge-needle-x3d -0.19 0.45 (the vario-phi) 0.042))
              (str (dash-radio-x3d (the cadence-control value)
                                   (the transport-control value)))
              (str (the port-feed-x3d))
              (str (starboard-eye-feed-x3d)))))

   (section-state-js
    (format nil "setTimeout(function () {~%window.GW_PLAN = ~a;~%window.GW_VOYAGE_T0 = ~a;~%try { if (sessionStorage.getItem('gw-helm-collapsed') === '1') { document.getElementById('helm-body').style.display = 'none'; document.getElementById('helm-caret').textContent = '\\u25b8'; } } catch (e) {}~%try { if (sessionStorage.getItem('gw-plot-collapsed') === '1') { document.getElementById('plot-body').style.display = 'none'; document.getElementById('plot-caret').textContent = '\\u25b8'; } } catch (e) {}~%if (window.GW_WIRE) GW_WIRE();~%if (window.GW_DRAW) GW_DRAW();~%if (window.GW_BEATS) GW_BEATS();~%if (window.GW_FLOOD_APPLY) GW_FLOOD_APPLY();~%if (window.GW_LOADED && window.GW_GEN !== '~a') { ~a }~%window.GW_GEN = '~a';~%window.GW_LOADED = true;~%}, 60);"
            (the plan-json)
            (if (the transit-samples) "Date.now() + 2000" "null")
            ;; the scene refresh is gated on the scene GENERATION
            ;; (moves-count, the gw-gen-N stamp every move path
            ;; bumps), not merely on the page being loaded: the
            ;; boarding check-in returns this section too, and
            ;; re-pointing the canvas at the generation it is still
            ;; fetching aborted the first load in production (X_ITE
            ;; AbortError, 2026-09-02) -- and reloaded the x3dom page
            ;; at every first boarding.
            (the moves-count)
            (the scene-refresh-js)
            (the moves-count)))

   ;; how this renderer takes the new scene after a move: the
   ;; scene-section was just swapped in place, so re-init x3dom
   ;; and arm the voyage clock the way the load path does
(scene-refresh-js
    ;; x3dom's GL disposal crashes on every scene-teardown path this
    ;; build offers (x3dom.reload, live transplant, with and without
    ;; the RenderedTexture feeds) -- benchmark datum and an upstream
    ;; candidate.  Until then the x3dom page takes the new scene the
    ;; classic way, while the card machinery and the X_ITE page keep
    ;; the true no-reload turn.
    "location.reload();")

   (helm-readout-html
    (format nil "~a · ~a~a"
            (case (the shifter-control value)
              (:forward "D") (:reverse "R") (otherwise "N"))
            (case (the wheel-control value)
              (:hard-port "hard port") (:easy-port "easy port")
              (:easy-starboard "easy stbd") (:hard-starboard "hard stbd")
              (otherwise "amidships"))
            (if (eql (the transport-control value) :rewind) " ◀◀" "")))

   (helm-form-html
    (with-form-string ()
      (:div :style "display:flex;flex-direction:column;gap:4px;"
        (:div (str (the voyage-control html-string)))
        (:div (str (the wheel-control html-string)))
        (:div (str (the shifter-control html-string)))
        (:div (str (the pedal-control html-string)))
        ;; the dash radio: cadence presets and the tape transport.
        ;; The real radio inputs ride hidden below; these buttons
        ;; drive them, the station readout names the channel.
        (:div :id "radio-face"
          :style "margin-top:4px;border:1px solid #7a6a1f;border-radius:8px;padding:6px 8px;background:rgba(10,10,10,0.72);"
          (:div :style "display:flex;justify-content:space-between;font-size:10px;letter-spacing:0.14em;color:#c9a227;margin-bottom:4px;"
            (:span "CADENCE")
            (:span :id "radio-station" :style "font-variant-numeric:tabular-nums;" ""))
          (:div :style "display:flex;gap:4px;align-items:center;"
            (:button :type "button" :class "rbtn cadence-btn" :data-val ":SLOW"
              :title "slow — a minute a turn (ambient drone)" "slow")
            (:button :type "button" :class "rbtn cadence-btn" :data-val ":MEDIUM"
              :title "medium — ten minutes a turn (dub)" "med")
            (:button :type "button" :class "rbtn cadence-btn" :data-val ":FAST"
              :title "fast — an hour a turn (house)" "fast")
            (:button :type "button" :class "rbtn cadence-btn" :data-val ":FASTEST"
              :title "fastest — a day a turn (145 bpm goa)" "fstst")
            (:span :style "flex:1;")
            (:button :type "button" :class "rbtn transport-btn" :data-val ":REWIND"
              :title "rewind — the turn falls into the past; the engines don't light" "◀◀")
            (:button :type "button" :class "rbtn transport-btn" :data-val ":PLAY"
              :title "play — the turn runs forward" "▶"))
          (:div :style "display:none;"
            (str (the cadence-control html-string))
            (str (the transport-control html-string))
            (:span :id "gw-signature"
              (str (the pilot-control html-string))))))
      ;; the floodlight: the sun lights the day side only (see
      ;; sun-light-x3d); this lantern lights the night side on
      ;; request, remembered per browser
      (:div :style "display:flex;gap:6px;align-items:center;margin-top:6px;"
        (:button :type "button" :id "gw-flood-btn" :class "rbtn"
          :onclick "GW_FLOOD_TOGGLE()"
          :title "floodlight — light the night side of the world; off, the sun alone lights her"
          "floodlight")
        (:span :style "font-size:10px;color:#8a7a2f;letter-spacing:0.08em;" "the sun lights the day side"))
      (:script (str *flood-js*))
      ;; the move posts through the stock gdlAjax pipe: the six
      ;; controls bash, after-set! runs, and the page's sections
      ;; re-render in place -- no reload, no splash, no re-init
      (:button :type "button" :id "gw-move-btn"
       :onclick (the (gdl-ajax-call
                      :form-controls (list (the voyage-control)
                                           (the wheel-control)
                                           (the shifter-control)
                                           (the pedal-control)
                                           (the cadence-control)
                                           (the transport-control))
                      :function-key :after-set!))
       :style "margin-top:8px;background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:4px 12px;font-size:12px;cursor:pointer;"
       "make the move")
      ;; the boarding signature: the browser keeps a pilot token of
      ;; its own minting (localStorage; a private window simply
      ;; boards unsigned), writes it on the hidden line, and posts
      ;; it once through the pipe -- the log book turns to the
      ;; pilot's page and every tally! of this session lands there.
      ;; Guarded both ends: the flag here, the pilot-id slot aboard.
      (:script
       (str (format nil "window.GW_CHECK_IN = function () {
  try {
    if (window.GW_CHECKED_IN) return;
    var pid = null;
    try { pid = localStorage.getItem('gw-pilot'); } catch (e) {}
    if (!pid) {
      var a = new Uint8Array(12);
      if (window.crypto && crypto.getRandomValues) crypto.getRandomValues(a);
      else for (var j = 0; j < a.length; j++) a[j] = Math.floor(Math.random() * 256);
      pid = 'p';
      for (var i = 0; i < a.length; i++) pid += (a[i] < 16 ? '0' : '') + a[i].toString(16);
      try { localStorage.setItem('gw-pilot', pid); } catch (e) {}
    }
    var f = document.querySelector('#gw-signature input');
    if (!f) return;
    f.value = pid;
    window.GW_CHECKED_IN = true;
    ~a
  } catch (e) {}
};
GW_CHECK_IN();"
                    (the (gdl-ajax-call
                          :form-controls (list (the pilot-control))
                          :function-key :check-in!)))))))

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
                                  ;; lifted from -0.24: the wheel is
                                  ;; smaller and lower now, so the
                                  ;; glass gets the vertical back.
                                  ;; The wider glass takes in the
                                  ;; hanging pedals at the frame's
                                  ;; foot.
                                  (make-vector 0.99 0 -0.15)
                                  "1.25" :z-near "0.05" :z-far "8000" :up up))
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
          :style "width:100vw;height:100vh;display:block;"
          (:|Scene|
            ;; the dynamic group is what a move transplants; the
            ;; eye feeds stand OUTSIDE it, because disposing a
            ;; RenderedTexture detonates this x3dom build's GL
            ;; teardown (deleteFramebuffer on the render loop)
            (:|Group| :id "gw-dynamic"
              (str (the scene-contents-x3d)))
            (str (the port-feed-x3d))
            (str (starboard-eye-feed-x3d)))))
      (:div :style "display:none;"
        (str (the scene-section main-div)))
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
#helm-body select option { background:#1a1a1a; color:#e8c839; }
#helm-body .rbtn { background:#141414; color:#e8c839; border:1px solid #7a6a1f; border-radius:4px; padding:2px 7px; font-size:11px; cursor:pointer; font-family:inherit; }
#helm-body .rbtn.lit { background:#e8c839; color:#141414; border-color:#e8c839; }"))
      (:div :style "position:fixed;bottom:14px;right:14px;z-index:10;background:rgba(16,16,16,0.45);border:1px solid #e8c839;border-radius:10px;padding:10px 16px;font-family:sans-serif;color:#e8c839;font-size:13px;min-width:250px;max-width:430px;"
        (str (the helm-section main-div)))
      ;; THE PLOT: the plan view every real ship keeps beside the
      ;; glass -- the world's frame from above, the road's true
      ;; shape, the ship a point with her nose drawn on.  A voyage
      ;; page flies the dot along the sampled road in step with the
      ;; scene's own clock.
      (:div :id "plot-card" :style "position:fixed;bottom:40px;left:14px;z-index:10;background:rgba(16,16,16,0.45);border:1px solid #e8c839;border-radius:10px;padding:8px 10px;font-family:sans-serif;color:#e8c839;"
        (:div :style "font-size:12px;letter-spacing:0.06em;cursor:pointer;display:flex;justify-content:space-between;align-items:center;gap:10px;"
              :onclick "togglePlot()"
          (:span "THE PLOT")
          (:span :id "plot-caret" "▾"))
        (str (the plot-section main-div)))
      (:div :style "position:fixed;bottom:12px;left:14px;z-index:10;color:#c9a227;font-family:sans-serif;font-size:13px;opacity:0.85;"
        "Galaxy World — the cockpit (x3dom build)"
        (:a :href "/" :style "color:#e8c839;margin-left:10px;" "back to the main cockpit"))
      ;; the paint shop: the world's face, the leather, and the wood
      ;; are all painted onto canvases here and handed to the scene's
      ;; ImageTextures as data URLs.  This inline script runs before
      ;; x3dom's load-time init, so the textures are in place when
      ;; the scene first builds.  Deterministic hash noise -- the
      ;; same grain on every visit.
      (:script (str "
function bindEye (id) {
  var vp = document.getElementById(id);
  if (vp) vp.setAttribute('set_bind', 'true');
  // re-binding the already-bound eye is a no-op in x3dom, so a
  // second press (or a press after wandering the camera by hand)
  // resets the view onto it explicitly
  setTimeout(function () {
    var x = document.querySelector('x3d');
    if (x && x.runtime && x.runtime.resetView) x.runtime.resetView();
  }, 80);
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
")
        (str *paint-shop-js*)
        (str "
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
        (str *plan-view-js*)
        (str *helm-hands-js*)
        (str *voyage-beats-js*)
        ;; the definitions just landed; give the section's state
        ;; script its first real run
        (str "if (window.GW_WIRE) GW_WIRE(); if (window.GW_DRAW) GW_DRAW(); if (window.GW_BEATS) GW_BEATS();")
        (str (the voyage-script-js)))))

   (eye-button-style
    "background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:6px 14px;font-size:13px;cursor:pointer;"))

  :objects
  (;; size 1 = popup menus, not open list boxes
   ;; the road: a fresh session comes with the trip to the moon
   ;; already programmed, so the FIRST move flies it -- then the
   ;; helm is yours, and the astrodynamics lessons start from the
   ;; moon's road
   ;; the roads on offer read the world: the moon road from home,
   ;; the sun's road to every world further out than the one that
   ;; has her -- and from anywhere that is not home, the programmed
   ;; road home: the moon road flown back, or the sun's own road
   ;; back down the well.
   (voyage-control :type 'gwl:menu-form-control
                   :prompt "the road: "
                   :size 1
                   :default :moon
                   :choice-plist
                   (append
                    (list :hand "fly her by hand")
                    ;; the yard flies her down on request -- the
                    ;; staging road for the view from the ground;
                    ;; the by-hand landing stays the game
                    (unless (the landed?)
                      (list :down "the programmed landing"))
                    (when (eql (the world) :home)
                      (list :moon "the programmed road to the moon"))
                    (unless (eql (the world) :home)
                      (list :home "the programmed road home"))
                    ;; the sun's roads run between the worlds on his
                    ;; own road (rows with no :sun-radius offer none)
                    (let ((here (world-figure (the world) :sun-radius)))
                      (when here
                        (loop for row in *worlds*
                              for sr = (world-figure (first row) :sun-radius)
                              when (and sr (> sr here))
                                append
                                (list (first row)
                                      (format nil "the programmed road to ~a"
                                              (world-figure (first row)
                                                            :name))))))))

   (wheel-control :type 'gwl:menu-form-control
                  :prompt "wheel: "
                  :size 1
                  :default :amidships
                  :choice-plist (list :hard-port "hard over, to port"
                                      :easy-port "easy, to port"
                                      :amidships "amidships"
                                      :easy-starboard "easy, to starboard"
                                      :hard-starboard "hard over, to starboard"))

   ;; the shifter carries direction and nothing else: forward,
   ;; neutral, reverse.  In neutral no pedal reaches the engines,
   ;; and she coasts -- an automatic's N, not a clutch.
   (shifter-control :type 'gwl:menu-form-control
                    :prompt "shifter: "
                    :size 1
                    :default :neutral
                    :choice-plist (list :forward "forward"
                                        :neutral "neutral — coasting"
                                        :reverse "reverse — nose-about"))

   (pedal-control :type 'gwl:menu-form-control
                  :prompt "pedal: "
                  :size 1
                  :default :burn
                  :choice-plist (list :burn "burn — the full pedal"
                                      :feather "feather — a breath of gas"
                                      :brake "brake"))

   ;; the radio: how long a turn runs, worn as the truck's dash
   ;; radio.  Four channels -- the faster the music, the bigger the
   ;; jump per turn -- and every move falls that long, even in
   ;; neutral with the wheel amidships: time passes whether or not
   ;; you touch anything, which is the whole lesson.  The real
   ;; radio inputs ride hidden in the form; the faceplate's preset
   ;; buttons drive them, the same mirror the 3D rigs use.
   (cadence-control :type 'gwl:radio-form-control
                    :default :slow
                    :choice-plist (list :slow "slow"
                                        :medium "medium"
                                        :fast "fast"
                                        :fastest "fastest"))

   ;; the tape transport: play runs the turn forward, rewind runs
   ;; it BACKWARD -- gravity is symmetric, so the deck can carry
   ;; the fall either way.  The engines don't light in rewind.
   (transport-control :type 'gwl:radio-form-control
                      :default :play
                      :choice-plist (list :play "play"
                                          :rewind "rewind"))

   ;; the log-book signature line: a hidden field the browser fills
   ;; with its own pilot token at boarding, posted once through the
   ;; same gdlAjax pipe the helm uses (check-in!).  Riding the form
   ;; keeps the token out of every URL.
   (pilot-control :type 'gwl:text-form-control
                  :default ""
                  :size 70))

  :hidden-objects
  (;; the no-reload machinery: the page's moving parts are stock
   ;; sheet-sections, swapped in place through the gdlAjax pipe
   ;; when the move button's gdl-ajax-call runs after-set!.
   ;; :js-to-eval :parse evaluates each section's scripts on
   ;; arrival -- the helm section's state script re-draws the
   ;; plot, re-wires the hands, and refreshes the scene per
   ;; renderer, every turn.
   (helm-section
    :type 'sheet-section
    :js-to-eval :parse
    :inner-html
    (with-lhtml-string ()
      (:div :style "font-size:14px;letter-spacing:0.06em;cursor:pointer;display:flex;justify-content:space-between;align-items:center;gap:10px;"
              :onclick "toggleHelm()"
          (:span "THE HELM")
          (:span :id "helm-readout"
            :style "font-weight:bold;letter-spacing:0.08em;color:#f4dc6a;"
            (str (the helm-readout-html)))
          (:span :id "helm-caret" "▾"))
        (:div :id "helm-body" :style "margin-top:8px;"
        (str (the helm-form-html))
        ;; the state block: DIMMED and tagged "as plotted" while a
        ;; voyage clip flies (the figures describe the road's END,
        ;; not the road), full gold again the moment the clip lands
        ;; -- see hudOn/hudOff in the plot script
        (:div :id "helm-state" :style "margin-top:10px;border-top:1px solid #7a6a1f;padding-top:8px;line-height:1.5;"
          (:div (if (the landed?)
                    (fmt "down on: ~a" (the world-name))
                    (fmt "falling around: ~a" (the world-name))))
          (:div (fmt "heading: ~3,'0d" (mod (round (the heading-deg)) 360)))
          (:div (fmt "speed: ~,2f km/s" (the speed)))
          (:div :id "coords-line" :style "font-size:11px;color:#c9a227;"
            (str (the coords-line-html)))
          ;; on the ground there is no road to describe -- the world
          ;; itself holds him
          (if (the landed?)
              (htm (:div "engines cold — forward and the full pedal to climb"))
              (htm
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
                          "the road is unbound — the deep dark has you")))))
          (:div :id "gw-clock" (fmt "ship's clock: ~a" (the clock-string)))
          (:div (fmt "moves made: ~d" (the moves-count)))
          (:div :id "move-note"
            :style "margin-top:6px;font-size:11px;font-style:italic;color:#c9a227;"
            (str (the last-move-note)))))
      (:script (str (the section-state-js)))))

   (plot-section
    :type 'sheet-section
    :js-to-eval :parse
    :inner-html
    (with-lhtml-string ()
      (:div :id "plot-body" :style "margin-top:6px;"
          (:canvas :id "plot-canvas"
            :width (format nil "~d" (the plot-size))
            :height (format nil "~d" (the plot-size))
            :style "display:block;")
          (:div :id "plot-coords"
            :style "font-size:12px;color:#e8c839;margin-top:4px;font-variant-numeric:tabular-nums;")
          (:div :id "plot-frame-label"
            :style "font-size:10px;color:#c9a227;margin-top:2px;"))))

(scene-section
    :type 'sheet-section
    :js-to-eval :parse
    :inner-html
    ;; a hidden TEMPLATE, not the live scene: x3dom's reload-after-
    ;; swap crashed tearing down the orphaned GL context, so the
    ;; live x3d element never leaves the page -- the refresh
    ;; transplants these children into the LIVE Scene, which x3dom
    ;; applies incrementally under a living context
    (with-lhtml-string ()
      (:div :id "gw-scene-template"
            (:|Background| :|skyColor| "0 0 0.012")
            (str (the viewpoints-x3d))
            ;; the universe turns around the ship, never the ship
            ;; around the universe -- and nothing in it moves between
            ;; moves: the scene stands until the pilot acts
            (:|NavigationInfo| :headlight "false")
            (:|Transform| :|DEF| "sky-heading" :|id| "sky-heading"
              :rotation (format nil "0 0 1 ~,5f"
                                (- (the sky-authored-heading-rad)))
              (str (sun-light-x3d :elevation (the sun-elevation)))
              (:|Transform| :|DEF| "sky-drift"
                (str (starfield-x3d :radius 5000.0d0))))
            (str (the bodies-x3d))
            (:|Group|
              (str (cab-light-x3d))
              (str (cockpit-x3d))
              (str (dice-x3d (the dice-lean)))
              (str (gauge-needle-x3d 0 0.46 (the speedo-phi) 0.055))
              (str (gauge-needle-x3d 0.19 0.45 (the heading-deg) 0.042))
              (str (gauge-needle-x3d -0.19 0.45 (the vario-phi) 0.042))
              (str (dash-radio-x3d (the cadence-control value)
                                   (the transport-control value))))
))))

  :functions
  (;; Fall through DT seconds of gravity: velocity Verlet in
   ;; one-minute substeps.  Returns the new (vx vy px py).  A
   ;; NEGATIVE dt runs the fall backward: gravity is symmetric, so
   ;; the rewind negates the velocities, falls |dt| forward, and
   ;; negates them back -- the exact same road, unfallen.
   (fall
    (vx vy px py dt)
    (if (minusp dt)
        (destructuring-bind (rvx rvy rpx rpy)
            (the (fall (- vx) (- vy) px py (- dt)))
          (list (- rvx) (- rvy) rpx rpy))
        (the (fall-forward vx vy px py dt))))

   (fall-forward
    (vx vy px py dt)
    (let ((h 60.0d0)
          (mu (the world-mu))
          (sky (the world-sky)))
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
                    vy (+ vy (* 0.5 (+ ay0 ay1) h)))))
          ;; the sky is solid at every substep: a long move must not
          ;; tunnel through the world between checks
          (when (< (sqrt (+ (* px px) (* py py))) sky)
            (return)))
        (list vx vy px py))))

   ;; One move of the game, folded into the ship's state when the
   ;; helm form posts.  With the programmed road selected, the move
   ;; IS the voyage; otherwise turn the wheel, then burn (or don't),
   ;; then FALL as long as the radio plays -- a minute on the slow
   ;; channel, a day on the fastest.  In neutral no pedal reaches
   ;; the engines -- an automatic's N.  The brake
   ;; is heard and changes nothing.  Rewind on the tape transport
   ;; runs the turn backward, engines cold.  Meet the sky of the
   ;; world and you are set back on the ring.
   ;; the browser's boarding post (see the signature script by the
   ;; move button): the hidden line arrives bashed, the token gets
   ;; the harbor-gate scrub, and the pilot signs the log book once
   ;; per session -- every later tally! of this cockpit lands on his
   ;; page through the after-set! binding below.
   (check-in!
    ()
    (unless (the pilot-id)
      (let ((id (clean-pilot-id (the pilot-control value))))
        (when id
          (check-in-pilot! id)
          (the (set-slot! :pilot-id id))))))

   (after-set!
    ()
    (let* ((*current-pilot* (the pilot-id))
           (choice (the voyage-control value))
           (road? (not (member choice '(:hand :down)))))
      (cond ;; a road bought from the ground buys THE CLIMB first,
            ;; and the climb is FLOWN: this move is the lift-off
            ;; -- the plain falls away, the globe emerges, the
            ;; ring takes him -- and the bought road STAYS on the
            ;; select, standing ready for the next press of the
            ;; starter (ruled 2026-09-01: the climb must be seen,
            ;; not just paid; supersedes pt 58's one-move fold)
            ((and road? (the landed?))
             (the (fly-climb-road!
                   :road-standing (getf (the voyage-control choice-plist)
                                        choice))))
            ((and (eql choice :down) (not (the landed?)))
             (the fly-landing-road!))
            ((and (eql choice :moon) (eql (the world) :home))
             (the fly-moon-road!))
            ;; the moon rides home, not the sun: his road home is
            ;; the moon road flown back, not a sun's road
            ((and (eql choice :home) (eql (the world) :moon))
             (the fly-moon-road-home!))
            ((and (assoc choice *worlds*)
                  (world-figure choice :sun-radius)
                  (world-figure (the world) :sun-radius)
                  (not (eql choice (the world))))
             (the (fly-sun-road! choice)))
            (t (the make-helm-move!)))))

   ;; The lift-off, FLOWN: straight up off the pad at her parked
   ;; seat -- the plain falling away, the globe emerging below --
   ;; the nose leaning prograde over the climb's back half, and the
   ;; ring takes him at his seat's own bearing.  The yard's gift,
   ;; shared by the gas-pedal climb and any road bought from the
   ;; ground; ROAD-STANDING (a label) says a bought road waits on
   ;; the select for the next move.
   (fly-climb-road!
    (&key road-standing)
    (let* ((sky (the world-sky))
           (ring (the world-ring))
           (phi0 (atan (the pos-y) (the pos-x)))
           (h-up phi0)                       ; nose away from the center
           (h-pro (+ phi0 (/ pi 2)))        ; prograde on the ring
           (n 24)
           ;; the climb's time: the half-ellipse from the sky to the
           ;; ring, the yard's own pace
           (tof (* pi (sqrt (/ (expt (* 0.5 (+ sky ring)) 3) (the world-mu)))))
           (t0 (the game-seconds))
           (time-map (list (cons 0.0 0.0)))
           (samples (list (list 0.0 h-up (the pos-x) (the pos-y)))))
      (dotimes (i (1+ n))
        (let* ((f (/ i n))
               ;; ease the radius: slow off the pad, fast at the top
               (r (+ sky (* (- ring sky) (* f f))))
               ;; the lean from straight-up to prograde rides the
               ;; climb's back half
               (lean (max 0.0 (/ (- f 0.5) 0.5)))
               (h (+ h-up (* lean (- h-pro h-up)))))
          (push (list (+ 0.05 (* 0.90 f)) h
                      (* r (cos phi0)) (* r (sin phi0)))
                samples)
          (push (cons (+ 0.05 (* 0.90 f)) (* tof f)) time-map)))
      (push (list 1.0 h-pro (* ring (cos phi0)) (* ring (sin phi0)))
            samples)
      (push (cons 1.0 tof) time-map)
      (setq samples (nreverse samples)
            time-map (nreverse time-map))
      (the (set-slot! :landed? nil))
      (the (set-slot! :heading-deg (mod (round (* h-pro (/ 180 pi))) 360)))
      (the (set-slot! :pos-x (* ring (cos phi0))))
      (the (set-slot! :pos-y (* ring (sin phi0))))
      (the (set-slot! :vel-x (* (the world-ring-speed) (- (sin phi0)))))
      (the (set-slot! :vel-y (* (the world-ring-speed) (cos phi0))))
      (the (set-slot! :last-burn :forward))
      (the (set-slot! :moves-count (1+ (the moves-count))))
      (the (advance-clock! tof))
      (the (set-slot! :transit-samples samples))
      (the (set-slot! :transit-time-map time-map))
      (the (set-slot! :transit-target :up))
      (the (set-slot! :transit-bodies
            (append
             (list (list :prefix "planet"
                         :texture (world-figure (the world) :texture)
                         :radius (the world-radius)
                         :targets (make-list (length samples)
                                             :initial-element (list 0 0))
                         :spin-phase (world-phase-rad (the world) t0)
                         :spin-rate (world-spin-rate (the world))
                         :diffuse (world-figure (the world) :diffuse)
                         :emissive (world-figure (the world) :emissive)
                         :tilt (world-figure (the world) :tilt)
                         :adornment (world-figure (the world) :adornment)))
             (when (eql (the world) :home)
               (list (list :prefix "moon" :texture "moon-tex"
                           :radius +moon-radius+
                           :targets (make-list (length samples)
                                               :initial-element
                                               (list +moon-x+ 0))
                           :spin-phase (world-phase-rad :moon t0)
                           :spin-rate (world-spin-rate :moon)
                           :diffuse "0.75 0.74 0.70"
                           :emissive "0.10 0.10 0.09")))
             (when (eql (the world) :moon)
               (list (list :prefix "home-far" :texture "earth-tex"
                           :radius +planet-radius+
                           :targets (make-list (length samples)
                                               :initial-element
                                               (list (- +moon-x+) 0))
                           :spin-phase (world-phase-rad :home t0)
                           :spin-rate (world-spin-rate :home)
                           :diffuse "0.10 0.18 0.85"
                           :emissive "0.05 0.07 0.12"))))))
      (the (set-slot! :transit-beats
            (list (cons 0.02 "the engines light -- the long climb begins")
                  (cons 0.10 "the ground falls away")
                  (cons 0.55 "the nose leans down the road -- building the ring's pace")
                  (cons 0.95 "the ring takes him"))))
      (the (set-slot! :last-move-note
            (if road-standing
                (format nil "the long climb off ~a, and the ring takes him.  ~a stands ready on the select -- make the move, and she flies"
                        (the world-name) road-standing)
                (format nil "the engines light -- the long climb off ~a, and the ring takes him back"
                        (the world-name)))))
      (tally! :moves)
      (tally! :takeoffs)))

   ;; The programmed landing: the yard takes the helm and FLIES the
   ;; descent -- the scene rides the whole road down: a retro kick
   ;; dropping the low point onto the sky, half a lap falling, and
   ;; the big brake at the touch, the ground rising to meet her at
   ;; the clip's end (see the ground-lift in bodies-x3d).  The
   ;; staging road for the view from the ground; the BY-HAND
   ;; landing stays the game (the doctrine: shed the road's speed
   ;; before the surface has to).  Figures at teaching grain: the
   ;; descent conic is cut from her current radius as if the road
   ;; were round, its apoapsis at her seat -- and it runs in her
   ;; orbit's own sense, so the watch's clockwise stays clockwise
   ;; on the way down.
   (fly-landing-road!
    ()
    (let* ((mu (the world-mu))
           (sky (the world-sky))
           (r1 (max (the radius) (* 1.02 sky)))
           (phi0 (atan (the pos-y) (the pos-x)))
           (alpha (- phi0 pi))
           ;; mirror the canonical ellipse when she runs clockwise
           (sense (if (minusp (the specific-h)) -1 1))
           (a (* 0.5 (+ r1 sky)))
           (e (/ (- r1 sky) (+ r1 sky)))
           (p (* a (- 1 (* e e))))
           (vcoeff (sqrt (/ mu p)))
           (dv1 (- (sqrt (/ mu r1))
                   (sqrt (* mu (- (/ 2 r1) (/ 1 a))))))
           (dv2 (sqrt (* mu (- (/ 2 sky) (/ 1 a)))))
           (n 32)
           ;; the descent's time: half a lap of the conic
           (n-d (sqrt (/ mu (* a a a))))
           (tof (/ pi n-d))
           (t0 (the game-seconds))
           (time-map (list (cons 0.0 0.0)))
           ;; pre-burn beat: wherever and however she stands
           (samples (list (list 0.0 (deg->rad (the heading-deg))
                                (the pos-x) (the pos-y))))
           (prev-h nil))
      ;; apo -> peri, the ellipse rotated so its high point sits at
      ;; her seat; positions and headings mirrored together when
      ;; the sense runs clockwise
      (dotimes (i (1+ n))
        (let* ((ecc-anom (+ pi (* pi (/ i n))))
               (r (* a (- 1 (* e (cos ecc-anom)))))
               (theta (atan (* (sqrt (- 1 (* e e))) (sin ecc-anom))
                            (- (cos ecc-anom) e)))
               (h (* sense (atan (* vcoeff (+ e (cos theta)))
                                 (* vcoeff (- (sin theta)))))))
          ;; unwrap the heading so the nose track never jumps a lap
          (when prev-h
            (loop while (> (- h prev-h) pi) do (decf h (* 2 pi)))
            (loop while (< (- h prev-h) (- pi)) do (incf h (* 2 pi))))
          (setq prev-h h)
          (let ((ang (+ (* sense theta) alpha)))
            (push (list (+ 0.06 (* 0.84 (/ i n)))
                        (+ h alpha)
                        (* r (cos ang))
                        (* r (sin ang)))
                  samples)
            ;; Kepler: game-time since the retro kick, from the apoapsis
            (push (cons (+ 0.06 (* 0.84 (/ i n)))
                        (/ (- ecc-anom (* e (sin ecc-anom)) pi) n-d))
                  time-map))))
      ;; the flip for the brake: the nose swings RETROGRADE over
      ;; the last beats -- she brakes tail-first, the way the big
      ;; brake must be pointed -- and holds the pose through the
      ;; touch while the ground rises to meet her
      (destructuring-bind (key fh fx fy) (first samples)
        (declare (ignore key))
        (push (list 0.95 (+ fh pi) fx fy) samples)
        (push (list 1.0 (+ fh pi) fx fy) samples))
      (push (cons 0.95 tof) time-map)
      (push (cons 1.0 tof) time-map)
      (setq samples (nreverse samples)
            time-map (nreverse time-map))
      (destructuring-bind (key fh fx fy) (car (last samples))
        (declare (ignore key))
        (the (set-slot! :heading-deg (mod (round (* fh (/ 180 pi))) 360)))
        (the (set-slot! :pos-x fx))
        (the (set-slot! :pos-y fy)))
      (the (set-slot! :vel-x 0))
      (the (set-slot! :vel-y 0))
      (the (set-slot! :landed? t))
      (the (set-slot! :last-burn :none))
      (the (set-slot! :moves-count (1+ (the moves-count))))
      (the (advance-clock! tof))
      (the (set-slot! :transit-samples samples))
      (the (set-slot! :transit-time-map time-map))
      (the (set-slot! :transit-target :down))
      (the (set-slot! :transit-bodies
            (append
             (list (list :prefix "planet"
                         :texture (world-figure (the world) :texture)
                         :radius (the world-radius)
                         :targets (make-list (length samples)
                                             :initial-element (list 0 0))
                         :spin-phase (world-phase-rad (the world) t0)
                         :spin-rate (world-spin-rate (the world))
                         :diffuse (world-figure (the world) :diffuse)
                         :emissive (world-figure (the world) :emissive)
                         :tilt (world-figure (the world) :tilt)
                         :adornment (world-figure (the world) :adornment)))
             ;; the sky's other rider comes along for the way down
             (when (eql (the world) :home)
               (list (list :prefix "moon" :texture "moon-tex"
                           :radius +moon-radius+
                           :targets (make-list (length samples)
                                               :initial-element
                                               (list +moon-x+ 0))
                           :spin-phase (world-phase-rad :moon t0)
                           :spin-rate (world-spin-rate :moon)
                           :diffuse "0.75 0.74 0.70"
                           :emissive "0.10 0.10 0.09")))
             (when (eql (the world) :moon)
               (list (list :prefix "home-far" :texture "earth-tex"
                           :radius +planet-radius+
                           :targets (make-list (length samples)
                                               :initial-element
                                               (list (- +moon-x+) 0))
                           :spin-phase (world-phase-rad :home t0)
                           :spin-rate (world-spin-rate :home)
                           :diffuse "0.10 0.18 0.85"
                           :emissive "0.05 0.07 0.12"))))))
      (the (set-slot! :transit-beats
            (list (cons 0.02 "the yard takes the helm -- hands off for the descent")
                  (cons 0.06 (format nil "the retro kick: -~,2f km/s, and the low point drops onto the sky" dv1))
                  (cons 0.45 "falling down the well -- the ground coming up to meet her")
                  (cons 0.90 (format nil "the big brake: -~,2f km/s, shed before the surface has to" dv2))
                  (cons 0.975 "DOWN -- engines cold, the world turning under him"))))
      (the (set-slot! :last-move-note
            (format nil "the yard flew her down: a retro kick (-~,2f km/s) dropped the low point onto the sky, half a lap falling, and the big brake (-~,2f) at the touch -- DOWN on ~a, engines cold.  The by-hand landing is the real game; forward and the full pedal to climb"
                    dv1 dv2 (the world-name))))
      (the voyage-control (set-slot! :value :hand))
      (tally! :moves)
      (tally! :yard-landings)))

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
           (dv1 (- (sqrt (* +mu+ (- (/ 2 r1) (/ 1 a)))) (sqrt (/ +mu+ r1))))
           ;; the capture kick is onto the MOON's watch now, out of
           ;; the transfer's apoapsis crawl -- his ring, his mu
           (dv2 (- (sqrt (/ +mu-moon+ +moon-watch-radius+))
                   (sqrt (* +mu+ (- (/ 2 r2) (/ 1 a))))))
           (n-tr (sqrt (/ +mu+ (* a a a))))
           (tof (/ pi n-tr))
           (tof-days (/ tof 86400))
           (vcoeff (sqrt (/ +mu+ p)))
           (n 32)
           ;; the clock before the move: the faces the clip opens on
           (t0 (the game-seconds))
           (h0 (deg->rad (the heading-deg)))
           ;; the opening beat: the coast round to the ring's
           ;; departure point, in her own sense (coast-to-node)
           (sense (if (minusp (the specific-h)) -1 1))
           (coast (multiple-value-list
                   (coast-to-node h0 (the pos-x) (the pos-y) (the radius)
                                  r1 +mu+ sense)))
           (wait (second coast))
           (prev-h (third coast))
           (coast-hours (/ wait 3600))
           ;; the samples and the clip's clock, built newest-first
           (samples (append (reverse (first coast))
                            (list (list 0.0 h0 (the pos-x) (the pos-y)))))
           (time-map (list (cons 0.05 wait) (cons 0.0 0.0))))
      (dotimes (i (1+ n))
        (let* ((ecc-anom (* pi (/ i n)))
               (r (* a (- 1 (* e (cos ecc-anom)))))
               (theta (atan (* (sqrt (- 1 (* e e))) (sin ecc-anom))
                            (- (cos ecc-anom) e)))
               ;; unwrap the heading so the nose track never jumps a lap
               (h (unwrap-heading (atan (* vcoeff (+ e (cos theta)))
                                        (* vcoeff (- (sin theta))))
                                  prev-h))
               ;; Kepler: game-time since the kick at this sample
               (tim (/ (- ecc-anom (* e (sin ecc-anom))) n-tr))
               (key (+ 0.10 (* 0.83 (/ i n)))))
          (setq prev-h h)
          (push (list key h (* r (cos theta)) (* r (sin theta))) samples)
          (push (cons key (+ wait tim)) time-map)))
      ;; the closing beat: nose-in on the moon
      (push (list 1.0 (unwrap-heading pi prev-h) (- r2) 0) samples)
      (push (cons 1.0 (+ wait tof)) time-map)
      (setq samples (nreverse samples)
            time-map (nreverse time-map))
      ;; she arrives in the MOON's OWN FRAME, truly falling around
      ;; him: the watch is state now, not scenery.  Clockwise, the
      ;; way the transfer's apoapsis crawl handed her over.
      (the (set-slot! :world :moon))
      (the (set-slot! :landed? nil))
      (the (set-slot! :heading-deg 180))
      (the (set-slot! :vel-x 0))
      (the (set-slot! :vel-y (- (sqrt (/ +mu-moon+ +moon-watch-radius+)))))
      (the (set-slot! :pos-x +moon-watch-radius+))
      (the (set-slot! :pos-y 0))
      (the (set-slot! :last-burn :none))
      (the (set-slot! :moves-count (1+ (the moves-count))))
      ;; the road spends the wait and the fall
      (the (advance-clock! (+ wait tof)))
      (the (set-slot! :transit-samples samples))
      (the (set-slot! :transit-time-map time-map))
      (the (set-slot! :transit-target :moon))
      (the (set-slot! :transit-bodies
            (list (list :prefix "planet" :texture "earth-tex"
                        :radius +planet-radius+
                        :targets (make-list (length samples)
                                            :initial-element (list 0 0))
                        :spin-phase (world-phase-rad :home t0)
                        :spin-rate (world-spin-rate :home)
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
                        :spin-phase (world-phase-rad :moon t0)
                        :spin-rate (world-spin-rate :moon)
                        :diffuse "0.75 0.74 0.70" :emissive "0.10 0.10 0.09"
                        :scale-override 0.001))))
      (the (set-slot! :transit-beats
            (append
             (list (cons 0.02 "hands off the wheel -- the programmed road has her"))
             (when (> wait 60)
               (list (cons 0.035 (format nil "coasting round to the departure point -- ~,1f hours on the clock" coast-hours))))
             (list (cons 0.10 (format nil "the kick at perigee: +~,2f km/s, and the road out opens" dv1))
                   (cons 0.35 (format nil "falling uphill -- ~,1f days of coast, compressed to a breath" tof-days))
                   (cons 0.90 (format nil "the capture kick: +~,2f km/s, onto the moon's watch" dv2))
                   (cons 0.97 "the watch takes her -- nose-in on a new world")))))
      (the (set-slot! :last-move-note
                      (format nil "the programmed road: ~aa kick at perigee (+~,2f km/s), ~,1f days falling uphill, a second kick (+~,2f) -- and she settles into the watch around the moon.  The helm is yours."
                              (if (> wait 60)
                                  (format nil "~,1f hours coasting round to the departure point, " coast-hours)
                                  "")
                              dv1 tof-days dv2)))
      (the voyage-control (set-slot! :value :hand))
      (tally! :moves)
      (tally! :voyages)
      (tally! :moon-voyages)))

   ;; The programmed road home: the moon road flown the other way.
   ;; A kick out of the moon's grip onto the transfer's apogee
   ;; crawl, the same half-ellipse fallen down the well -- the
   ;; lower half of the plane, the mirror of the road out -- and
   ;; the big brake at perigee onto the home ring.  The same two
   ;; kicks as the road out, spent in the other order: cheap to
   ;; leave the moon, dear to stop at the bottom of the well.  The
   ;; scene flies the leg in home's frame, the moon falling astern
   ;; at full size, home growing in the glass, and the arrival
   ;; mirrors every road's: nose-in, the world square in the
   ;; windshield.
   (fly-moon-road-home!
    ()
    (let* ((r1 +ring-radius+) (r2 +moon-road-radius+)
           (a (* 0.5 (+ r1 r2)))
           (e (/ (- r2 r1) (+ r2 r1)))
           (p (* a (- 1 (* e e))))
           ;; the departure kick, off the moon's watch and onto the
           ;; transfer's apogee -- his grip let go
           (dv1 (- (sqrt (/ +mu-moon+ +moon-watch-radius+))
                   (sqrt (* +mu+ (- (/ 2 r2) (/ 1 a))))))
           ;; the capture brake at perigee, down to the ring's own
           ;; pace -- the same figure the road out spent to leave
           (dv2 (- (sqrt (* +mu+ (- (/ 2 r1) (/ 1 a))))
                   (sqrt (/ +mu+ r1))))
           (n-tr (sqrt (/ +mu+ (* a a a))))
           (tof (/ pi n-tr))
           (tof-days (/ tof 86400))
           (vcoeff (sqrt (/ +mu+ p)))
           (n 32)
           (t0 (the game-seconds))
           (h0 (deg->rad (the heading-deg)))
           ;; the opening beat: the coast round the moon's watch to
           ;; the transfer's own node, in her own sense.  The scene
           ;; flies the leg in home's frame, so the coast rides
           ;; there too (the moon at his node)
           (sense (if (minusp (the specific-h)) -1 1))
           (coast (multiple-value-list
                   (coast-to-node h0 (the pos-x) (the pos-y) (the radius)
                                  +moon-watch-radius+ +mu-moon+ sense
                                  :center-x +moon-x+)))
           (wait (second coast))
           (prev-h (third coast))
           (coast-hours (/ wait 3600))
           (samples (append (reverse (first coast))
                            (list (list 0.0 h0
                                        (+ +moon-x+ (the pos-x)) (the pos-y)))))
           (time-map (list (cons 0.05 wait) (cons 0.0 0.0))))
      (dotimes (i (1+ n))
        (let* ((ecc-anom (+ pi (* pi (/ i n))))
               (r (* a (- 1 (* e (cos ecc-anom)))))
               (theta (atan (* (sqrt (- 1 (* e e))) (sin ecc-anom))
                            (- (cos ecc-anom) e)))
               ;; unwrap the heading so the nose track never jumps a lap
               (h (unwrap-heading (atan (* vcoeff (+ e (cos theta)))
                                        (* vcoeff (- (sin theta))))
                                  prev-h))
               ;; Kepler: game-time since the kick, from the apogee
               (tim (/ (- ecc-anom (* e (sin ecc-anom)) pi) n-tr))
               (key (+ 0.10 (* 0.83 (/ i n)))))
          (setq prev-h h)
          (push (list key h (* r (cos theta)) (* r (sin theta))) samples)
          (push (cons key (+ wait tim)) time-map)))
      ;; the closing beat: nose-in on home, swung on from prograde
      ;; without ever unwinding the lap the fall wound up
      (push (list 1.0 (unwrap-heading pi prev-h) r1 0) samples)
      (push (cons 1.0 (+ wait tof)) time-map)
      (setq samples (nreverse samples)
            time-map (nreverse time-map))
      ;; she arrives back in HOME's frame, on the ring she first
      ;; rode: circular, prograde, nose-in
      (the (set-slot! :world :home))
      (the (set-slot! :landed? nil))
      (the (set-slot! :heading-deg 180))
      (the (set-slot! :vel-x 0))
      (the (set-slot! :vel-y +ring-speed+))
      (the (set-slot! :pos-x +ring-radius+))
      (the (set-slot! :pos-y 0))
      (the (set-slot! :last-burn :none))
      (the (set-slot! :moves-count (1+ (the moves-count))))
      (the (advance-clock! (+ wait tof)))
      (the (set-slot! :transit-samples samples))
      (the (set-slot! :transit-time-map time-map))
      (the (set-slot! :transit-target :homeward))
      (the (set-slot! :transit-bodies
            (list (list :prefix "planet" :texture "earth-tex"
                        :radius +planet-radius+
                        :targets (make-list (length samples)
                                            :initial-element (list 0 0))
                        :spin-phase (world-phase-rad :home t0)
                        :spin-rate (world-spin-rate :home)
                        :diffuse "0.10 0.18 0.85" :emissive "0.05 0.07 0.12")
                  ;; the moon at full size from the first frame:
                  ;; she departs his very doorstep, and he falls
                  ;; astern the whole way down
                  (list :prefix "moon" :texture "moon-tex"
                        :radius +moon-radius+
                        :targets (make-list (length samples)
                                            :initial-element (list +moon-x+ 0))
                        :spin-phase (world-phase-rad :moon t0)
                        :spin-rate (world-spin-rate :moon)
                        :diffuse "0.75 0.74 0.70" :emissive "0.10 0.10 0.09"))))
      (the (set-slot! :transit-beats
            (append
             (list (cons 0.02 "cast off the watch -- the road home has her"))
             (when (> wait 60)
               (list (cons 0.035 (format nil "coasting round the watch to the node -- ~,1f hours on the clock" coast-hours))))
             (list (cons 0.10 (format nil "the kick off the watch: +~,2f km/s, and the moon's grip lets go" dv1))
                   (cons 0.35 (format nil "falling down the well -- ~,1f days, home growing in the glass" tof-days))
                   (cons 0.90 (format nil "the big brake at perigee: -~,2f km/s, down to the ring's own pace" dv2))
                   (cons 0.97 "the home ring takes her back")))))
      (the (set-slot! :last-move-note
                      (format nil "the road home: ~aa kick off the moon's watch (+~,2f km/s), ~,1f days falling down the well, and the big brake at perigee (-~,2f) -- the home ring takes her back.  The helm is yours."
                              (if (> wait 60)
                                  (format nil "~,1f hours coasting round the watch, " coast-hours)
                                  "")
                              dv1 tof-days dv2)))
      (the voyage-control (set-slot! :value :hand))
      (tally! :moves)
      (tally! :voyages)
      (tally! :homecomings)))

   ;; The sun's road to another world: an escape kick out of the
   ;; grip of the world that has her, the sun's own Hohmann from
   ;; her road to the new world's road, and a capture kick onto the
   ;; ring there -- patched conics at teaching grain, real figures
   ;; throughout, every leg read from the worlds table (home to
   ;; Mars, Mars onward to Jupiter, or straight out from home).
   ;; And the window is real: each road only exists when the new
   ;; world stands the right few dozen degrees ahead along the
   ;; sun's road, so she waits for it (a departure board is a later
   ;; lesson).  The same conic serves both directions: an inward
   ;; leg -- the road home -- runs the ellipse from its far end,
   ;; the eccentricity signed negative and every formula standing.  The scene flies the leg heliocentric -- the world
   ;; she left falls astern and shrinks to a spark while every
   ;; world keeps riding its own road, the new one rises ahead and
   ;; grows -- and the arrival mirrors the departure, nose-in with
   ;; him close aboard, the moon canon over the new world.
   (fly-sun-road!
    (to-world)
    (let* ((from-world (the world))
           (re (world-figure from-world :sun-radius))
           (rm (world-figure to-world :sun-radius))
           (mu1 (the world-mu))
           (mu2 (world-figure to-world :mu))
           (a (* 0.5 (+ re rm)))
           (e (/ (- rm re) (+ rm re)))
           (p (* a (- 1 (* e e))))
           (vcoeff (sqrt (/ +mu-sun+ p)))
           (n-tr (sqrt (/ +mu-sun+ (* a a a))))
           (tof (/ pi n-tr))
           (tof-months (/ tof (* 86400 30.44)))
           (n-e (sqrt (/ +mu-sun+ (* re re re))))
           (n-m (sqrt (/ +mu-sun+ (* rm rm rm))))
           ;; the window: where the new world must stand at
           ;; departure so ship and world arrive at the same node
           ;; together
           (mars0 (- pi (* n-m tof)))
           (window-deg (* mars0 (/ 180 pi)))
           ;; phrased for the sky as she sees it: on an inward leg
           ;; the faster inner world laps her, and the window reads
           ;; astern
           (window-phrase (let ((w (mod (round window-deg) 360)))
                            (if (<= w 180)
                                (format nil "~d degrees ahead of" w)
                                (format nil "~d degrees astern of" (- 360 w)))))
           (leg-span (if (> tof-months 24)
                         (format nil "~,1f years" (/ tof (* 86400 365.25)))
                         (format nil "~,1f months" tof-months)))
           ;; the escape kick, from wherever she rides now
           (r1 (the radius))
           (v-inf-dep (- (* vcoeff (+ 1 e)) (sqrt (/ +mu-sun+ re))))
           (dv1 (- (sqrt (+ (/ (* 2 mu1) r1) (* v-inf-dep v-inf-dep)))
                   (sqrt (/ mu1 r1))))
           ;; the capture kick, onto the ring over the new world
           (r2 (world-figure to-world :ring))
           (v-circ2 (sqrt (/ mu2 r2)))
           (v-inf-arr (- (sqrt (/ +mu-sun+ rm)) (* vcoeff (- 1 e))))
           (dv2 (- (sqrt (+ (/ (* 2 mu2) r2) (* v-inf-arr v-inf-arr)))
                   v-circ2))
           ;; the new world's track carries him one ring outboard of
           ;; the road's end, so the arrival closes with him close
           ;; aboard
           (rm+ (+ rm r2))
           (n 40)
           ;; where she stands off her world's center at departure:
           ;; the transfer's perihelion sits AT that world in the
           ;; sun's frame, so this offset fades out over the early
           ;; samples -- the escape unwinding -- keeping the world
           ;; astern at the swing beat and the arrival exact
           (ox (the pos-x)) (oy (the pos-y))
           ;; pre-burn beat: wherever and however she stands, in the
           ;; sun's frame -- her world rides at (re, 0) at departure
           (samples (list (list 0.0 (deg->rad (the heading-deg))
                                (+ re ox) oy)))
           (asterns (list (list re 0)))
           (moons (list (list (+ re +moon-x+) 0)))
           (dests (list (list (* rm+ (cos mars0)) (* rm+ (sin mars0)))))
           (prev-h nil)
           (t0 (the game-seconds))
           (time-map (list (cons 0.0 0.0))))
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
          (push (list (* re (cos th-e)) (* re (sin th-e))) asterns)
          (push (list (+ (* re (cos th-e)) +moon-x+) (* re (sin th-e)))
                moons)
          (push (list (* rm+ (cos th-m)) (* rm+ (sin th-m))) dests)
          (push (cons (+ 0.07 (* 0.86 (/ i n))) tim) time-map)))
      ;; the closing beat: nose-in on the new world, the old one
      ;; dead astern of the sun
      (push (list 1.0 pi (- rm) 0) samples)
      (let ((th-e (* n-e tof)))
        (push (list (* re (cos th-e)) (* re (sin th-e))) asterns)
        (push (list (+ (* re (cos th-e)) +moon-x+) (* re (sin th-e))) moons))
      (push (list (- rm+) 0) dests)
      (push (cons 1.0 tof) time-map)
      (setq samples (nreverse samples)
            time-map (nreverse time-map)
            asterns (nreverse asterns)
            moons (nreverse moons)
            dests (nreverse dests))
      (the (set-slot! :world to-world))
      (the (set-slot! :landed? nil))
      (the (set-slot! :heading-deg 180))
      (the (set-slot! :vel-x 0))
      (the (set-slot! :vel-y v-circ2))
      (the (set-slot! :pos-x r2))
      (the (set-slot! :pos-y 0))
      (the (set-slot! :last-burn :none))
      (the (set-slot! :moves-count (1+ (the moves-count))))
      (the (advance-clock! tof))
      (the (set-slot! :transit-samples samples))
      (the (set-slot! :transit-time-map time-map))
      (the (set-slot! :transit-target to-world))
      (the (set-slot! :transit-bodies
            (append
             (list (list :prefix "planet"
                         :texture (world-figure to-world :texture)
                         :radius (world-figure to-world :radius)
                         :targets dests
                         :diffuse (world-figure to-world :diffuse)
                         :emissive (world-figure to-world :emissive)
                         :tilt (world-figure to-world :tilt)
                         :adornment (world-figure to-world :adornment)
                         :spin-phase (world-phase-rad to-world t0)
                         :spin-rate (world-spin-rate to-world)
                         ;; no visible size at au range anyway; the
                         ;; first key sets him true -- the same guard
                         ;; the moon road gives its hidden moon
                         :scale-override 0.001)
                   (list :prefix "astern"
                         :texture (world-figure from-world :texture)
                         :radius (world-figure from-world :radius)
                         :targets asterns
                         :spin-phase (world-phase-rad from-world t0)
                         :spin-rate (world-spin-rate from-world)
                         :diffuse (world-figure from-world :diffuse)
                         :emissive (world-figure from-world :emissive)
                         :tilt (world-figure from-world :tilt)
                         :adornment (world-figure from-world :adornment)))
             ;; the moon rides along only when home is the world
             ;; falling astern
             (when (eql from-world :home)
               (list (list :prefix "moon" :texture "moon-tex"
                           :radius +moon-radius+
                           :targets moons
                           :spin-phase (world-phase-rad :moon t0)
                           :spin-rate (world-spin-rate :moon)
                           :diffuse "0.75 0.74 0.70"
                           :emissive "0.10 0.10 0.09")))
             ;; ...and rides out to MEET her when home is the
             ;; road's end, standing at his node off the growing
             ;; world (hidden until the first key, the same guard
             ;; the destination gets)
             (when (eql to-world :home)
               (list (list :prefix "moon" :texture "moon-tex"
                           :radius +moon-radius+
                           :targets (mapcar (lambda (d)
                                              (list (+ (first d) +moon-x+)
                                                    (second d)))
                                            dests)
                           :spin-phase (world-phase-rad :moon t0)
                           :spin-rate (world-spin-rate :moon)
                           :diffuse "0.75 0.74 0.70"
                           :emissive "0.10 0.10 0.09"
                           :scale-override 0.001))))))
      (the (set-slot! :transit-beats
            (list (cons 0.02 (format nil "she waited on the window -- ~a standing ~a ~a"
                                     (world-figure to-world :name) window-phrase
                                     (world-figure from-world :name)))
                  (cons 0.07 (format nil "the escape kick: +~,2f km/s, out of ~a's grip"
                                     dv1 (world-figure from-world :name)))
                  (cons 0.35 (format nil "falling around the sun -- ~a of coast, ~a shrinking astern"
                                     leg-span (world-figure from-world :name)))
                  (cons 0.90 (format nil "the capture kick: +~,2f km/s, onto the ring over ~a"
                                     dv2 (world-figure to-world :name)))
                  (cons 0.97 (format nil "~a fills the glass" (world-figure to-world :name))))))
      (the (set-slot! :last-move-note
            (format nil "the sun's road: she waited on the window -- ~a standing ~a ~a -- then a kick out of ~a's grip (+~,2f km/s), ~a falling around the sun, and a capture kick (+~,2f) onto the ring over ~a~a.  ~a turns in the glass; the helm is yours."
                    (world-figure to-world :name) window-phrase
                    (world-figure from-world :name)
                    (world-figure from-world :name) dv1
                    leg-span
                    dv2 (world-figure to-world :name)
                    (if (> dv2 5)
                        " -- the deeper the well, the dearer the stop at the bottom"
                        "")
                    (if (eql to-world :home) "Home" "A new world"))))
      (the voyage-control (set-slot! :value :hand))
      (tally! :moves)
      (tally! :voyages)
      (tally! (intern (concatenate 'string (symbol-name to-world) "-VOYAGES")
                      :keyword))))

   ;; a move spends game-time: the ship's clock runs by that much
   ;; and no more -- between moves it is stopped -- and every world
   ;; turns by what his day makes of it.  SECS may be negative on a
   ;; rewind, unwinding the turn with the fall.
   (advance-clock!
    (secs)
    (the (set-slot! :game-seconds (+ (the game-seconds) secs))))

   (make-helm-move!
    ()
    (the (set-slot! :transit-samples nil))
    (the (set-slot! :transit-target nil))
    (the (set-slot! :transit-bodies nil))
    (the (set-slot! :transit-beats nil))
    (the (set-slot! :transit-time-map nil))
    (if (the landed?)
        (the make-parked-move!)
        (let* ((turn (ecase (the wheel-control value)
                       (:hard-port 30) (:easy-port 10) (:amidships 0)
                       (:easy-starboard -10) (:hard-starboard -30)))
               (shifter (the shifter-control value))
               (pedal (the pedal-control value))
               (rewind? (eql (the transport-control value) :rewind))
               (heading (mod (+ (the heading-deg) turn) 360))
               (rad (deg->rad heading))
               (flip (if (eql shifter :reverse) -1 1))
               ;; the full pedal is a real kick; feather is the
               ;; light foot close work needs; neutral sends no
               ;; pedal to the engines, and they don't light in
               ;; rewind -- the tape only carries the fall
               (dv (if (or rewind? (eql shifter :neutral))
                       0
                       (case pedal (:burn 0.5) (:feather 0.1) (t 0))))
               ;; the scope of a move: a minute of close work up to
               ;; a full day on the fastest channel -- Space
               ;; Travel's clock, worn as the dash radio, and the
               ;; tape transport signs it
               (dt (* (if rewind? -1 1) (the cadence-seconds)))
               (state (the (fall (+ (the vel-x) (* dv flip (cos rad)))
                                 (+ (the vel-y) (* dv flip (sin rad)))
                                 (the pos-x) (the pos-y) dt)))
               (r (sqrt (+ (* (third state) (third state))
                           (* (fourth state) (fourth state)))))
               (contact? (< r (the world-sky)))
               ;; the substep ends below the sky, having gained speed
               ;; the true touch never had; energy walks the end state
               ;; back up to the sky exactly
               (contact-speed
                (let ((v2 (+ (* (first state) (first state))
                             (* (second state) (second state)))))
                  (if contact?
                      (sqrt (max 0.0 (- v2 (* 2 (the world-mu)
                                              (- (/ 1 (max r 1.0))
                                                 (/ 1 (the world-sky)))))))
                      (sqrt v2))))
               ;; meet the sky under the cap and you are DOWN;
               ;; over it and the surface sheds the speed for you
               (down? (and contact? (< contact-speed +landing-speed-cap+)))
               (crashed? (and contact? (not down?)))
               ;; how the burn lay against the road: along it, against
               ;; it, or a sideways shove
               (v0 (sqrt (+ (* (the vel-x) (the vel-x))
                            (* (the vel-y) (the vel-y)))))
               (alignment (if (or (zerop dv) (< v0 0.1))
                              0
                              (/ (+ (* dv flip (cos rad) (the vel-x))
                                    (* dv flip (sin rad) (the vel-y)))
                                 (* dv v0))))
               (note (cond (down?
                            (format nil "DOWN on ~a -- ~,2f km/s at the touch, under the ~,1f the surface forgives.  The world turns under him; forward and the full pedal to climb back to the ring"
                                    (the world-name) contact-speed
                                    +landing-speed-cap+))
                           (crashed?
                            (format nil "the world came up to meet you at ~,1f km/s -- back on the ring, falling clean.  Under ~,1f km/s at the touch would have been a landing"
                                    contact-speed +landing-speed-cap+))
                           (rewind?
                            "the tape runs backward — the road unfalls behind her; the engines don't light in rewind")
                           ((eql pedal :brake)
                            "the brake presses beautifully and does nothing — space doesn't brake")
                           ((and (plusp dv) (> alignment 0.5))
                            "burn along the road — more speed, and the far side of the orbit rises")
                           ((and (plusp dv) (< alignment -0.5))
                            "burn against the road — less speed, and the far side falls")
                           ((plusp dv)
                            "a sideways shove — the road tilts; speed hardly changes")
                           (t "coasting in neutral, falling around the world; that curve IS the orbit"))))
          (cond (down?
                 ;; set down at the point of contact, engines cold
                 (let ((scale (/ (the world-sky) (max r 1.0))))
                   (the (set-slot! :pos-x (* (third state) scale)))
                   (the (set-slot! :pos-y (* (fourth state) scale))))
                 (the (set-slot! :vel-x 0))
                 (the (set-slot! :vel-y 0))
                 (the (set-slot! :heading-deg heading))
                 (the (set-slot! :landed? t)))
                (crashed?
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
          (the (set-slot! :last-burn (cond ((or contact? (zerop dv)) :none)
                                           ((eql shifter :reverse) :retro)
                                           (t :forward))))
          (the (set-slot! :moves-count (1+ (the moves-count))))
          (the (advance-clock! dt))
          (the (set-slot! :last-move-note note))
          (tally! :moves)
          (tally! (cond (rewind? :rewinds)
                        ((eql pedal :brake) :brakes)
                        ((zerop dv) :coasts)
                        ((eql pedal :burn) :burns)
                        (t :feathers)))
          (when crashed? (tally! :crashes))
          (when down?
            (tally! :landings)
            (tally! (intern (concatenate 'string (symbol-name (the world))
                                         "-LANDINGS")
                            :keyword)))
          (unless contact? (the maybe-hand-off!)))))

   ;; Thompson's rule, one world at a time: whoever pulls hardest on
   ;; her owns her.  Checked at the end of every hand-flown move,
   ;; with a margin so the boundary between grips does not flap.
   ;; The moon stands at his node (no galaxy clock yet), so home's
   ;; frame and his differ by a pure translation and the velocity
   ;; crosses unchanged.
   (maybe-hand-off!
    ()
    (cond ((eql (the world) :home)
           (let* ((mx (- (the pos-x) +moon-x+))
                  (my (the pos-y))
                  (pull-moon (/ +mu-moon+ (+ (* mx mx) (* my my))))
                  (pull-home (/ +mu+ (* (the radius) (the radius)))))
             (when (> pull-moon (* 1.15 pull-home))
               (the (set-slot! :world :moon))
               (the (set-slot! :pos-x mx))
               (the (set-slot! :last-move-note
                     (concatenate 'string (the last-move-note)
                                  " — and the moon's grip takes her: falling around the moon now")))
               (tally! :handoffs)
               (tally! :handoffs-moonward))))
          ((eql (the world) :moon)
           (let* ((hx (+ (the pos-x) +moon-x+))
                  (hy (the pos-y))
                  (pull-home (/ +mu+ (+ (* hx hx) (* hy hy))))
                  (pull-moon (/ +mu-moon+ (* (the radius) (the radius)))))
             (when (> pull-home (* 1.15 pull-moon))
               (the (set-slot! :world :home))
               (the (set-slot! :pos-x hx))
               (the (set-slot! :last-move-note
                     (concatenate 'string (the last-move-note)
                                  " — and home reclaims her: back in the big well")))
               (tally! :handoffs)
               (tally! :handoffs-homeward))))
          (t nil)))

   ;; A move made on the ground.  Forward gear and the full pedal
   ;; light the engines for the programmed climb back to the
   ;; world's ring -- the LANDING is where the skill lives, the
   ;; ascent is the yard's gift.  Anything else and he sits, the
   ;; world turning under him.
   (make-parked-move!
    ()
    (let ((shifter (the shifter-control value))
          (pedal (the pedal-control value))
          (rewind? (eql (the transport-control value) :rewind)))
      (cond ((and (not rewind?) (eql shifter :forward) (eql pedal :burn))
             ;; the gas-pedal climb flies the same lift-off clip a
             ;; bought road does (it counts its own move)
             (the fly-climb-road!))
            (t
             (the (set-slot! :last-burn :none))
             (the (set-slot! :last-move-note
                             (cond (rewind?
                                    "the tape runs backward and the world turns the other way under him -- there is no road into the past off this rock")
                                   ((eql pedal :brake)
                                    "the brake presses beautifully, and he is already stopped")
                                   ((eql shifter :reverse)
                                    "reverse on the ground just squats her on her gear -- forward is the way off a world")
                                   ((eql shifter :neutral)
                                    (if (eql pedal :burn)
                                        "revving in neutral -- the engines answer and nothing moves.  Forward, and the full pedal climbs"
                                        (format nil "down on ~a, engines cold -- the world turns under him.  Forward and the full pedal to climb"
                                                (the world-name))))
                                   ((eql pedal :feather)
                                    "a breath of gas stirs the dust -- it takes the full pedal to climb")
                                   (t (format nil "down on ~a, engines cold -- the world turns under him.  Forward and the full pedal to climb"
                                              (the world-name))))))
             (the (set-slot! :moves-count (1+ (the moves-count))))
             ;; sitting on a world still spends the turn: the clock
             ;; runs by the cadence, and the world turns under him
             (the (advance-clock! (* (if rewind? -1 1) (the cadence-seconds))))
             (tally! :moves)))))))

;; ============================================================
;; THE X_ITE SCOUT: the same cockpit under the other X3D browser.
;; X3D is the standard (ISO/IEC 19775, Web3D Consortium); x3dom and
;; X_ITE are two independent implementations of it, and this page is
;; the second-renderer proof: the scene rides as a standalone X3D
;; document (self-igniting voyage clock, no x3dom extensions -- the
;; RenderedTexture eye feeds stay dark, a measured debt), the helm
;; card keeps the game loop honest, and X_ITE's own WebXR button
;; stands ready on the canvas.  If the pointing sensors behave here,
;; the x3dom flakiness is theirs, not ours.
;; ============================================================

;; The SAI wiring: X_ITE speaks the standard Scene Access Interface
;; (named nodes, field callbacks) where x3dom speaks DOM events.
;; Everything lands on the same helm-card controls, so the game
;; step is identical.  All of it best-effort and guarded: a miss
;; here leaves the card fully in command.
(defparameter *xr-sai-js* "
(function () {
  function findSel (opt) {
    var sels = document.querySelectorAll('#helm-body select');
    for (var i = 0; i < sels.length; i++)
      if (sels[i].querySelector('option[value=\"' + opt + '\"]')) return sels[i];
    return null;
  }
  function faceClick (cls, val) {
    var btns = document.querySelectorAll(cls);
    for (var i = 0; i < btns.length; i++)
      if (btns[i].getAttribute('data-val') === val) { btns[i].click(); return; }
  }
  function bandFor (ang) {
    if (ang > 0.9) return ':HARD-PORT';
    if (ang > 0.22) return ':EASY-PORT';
    if (ang < -0.9) return ':HARD-STARBOARD';
    if (ang < -0.22) return ':EASY-STARBOARD';
    return ':AMIDSHIPS';
  }
  var gearCycle = [':FORWARD', ':NEUTRAL', ':REVERSE'];
  function wire (browser) {
    var scene = browser.currentScene;
    // the floodlight stands as remembered: a fresh scene document
    // always arrives with it off
    try { if (window.GW_FLOOD_APPLY) GW_FLOOD_APPLY(browser); } catch (e) {}
    function named (n) { try { return scene.getNamedNode(n); } catch (e) { return null; } }
    function onRelease (def, fn) {
      var node = named(def); if (!node) return;
      try {
        node.getField('isActive').addFieldCallback('gw-' + def, function (v) {
          if (v === false || v === 'false') fn();
        });
      } catch (e) {}
    }
    var wheelSel = findSel(':AMIDSHIPS'), gearSel = findSel(':NEUTRAL'),
        pedalSel = findSel(':BURN'), roadSel = findSel(':HAND');
    // the same lockout truth GW_WIRE computes; sensors here are
    // scene-document nodes, so the greys travel by SAI instead of
    // setAttribute
    function locks () {
      try { return window.GW_LOCKS ? GW_LOCKS() : {}; } catch (e) { return {}; }
    }
    function changed (sel) {
      try { sel.dispatchEvent(new Event('change')); } catch (e) {}
    }
    onRelease('shifter-touch', function () {
      if (!gearSel || locks().gear) return;
      gearSel.value = gearCycle[(gearCycle.indexOf(gearSel.value) + 1) % gearCycle.length];
      changed(gearSel);
    });
    [':FEATHER', ':BRAKE', ':BURN'].forEach(function (v, i) {
      onRelease('pedal-touch-' + i, function () {
        if (locks().pedal) return;
        if (pedalSel) pedalSel.value = v;
      });
    });
    onRelease('horn-touch', function () {
      if (locks().wheel) return;
      if (wheelSel) { wheelSel.value = ':AMIDSHIPS'; changed(wheelSel); }
    });
    [':SLOW', ':MEDIUM', ':FAST', ':FASTEST'].forEach(function (v, i) {
      onRelease('radio-preset-' + i + '-touch', function () { faceClick('.cadence-btn', v); });
    });
    [':REWIND', ':PLAY'].forEach(function (v, i) {
      onRelease('radio-tpt-' + i + '-touch', function () { faceClick('.transport-btn', v); });
    });
    onRelease('starter-touch', function () {
      var b = document.getElementById('gw-move-btn'); if (b) b.click();
    });
    var wheel = named('wheel-sensor');
    if (wheel) {
      var lastAng = 0;
      try {
        wheel.getField('rotation_changed').addFieldCallback('gw-rot', function (r) {
          try {
            var a = (typeof r.angle === 'number') ? r.angle : 0;
            var y = (typeof r.y === 'number') ? r.y : 1;
            lastAng = (y < 0 ? -1 : 1) * a;
            while (lastAng > Math.PI) lastAng -= 2 * Math.PI;
            while (lastAng < -Math.PI) lastAng += 2 * Math.PI;
          } catch (e) {}
        });
        wheel.getField('isActive').addFieldCallback('gw-wact', function (v) {
          if ((v === false || v === 'false') && wheelSel && !locks().wheel) {
            wheelSel.value = bandFor(lastAng);
            changed(wheelSel);
          }
        });
      } catch (e) {}
    }
    // colors travel by SAI here: the scene is a document, not
    // page DOM, so setAttribute reaches nothing
    function setColor (node, fieldName, str) {
      var p = str.split(' ').map(parseFloat);
      try { node.getField(fieldName).setValue(new X3D.SFColor(p[0], p[1], p[2])); return; } catch (e) {}
      try { node[fieldName] = new X3D.SFColor(p[0], p[1], p[2]); } catch (e) {}
    }
    // the helm wears its own state: knob color by gear, lever
    // pose, the steerage lamp for the active band, the radio keys
    function indicators () {
      var kc = { ':FORWARD': '0.30 0.75 0.35', ':NEUTRAL': '0.85 0.87 0.88',
                 ':REVERSE': '0.91 0.60 0.18' };
      var km = named('shifter-knob-mat');
      if (km && gearSel) setColor(km, 'diffuseColor', kc[gearSel.value] || '0.85 0.87 0.88');
      var rig = named('shifter-rig');
      var pose = { ':FORWARD': -0.5, ':NEUTRAL': 0, ':REVERSE': 0.5 };
      if (rig && gearSel) {
        try {
          var rf = rig.getField('rotation');
          var rv = rf.getValue();
          rv.angle = pose[gearSel.value] || 0;
          rf.setValue(rv);
        } catch (e) {}
      }
      var order = [':HARD-PORT', ':EASY-PORT', ':AMIDSHIPS',
                   ':EASY-STARBOARD', ':HARD-STARBOARD'];
      var idx = wheelSel ? order.indexOf(wheelSel.value) : 2;
      for (var i = 0; i < 5; i++) {
        var lm = named('steer-lamp-mat-' + i);
        if (!lm) continue;
        setColor(lm, 'diffuseColor', i === idx ? '0.91 0.78 0.22' : '0.15 0.16 0.18');
        setColor(lm, 'emissiveColor', i === idx ? '0.55 0.45 0.10' : '0.04 0.04 0.05');
      }
      function litSet (prefix, vals, cls) {
        var b = document.querySelector(cls + '.lit');
        var active = b ? b.getAttribute('data-val') : null;
        vals.forEach(function (v, i) {
          var m = named(prefix + '-mat-' + i);
          if (!m) return;
          var lit = v === active;
          setColor(m, 'diffuseColor', lit ? '0.91 0.78 0.22' : '0.13 0.15 0.17');
          setColor(m, 'emissiveColor', lit ? '0.55 0.45 0.10' : '0.05 0.06 0.07');
        });
      }
      litSet('radio-preset', [':SLOW', ':MEDIUM', ':FAST', ':FASTEST'], '.cadence-btn');
      litSet('radio-tpt', [':REWIND', ':PLAY'], '.transport-btn');
      // the greys reach the scene's own sensors: a locked control
      // refuses the grab at the sensor, both renderers agreeing
      var L = locks();
      function en (def, on) {
        var n = named(def); if (!n) return;
        try { n.getField('enabled').setValue(!!on); } catch (e) {
          try { n.enabled = !!on; } catch (e2) {}
        }
      }
      en('wheel-sensor', !L.wheel);
      en('horn-touch', !L.wheel);
      en('shifter-touch', !L.gear);
      en('pedal-touch-0', !L.pedal);
      en('pedal-touch-1', !L.pedal);
      en('pedal-touch-2', !L.pedal);
    }
    if (gearSel) gearSel.addEventListener('change', indicators);
    if (wheelSel) wheelSel.addEventListener('change', indicators);
    if (roadSel) roadSel.addEventListener('change', indicators);
    Array.prototype.forEach.call(
      document.querySelectorAll('.cadence-btn, .transport-btn'),
      function (b) {
        b.addEventListener('click', function () { setTimeout(indicators, 0); });
      });
    indicators();
    // the painted faces travel by SAI too: the paint shop stashes
    // every data url on GW_TEX, and the DEF'd ImageTextures drink
    // from it -- continents and craters under the second renderer
    if (window.GW_TEX) {
      Object.keys(window.GW_TEX).forEach(function (id) {
        var tx = named(id);
        if (!tx) return;
        try { tx.getField('url').setValue(new X3D.MFString(window.GW_TEX[id])); } catch (e) {
          try { tx.url = new X3D.MFString(window.GW_TEX[id]); } catch (e2) {}
        }
      });
    }
    // X_ITE MODULATES texture by diffuse where x3dom replaced
    // it: once a body's face lands, its material goes white so
    // the continents read true
    ['planet', 'astern', 'moon', 'home-far'].forEach(function (p) {
      var m = named(p + '-mat');
      if (m) {
        setColor(m, 'diffuseColor', '1 1 1');
        setColor(m, 'emissiveColor', '0.18 0.18 0.18');
      }
    });
    // the voyage clock re-arms on the CLIENT's own time: the baked
    // startTime rode the server's clock, and skew or load latency
    // left the road standing still
    var vc = named('voyage-clock');
    if (vc) {
      var t0 = Date.now() / 1000 + 1.0;
      try { vc.getField('startTime').setValue(t0); } catch (e) {
        try { vc.startTime = t0; } catch (e2) {}
      }
      window.GW_VOYAGE_T0 = t0 * 1000;
    }
    var note = document.getElementById('plot-frame-label');
    if (note) note.textContent = 'X_ITE: wired';
    // the twist itself: X_ITE's examine viewer STRAIGHTENS the
    // horizon to +Y, discarding the roll our z-up viewpoints
    // carry.  Turn that off before binding.
    try { browser.setBrowserOption('StraightenHorizon', false); } catch (e) {}
    // the early bind can be overridden by X_ITE's own initial
    // bind; assert the seat once more now that all stands
    bindSeat(browser);
    // ...and X_ITE's own initial bind can land later still, when
    // the full scene stands ready -- so the seat is asserted again
    // on the browser's INITIALIZED event and on a short ladder of
    // timeouts, whichever fires last
    try {
      browser.addBrowserCallback('gw-bind', X3D.X3DConstants.INITIALIZED_EVENT,
        function () { setTimeout(function () { bindSeat(browser); }, 100); });
    } catch (e) {}
    [1500, 4000, 9000].forEach(function (ms) {
      setTimeout(function () { bindSeat(browser); }, ms);
    });
  }
  // bind the driver's seat the moment it is parseable -- with
  // TELEPORT transitions this is a snap, not a tour
  function bindSeat (browser) {
    var note = document.getElementById('plot-frame-label');
    var vp = null;
    try { vp = browser.currentScene && browser.currentScene.getNamedNode('drivers-seat'); } catch (e) {}
    if (!vp) { if (note) note.textContent = 'bind: no node yet'; return false; }
    var how = 'fail';
    // an early bind against the half-initialized scene leaves the
    // node MARKED bound while the finished scene renders its own
    // default camera -- and set_bind TRUE on a bound node is a
    // no-op by spec.  Toggling FALSE first clears the stale entry.
    try {
      var bf = vp.getField('set_bind');
      bf.setValue(false);
      bf.setValue(true);
      how = 'field';
    } catch (e) {
      try { vp.set_bind = false; vp.set_bind = true; how = 'prop'; } catch (e2) {}
    }
    var cur = '';
    try { cur = (browser.activeViewpoint && browser.activeViewpoint.description) || ''; } catch (e) {}
    if (note) note.textContent = 'bind:' + how + ' t=' + Math.round(performance.now() / 100) / 10 + ' cur=' + cur;
    return how !== 'fail';
  }
  function start () {
    var canvas = document.querySelector('x3d-canvas');
    if (!canvas) return;
    var browser = canvas.browser;
    if (!browser) { setTimeout(start, 250); return; }
    // the worlds are unit spheres scaled enormous, and X_ITE's
    // default MEDIUM primitive quality shows its polygons on the
    // limb where x3dom cut finer.  Set quality before the scene
    // builds so the first tessellation is the good one.
    try { browser.setBrowserOption('PrimitiveQuality', 'HIGH'); } catch (e) {}
    try { browser.setBrowserOption('TextureQuality', 'HIGH'); } catch (e) {}
    // NO early bind: asserting against the half-initialized
    // scene poisons the bind stack (see bindSeat)
    var tries = 0;
    (function poll () {
      tries++;
      var ok = false;
      try {
        ok = browser.currentScene &&
             browser.currentScene.getNamedNode('shifter-touch');
        // after a move, only the freshly generated scene will do:
        // the canvas holds the OLD scene until the new document
        // arrives and parses, and wiring that one replays the past
        if (ok && window.GW_XR_EXPECT != null)
          ok = browser.currentScene.getNamedNode('gw-gen-' + window.GW_XR_EXPECT);
      } catch (e) { ok = false; }
      if (ok) { try { wire(browser); } catch (e) {} return; }
      if (tries < 120) setTimeout(poll, 500);
    })();
  }
  window.GW_XR_WIRE = start;
  window.addEventListener('load', function () { setTimeout(start, 100); });
})();")

;; Strip one attribute wherever it appears -- the x3dom-only
;; Viewpoint fields (zNear/zFar) make X_ITE's stricter parser drop
;; the whole node, and a scene with no surviving Viewpoint falls to
;; the default camera.
(defun strip-attr (markup attr)
  (let ((needle (format nil " ~a=\"" attr)))
    (loop for start = (search needle markup)
          while start
          do (let ((close (position #\" markup
                                    :start (+ start (length needle)))))
               (setq markup (concatenate 'string
                                         (subseq markup 0 start)
                                         (subseq markup (1+ close))))))
    markup))

(define-object cockpit-xr-view (cockpit-view)

  :computed-slots
  ((title "Galaxy World — the cockpit")
   ;; the xr renderer takes a new scene by re-pointing the canvas
   ;; at the session's document (cache-busted) and re-running the
   ;; SAI wiring, which re-arms the voyage clock, re-binds the
   ;; seat, and relights the indicators
   (scene-refresh-js
    (format nil "window.GW_XR_EXPECT = '~a';~%var cv = document.querySelector('x3d-canvas');~%if (cv) cv.setAttribute('src', '/xr-scene.x3d?ship=~a&bust=' + Date.now());~%if (window.GW_XR_WIRE) setTimeout(window.GW_XR_WIRE, 400);"
            (the moves-count) (the instance-id)))
   (xr-scene? t)
   (use-x3dom? nil)
   (additional-header-content
    ;; the console tap rides ahead of X_ITE: scene-load complaints
    ;; land on the plot's frame label, so a headless webshot can
    ;; read the browser's mind
    "<script>(function () { var orig = console.error; window.GW_ERRS = []; console.error = function () { try { var s = Array.prototype.join.call(arguments, ' '); GW_ERRS.push(s); var el = document.getElementById('plot-frame-label'); if (el) el.textContent = ('ERR ' + s).slice(0, 220); } catch (e) {} orig.apply(console, arguments); }; })();</script><script defer src=\"https://cdn.jsdelivr.net/npm/x_ite@16.2.0/dist/x_ite.min.js\"></script>")

   ;; the whole scene as one standing X3D document, served beside
   ;; the page (see xr-scene-responder): every piece the x3dom page
   ;; carries except the RenderedTexture eye feeds
   (xr-scene-document
    (string-append
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?><X3D profile=\"Immersive\" version=\"4.0\"><Scene>"
     "<Background skyColor=\"0 0 0.012\"></Background>"
     ;; the generation stamp: the page's re-wire after a move
     ;; waits for THIS scene, not whichever scene the canvas still
     ;; holds -- production latency taught that lesson
     (format nil "<WorldInfo DEF=\"gw-gen-~a\" title=\"gw-gen-~a\"></WorldInfo>" (the moves-count) (the moves-count))
     ;; transitionTime 0: binds snap instead of touring (the
     ;; visible tumble at load was the transition animation).  NOT
     ;; transitionType TELEPORT -- under X_ITE that mode reported
     ;; the bind and never moved the camera (benchmark datum #3).
     ;; headlight OFF: the sun lights the worlds (sun-light-x3d)
     "<NavigationInfo headlight=\"false\" transitionTime=\"0\"></NavigationInfo>"
     ;; NO y-up wrapper: X_ITE binds a Transform-wrapped Viewpoint
     ;; without applying the parent transform (benchmark datum #2),
     ;; so the world stays z-up here and the y-up question waits
     ;; for authored-in-y-up viewpoints on the WebXR slice
     (strip-attr (strip-attr (the viewpoints-x3d) "zNear") "zFar")
     (format nil "<Transform DEF=\"sky-heading\" rotation=\"0 0 1 ~,5f\">~a<Transform DEF=\"sky-drift\">~a</Transform></Transform>"
             (- (the sky-authored-heading-rad))
             (sun-light-x3d :elevation (the sun-elevation))
             (starfield-x3d :radius 5000.0d0))
     (the bodies-x3d)
     ;; the cab under its own lamp (see cab-light-x3d)
     "<Group>"
     (cab-light-x3d)
     (cockpit-x3d)
     (dice-x3d (the dice-lean))
     (gauge-needle-x3d 0 0.46 (the speedo-phi) 0.055)
     (gauge-needle-x3d 0.19 0.45 (the heading-deg) 0.042)
     (gauge-needle-x3d -0.19 0.45 (the vario-phi) 0.042)
     (dash-radio-x3d (the cadence-control value)
                     (the transport-control value))
     "</Group>"
     "</Scene></X3D>"))

   (body
    (with-lhtml-string ()
      (:div :style "position:fixed;inset:0;background:#000;"
        ;; the param is "ship", NOT "iid": the transporter's
        ;; gwl-query-affinity module claims any iid-bearing query
        ;; and blackholes the ones without an affinity record --
        ;; the pt-20 gotcha, met again in the wild
        ;; splashScreen off: every move re-renders the page, and
        ;; the X_ITE splash flashing between turns broke the spell
        ;; -- dark glass until the scene stands is the better wait
        (:|x3d-canvas|
          :src (format nil "/xr-scene.x3d?ship=~a" (the instance-id))
          :|splashScreen| "false"
          :style "width:100%;height:100%;display:block;background:#000;"))
      (:style (str "
#helm-body select { background:rgba(16,16,16,0.6); color:#e8c839; border:1px solid #7a6a1f; border-radius:6px; padding:2px 4px; font-size:12px; }
#helm-body select option { background:#1a1a1a; color:#e8c839; }
#helm-body .rbtn { background:#141414; color:#e8c839; border:1px solid #7a6a1f; border-radius:4px; padding:2px 7px; font-size:11px; cursor:pointer; font-family:inherit; }
#helm-body .rbtn.lit { background:#e8c839; color:#141414; border-color:#e8c839; }"))
      (:div :style "position:fixed;bottom:14px;right:14px;z-index:10;background:rgba(16,16,16,0.45);border:1px solid #e8c839;border-radius:10px;padding:10px 16px;font-family:sans-serif;color:#e8c839;font-size:13px;min-width:250px;max-width:430px;"
        (str (the helm-section main-div)))
      (:div :id "plot-card" :style "position:fixed;bottom:40px;left:14px;z-index:10;background:rgba(16,16,16,0.45);border:1px solid #e8c839;border-radius:10px;padding:8px 10px;font-family:sans-serif;color:#e8c839;"
        (:div :style "font-size:12px;letter-spacing:0.06em;cursor:pointer;display:flex;justify-content:space-between;align-items:center;gap:10px;"
              :onclick "togglePlot()"
          (:span "THE PLOT")
          (:span :id "plot-caret" "▾"))
        (str (the plot-section main-div)))
      (:div :style "position:fixed;bottom:12px;left:14px;z-index:10;color:#c9a227;font-family:sans-serif;font-size:13px;opacity:0.85;"
        "Galaxy World — the cockpit"
        (:a :href "/x3dom" :style "color:#e8c839;margin-left:10px;" "x3dom build")
        (:a :href "/bridge" :style "color:#e8c839;margin-left:10px;" "⊙ to the bridge"))
      (:script
        (str "
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
})();")
        (str *paint-shop-js*)
        (str *plan-view-js*)
        (str *helm-hands-js*)
        (str *voyage-beats-js*)
        ;; the definitions just landed; give the section's state
        ;; script its first real run (the xr wiring arms itself on
        ;; window load)
        (str "if (window.GW_WIRE) GW_WIRE(); if (window.GW_DRAW) GW_DRAW(); if (window.GW_BEATS) GW_BEATS();")
        (str *xr-sai-js*))))))
