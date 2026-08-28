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
(defparameter +rim-ivory+ "#f2ead3")    ; the thin-rim wheel
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
    (make-vector (- (sin theta)) (cos theta) 0)))

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

   (instrument-panel :type 'box
                     :center (make-point 0.79 -0.36 0.415)
                     :width 0.12 :length 1.66 :height 0.27
                     :display-controls (list :color +paint+))

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
   (speedo-needle :type 'c-cylinder
                  :start (make-point 0.714 0 0.462)
                  :end (make-point 0.714 -0.035 0.515)
                  :radius 0.003
                  :display-controls (list :color +needle+))

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
   (port-gauge-needle :type 'c-cylinder
                      :start (make-point 0.715 0.19 0.452)
                      :end (make-point 0.715 0.162 0.486)
                      :radius 0.0025
                      :display-controls (list :color +needle+))

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

   ;; the cosmic dice, hanging from the mirror on their cords.  They
   ;; are the ship's free inertial indicator: under thrust they lean
   ;; away from it, and right now they hang perfectly still, plumb to
   ;; the cab -- which is the dice telling you the ship is coasting.
   (dice-cords :type 'c-cylinder
               :sequence (:size 2)
               :start (make-point 0.77
                                  (ecase (the-child index) (0 -0.325) (1 -0.395))
                                  0.91)
               :end (make-point 0.77
                                (ecase (the-child index) (0 -0.325) (1 -0.395))
                                0.85)
               :radius 0.0018
               :display-controls (list :color +chrome+))

   (cosmic-dice :type 'box
                :sequence (:size 2)
                :center (make-point 0.77
                                    (ecase (the-child index) (0 -0.325) (1 -0.395))
                                    0.827)
                :orientation (ecase (the-child index)
                               (0 nil)
                               (1 (alignment :rear (rotate-vector-d
                                                    (make-vector 0 1 0)
                                                    35
                                                    (make-vector 0 0 1)))))
                :width 0.045
                :length 0.045
                :height 0.045
                :display-controls (list :color "#b04040"))

   ;; the HELM: big thin-rim wheel on a raked column
   (wheel-rim :type 'torus
              :center (the wheel-center)
              :orientation (alignment :top (the column-axis))
              :major-radius (the wheel-radius)
              :minor-radius 0.011
              :number-of-longitudinal-sections 48
              :number-of-transverse-sections 16
              :display-controls (list :color +rim-ivory+))

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

   (roof-panel :type 'box
               :center (make-point -0.44 -0.36 1.055)
               :width 2.60
               :length 1.80
               :height 0.03
               :display-controls (list :color +paint+))))

;; The cab is the same for every session, so like the starfield its
;; markup is cut once and shared across all cockpits.
(defvar *cockpit-x3d-cache* nil)

(defun cockpit-x3d ()
  (or *cockpit-x3d-cache*
      (setf *cockpit-x3d-cache*
            (with-output-to-string (s)
              (with-format (geom-base::x3d s)
                (write-the-object (make-object 'cockpit)
                                  (geom-base::cad-output-tree)))))))

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
  ((viewpoints-x3d
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
            (str (starfield-x3d :radius 5000.0d0))
            (str (cockpit-x3d)))))
      (:div :style "position:fixed;top:14px;left:14px;z-index:10;display:flex;gap:10px;font-family:sans-serif;"
        (:button :id "drivers-seat-btn" :type "button" :onclick "bindEye('drivers-seat')"
          :style (the eye-button-style) "driver's seat")
        (:button :id "jump-seat-btn" :type "button" :onclick "bindEye('jump-seat')"
          :style (the eye-button-style) "jump seat")
        (:button :id "back-seat-btn" :type "button" :onclick "bindEye('back-seat')"
          :style (the eye-button-style) "back seat")
        (:button :id "walkaround-btn" :type "button" :onclick "bindEye('walkaround')"
          :style (the eye-button-style) "walkaround")
        (:a :href "/" :style (string-append (the eye-button-style)
                                            "text-decoration:none;display:inline-block;")
          "⊙ to the bridge"))
      (:div :style "position:fixed;bottom:12px;left:14px;z-index:10;color:#c9a227;font-family:sans-serif;font-size:13px;opacity:0.85;"
        "Galaxy World — the cockpit (first fitting-out)")
      (:script (str "
function bindEye (id) {
  document.getElementById(id).setAttribute('set_bind','true');
}"))))

   (eye-button-style
    "background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:6px 14px;font-size:13px;cursor:pointer;")))
