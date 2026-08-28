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

(defparameter +wheel-rake+ (deg->rad 38))

(define-object cockpit (base-object)

  :computed-slots
  (;; the column's axis, pointing up and back toward the driver
   (column-axis (let ((theta +wheel-rake+))
                  (make-vector (- (cos theta)) 0 (sin theta))))
   (wheel-center (make-point 0.42 0 0.58))
   ;; in-plane frame of the wheel: across, and up-forward
   (wheel-across (make-vector 0 1 0))
   (wheel-up (cross-vectors (the wheel-across) (the column-axis)))
   (wheel-radius 0.20)
   (shifter-pivot (add-vectors (the wheel-center)
                               (scalar*vector -0.14 (the column-axis)))))

  :functions
  ((rim-point
    (alpha)  ; angle from wheel-up, radians
    (add-vectors (the wheel-center)
                 (add-vectors
                  (scalar*vector (* (the wheel-radius) (cos alpha)) (the wheel-up))
                  (scalar*vector (* (the wheel-radius) (sin alpha)) (the wheel-across))))))

  :objects
  (;; the shell of the room: floor, bench, instrument panel
   (floor-pan :type 'box
              :center (make-point 0.30 -0.36 -0.37)
              :width 1.6 :length 1.5 :height 0.04
              :display-controls (list :color +rubber+))

   (bench-cushion :type 'box
                  :center (make-point -0.03 -0.36 -0.075)
                  :width 0.55 :length 1.42 :height 0.15
                  :display-controls (list :color +leather+))

   (bench-back :type 'box
               :center (make-point -0.42 -0.36 0.22)
               :width 0.12 :length 1.42 :height 0.60
               :display-controls (list :color +leather+))

   (instrument-panel :type 'box
                     :center (make-point 0.88 -0.36 0.70)
                     :width 0.12 :length 1.42 :height 0.30
                     :display-controls (list :color +paint+))

   ;; the gauge cluster, chrome bezels proud of the panel face
   (speedo-bezel :type 'c-cylinder
                 :start (make-point 0.812 0 0.71)
                 :end (make-point 0.822 0 0.71)
                 :radius 0.085
                 :number-of-sections 24
                 :display-controls (list :color +chrome+))
   (speedo-face :type 'c-cylinder
                :start (make-point 0.806 0 0.71)
                :end (make-point 0.813 0 0.71)
                :radius 0.075
                :number-of-sections 24
                :display-controls (list :color +gauge-face+))
   (speedo-needle :type 'c-cylinder
                  :start (make-point 0.804 0 0.712)
                  :end (make-point 0.804 -0.035 0.765)
                  :radius 0.003
                  :display-controls (list :color +needle+))

   (port-gauge-bezel :type 'c-cylinder
                     :start (make-point 0.812 0.19 0.70)
                     :end (make-point 0.822 0.19 0.70)
                     :radius 0.055
                     :number-of-sections 24
                     :display-controls (list :color +chrome+))
   (port-gauge-face :type 'c-cylinder
                    :start (make-point 0.807 0.19 0.70)
                    :end (make-point 0.813 0.19 0.70)
                    :radius 0.047
                    :number-of-sections 24
                    :display-controls (list :color +gauge-face+))
   (port-gauge-needle :type 'c-cylinder
                      :start (make-point 0.805 0.19 0.702)
                      :end (make-point 0.805 0.162 0.736)
                      :radius 0.0025
                      :display-controls (list :color +needle+))

   (starboard-gauge-bezel :type 'c-cylinder
                          :start (make-point 0.812 -0.19 0.70)
                          :end (make-point 0.822 -0.19 0.70)
                          :radius 0.055
                     :number-of-sections 24
                          :display-controls (list :color +chrome+))
   (starboard-gauge-face :type 'c-cylinder
                         :start (make-point 0.807 -0.19 0.70)
                         :end (make-point 0.813 -0.19 0.70)
                         :radius 0.047
                    :number-of-sections 24
                         :display-controls (list :color +gauge-face+))
   (starboard-gauge-needle :type 'c-cylinder
                           :start (make-point 0.805 -0.19 0.702)
                           :end (make-point 0.805 -0.218 0.736)
                           :radius 0.0025
                           :display-controls (list :color +needle+))

   ;; the HELM: big thin-rim wheel on a raked column
   (wheel-rim :type 'torus
              :center (the wheel-center)
              :orientation (alignment :top (the column-axis))
              :major-radius (the wheel-radius)
              :minor-radius 0.011
              :number-of-longitudinal-sections 48
              :number-of-transverse-sections 16
              :display-controls (list :color +rim-ivory+))

   (wheel-hub :type 'c-cylinder
              :start (add-vectors (the wheel-center)
                                  (scalar*vector -0.03 (the column-axis)))
              :end (add-vectors (the wheel-center)
                               (scalar*vector 0.03 (the column-axis)))
              :radius 0.04
              :number-of-sections 24
              :display-controls (list :color +chrome+))

   (horn-button :type 'sphere
                :center (add-vectors (the wheel-center)
                                     (scalar*vector 0.035 (the column-axis)))
                :radius 0.025
                :display-controls (list :color +chrome+))

   ;; classic three-spoke: two high, one straight down
   (spokes :type 'c-cylinder
           :sequence (:size 3)
           :start (the wheel-center)
           :end (the (rim-point (ecase (the-child index)
                                  (0 (deg->rad 60))
                                  (1 (deg->rad -60))
                                  (2 pi))))
           :radius 0.008
           :display-controls (list :color +chrome+))

   (column :type 'c-cylinder
           :start (add-vectors (the wheel-center)
                               (scalar*vector -0.02 (the column-axis)))
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
                 :display-controls (list :color +chrome+))))

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
   (favicon-path "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'%3E%3Ccircle cx='32' cy='32' r='31' fill='%23000003'/%3E%3Ccircle cx='32' cy='32' r='24' fill='%23e8c839' stroke='%237a6a1f' stroke-width='2'/%3E%3Cellipse cx='32' cy='32' rx='7' ry='19' fill='%23050505'/%3E%3Ccircle cx='25' cy='23' r='4.5' fill='%23fff8d8' opacity='.75'/%3E%3C/svg%3E"))

  :computed-slots
  ((viewpoints-x3d
    (let ((up (make-vector 0 0 1)))
      (string-append
       (viewpoint-x3d "drivers-seat" "Driver's seat"
                      (make-point -0.02 0 0.70)
                      (make-vector 0.965 0 -0.263)
                      "1.3" :z-near "0.05" :z-far "8000" :up up)
       (viewpoint-x3d "jump-seat" "Jump seat"
                      (make-point 0.05 -0.72 0.62)
                      (unitize-vector (make-vector 0.37 0.72 -0.04))
                      "1.2" :z-near "0.05" :z-far "8000" :up up)
       (viewpoint-x3d "walkaround" "Walkaround"
                      (make-point -1.5 1.1 1.2)
                      (unitize-vector (make-vector 1.9 -1.3 -0.9))
                      "1.0" :z-near "0.05" :z-far "8000" :up up))))

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
        (:button :id "walkaround-btn" :type "button" :onclick "bindEye('walkaround')"
          :style (the eye-button-style) "walkaround"))
      (:div :style "position:fixed;bottom:12px;left:14px;z-index:10;color:#c9a227;font-family:sans-serif;font-size:13px;opacity:0.85;"
        "Galaxy World — the cockpit (first fitting-out)")
      (:script (str "
function bindEye (id) {
  document.getElementById(id).setAttribute('set_bind','true');
}"))))

   (eye-button-style
    "background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:6px 14px;font-size:13px;cursor:pointer;")))
