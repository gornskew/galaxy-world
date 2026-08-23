;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

(defun point-string (point)
  (format nil "~,1f ~,1f ~,1f" (get-x point) (get-y point) (get-z point)))

(defun look-orientation (direction)
  "x3d axis-angle string rotating the default gaze (0 0 -1) onto DIRECTION."
  (let* ((d (unitize-vector direction))
         (from (make-vector 0 0 -1))
         (dot (dot-vectors from d)))
    (cond ((> dot 0.99999) "0 1 0 0")
          ((< dot -0.99999) "0 1 0 3.14159")
          (t (let ((axis (unitize-vector (cross-vectors from d)))
                   (angle (acos dot)))
               (format nil "~,5f ~,5f ~,5f ~,5f"
                       (get-x axis) (get-y axis) (get-z axis) angle))))))

;; The view from the bridge: the ship rides above the home planet and
;; you look out the two side eyes. Each eye is a camera; the toggle
;; binds one or the other. No ajax, no session machinery -- the page
;; is view-only by construction.
(define-object bridge-view (base-html-page)

  :input-slots
  ((title "Galaxy World")
   (use-ajax? nil)
   (use-svgpanzoom? nil)
   (use-tailwind? nil)

   ;; Where the ship rides for first light, km, planet at origin.
   (ship-station (make-point 20000 0 5500))
   ;; How far outboard each eye sits from the centerline, km. At
   ;; orbital scale the offset is symbolic, but the eyes are two.
   (eye-offset 40)
   ;; How far outboard of dead-ahead each eye gazes: 0 stares at the
   ;; bow, 1 stares abeam.
   (eye-splay 0.45)
   (eye-field-of-view "1.0"))

  :computed-slots
  ((bow-direction (unitize-vector
                   (subtract-vectors (make-point 0 0 0) (the ship-station))))

   (port-eye-position (add-vectors (the ship-station)
                                   (make-vector 0 (the eye-offset) 0)))
   (starboard-eye-position (add-vectors (the ship-station)
                                        (make-vector 0 (- (the eye-offset)) 0)))

   ;; Outboard-forward: each eye looks ahead and to its own side.
   (port-eye-direction (unitize-vector
                        (add-vectors (the bow-direction)
                                     (make-vector 0 (the eye-splay) 0))))
   (starboard-eye-direction (unitize-vector
                             (add-vectors (the bow-direction)
                                          (make-vector 0 (- (the eye-splay)) 0))))

   (chart-x3d (with-output-to-string (s)
                (with-format (geom-base::x3d s)
                  (write-the chart (geom-base::cad-output-tree)))))

   (sky-x3d (starfield-x3d))

   (body
    (with-lhtml-string ()
      (:div :style "position:fixed;inset:0;background:#000;"
        (:|x3d| :id "bridge-x3d" :width "100%" :height "100%"
          :style "width:100%;height:100%;display:block;"
          (:|Scene|
            (:|Background| :|skyColor| "0 0 0.012")
            (:|Viewpoint| :|id| "port-eye"
              :|description| "Port eye"
              :|position| (point-string (the port-eye-position))
              :|orientation| (look-orientation (the port-eye-direction))
              :|fieldOfView| (the eye-field-of-view)
              :|zNear| "50" :|zFar| "800000"
              :|set_bind| "true")
            (:|Viewpoint| :|id| "starboard-eye"
              :|description| "Starboard eye"
              :|position| (point-string (the starboard-eye-position))
              :|orientation| (look-orientation (the starboard-eye-direction))
              :|fieldOfView| (the eye-field-of-view)
              :|zNear| "50" :|zFar| "800000")
            (str (the sky-x3d))
            (str (the chart-x3d)))))
      (:div :style "position:fixed;top:14px;left:14px;z-index:10;display:flex;gap:10px;font-family:sans-serif;"
        (:button :id "port-eye-btn" :type "button" :onclick "bindEye('port-eye')"
          :style (the eye-button-style) "◐ port eye")
        (:button :id "starboard-eye-btn" :type "button" :onclick "bindEye('starboard-eye')"
          :style (the eye-button-style) "starboard eye ◑"))
      (:div :style "position:fixed;bottom:12px;left:14px;z-index:10;color:#c9a227;font-family:sans-serif;font-size:13px;opacity:0.85;"
        "Galaxy World — the view from the bridge (first light)")
      (:script (str "
function bindEye (id) {
  document.getElementById(id).setAttribute('set_bind','true');
}"))))

   (eye-button-style
    "background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:6px 14px;font-size:13px;cursor:pointer;"))

  :objects
  ((chart :type 'chartroom:assembly)))
