;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

(defun point-string (point)
  (format nil "~,1f ~,1f ~,1f" (get-x point) (get-y point) (get-z point)))

(defun viewpoint-x3d (id description position direction field-of-view
                      &key (z-near "50") (z-far "2500000") up)
  "One x3d Viewpoint as markup. x3dom binds the first Viewpoint in
document order at load, so callers control binding by emission order.
The default clip planes suit the orbital scenes; a room-sized scene
passes its own, along with an UP to keep its floor down."
  (format nil "<Viewpoint id=\"~a\" description=\"~a\" position=\"~a\" orientation=\"~a\" fieldOfView=\"~a\" zNear=\"~a\" zFar=\"~a\"></Viewpoint>"
          id description (point-string position)
          (if up
              (look-at-orientation direction up)
              (look-orientation direction))
          field-of-view z-near z-far))

(defun look-orientation (direction)
  "x3d axis-angle string rotating the default gaze (0 0 -1) onto DIRECTION.
Leaves camera roll wherever the rotation lands it -- fine in open
space, wrong inside a room; there use look-at-orientation."
  (let* ((d (unitize-vector direction))
         (from (make-vector 0 0 -1))
         (dot (dot-vectors from d)))
    (cond ((> dot 0.99999) "0 1 0 0")
          ((< dot -0.99999) "0 1 0 3.14159")
          (t (let ((axis (unitize-vector (cross-vectors from d)))
                   (angle (acos dot)))
               (format nil "~,5f ~,5f ~,5f ~,5f"
                       (get-x axis) (get-y axis) (get-z axis) angle))))))

(defun look-at-orientation (direction up)
  "x3d axis-angle string for a camera gazing along DIRECTION with its
screen-up as near UP as the gaze allows. Degenerate when gaze and UP
are parallel -- don't point this straight up or down."
  (let* ((g (unitize-vector direction))
         (r (unitize-vector (cross-vectors g (unitize-vector up))))
         (u (cross-vectors r g))
         (trace (+ (get-x r) (get-y u) (- (get-z g))))
         (angle (acos (max -1.0d0 (min 1.0d0 (/ (- trace 1) 2))))))
    (if (< angle 1.0d-5)
        "0 1 0 0"
        (let ((d (* 2 (sin angle))))
          (format nil "~,5f ~,5f ~,5f ~,5f"
                  (/ (+ (get-z u) (get-y g)) d)
                  (/ (- (- (get-x g)) (get-z r)) d)
                  (/ (- (get-y r) (get-x u)) d)
                  angle)))))

;; The view from the bridge: the ship rides her own ring above the
;; home planet, you look out the two side eyes, and the chart table
;; plots a course. Course choice travels by classic form post -- the
;; whole page re-renders, which also gives x3dom a clean re-init.
;; session-control-mixin keeps public sessions mortal (the reaper
;; collects expired ones -- the berth must run
;; gwl:start-session-reaper).
(define-object bridge-view (session-control-mixin base-html-page)

  :input-slots
  ((title "Galaxy World")
   (use-ajax? nil)
   (use-svgpanzoom? nil)
   (use-tailwind? nil)

   ;; The bridge flies its own colors: a yellow eye, inlined as a
   ;; data: URI so the icon needs no route and no static asset.
   ;; Palette matches the page -- iris #e8c839, rim #7a6a1f, sky
   ;; #000003 (the scene's skyColor).
   (favicon-type "image/svg+xml")
   (favicon-path "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'%3E%3Ccircle cx='32' cy='32' r='31' fill='%23000003'/%3E%3Ccircle cx='32' cy='32' r='24' fill='%23e8c839' stroke='%237a6a1f' stroke-width='2'/%3E%3Cellipse cx='32' cy='32' rx='7' ry='19' fill='%23050505'/%3E%3Ccircle cx='25' cy='23' r='4.5' fill='%23fff8d8' opacity='.75'/%3E%3C/svg%3E")

   ;; The ship's ring: circular parking orbit, km / degrees. The
   ;; transfer departs this ring, in this plane.
   (ship-orbit-radius 20742)
   (ship-orbit-inclination 15)
   (ship-orbit-raan 0)
   ;; where along the ring the ship rides just now
   (ship-true-anomaly 15)

   ;; How far outboard each eye sits from the centerline, km. At
   ;; orbital scale the offset is symbolic, but the eyes are two.
   (eye-offset 40)
   ;; How far outboard of dead-ahead each eye gazes: 0 stares at the
   ;; bow, 1 stares abeam.
   (eye-splay 0.45)
   (eye-field-of-view "1.0")

   (destination-default :none))

  :computed-slots
  ((ship-station
    (multiple-value-bind (r v)
        (bsk-astro:elem2rv bsk-astro:+mu-earth+
                           :a (coerce (the ship-orbit-radius) 'double-float)
                           :e 0.0005d0
                           :i (deg->rad (the ship-orbit-inclination))
                           :big-omega (deg->rad (the ship-orbit-raan))
                           :little-omega 0d0
                           :f (deg->rad (the ship-true-anomaly)))
      (declare (ignore v))
      (make-point (first r) (second r) (third r))))

   (bow-direction (unitize-vector
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

   (destination (the destination-choice value))
   (course-plotted? (and (the destination)
                         (not (eql (the destination) :none))
                         (destination-entry (the destination))))

   ;; The chart view: pulled up out of the ring plane, framing
   ;; whatever is plotted (or the geostationary ring by default).
   ;; Bound automatically when a course is on the table.
   (chart-extent (if (the course-plotted?)
                     (max (the ship-orbit-radius)
                          (destination-radius (the destination)))
                     42164))
   (chart-eye-position (make-point (* 1.15 (the chart-extent))
                                   (* -1.15 (the chart-extent))
                                   (* 0.85 (the chart-extent))))
   (chart-eye-direction (unitize-vector
                         (subtract-vectors (make-point 0 0 0)
                                           (the chart-eye-position))))

   ;; The first viewpoint in document order is bound at load: the
   ;; chart view when a course is on the table, the port eye
   ;; otherwise.  Every eye keeps the pole star overhead (:up +z):
   ;; the bare shortest-arc orientation left the cameras rolled 90
   ;; degrees for gazes lying near the ring plane.
   (viewpoints-x3d
    (let* ((up (make-vector 0 0 1))
           (port (viewpoint-x3d "port-eye" "Port eye"
                                (the port-eye-position)
                                (the port-eye-direction)
                                (the eye-field-of-view)
                                :up up))
           (starboard (viewpoint-x3d "starboard-eye" "Starboard eye"
                                     (the starboard-eye-position)
                                     (the starboard-eye-direction)
                                     (the eye-field-of-view)
                                     :up up))
           (chart-eye (viewpoint-x3d "chart-eye" "Chart view"
                                     (the chart-eye-position)
                                     (the chart-eye-direction)
                                     (the eye-field-of-view)
                                     :up up)))
      (if (the course-plotted?)
          (string-append chart-eye port starboard)
          (string-append port starboard chart-eye))))

   (chart-x3d (with-output-to-string (s)
                (with-format (geom-base::x3d s)
                  (write-the chart (geom-base::cad-output-tree)))))

   ;; Only the transfer arc itself (with its rider marker) joins the
   ;; scene -- the departure and arrival rings are already on the
   ;; chart.
   (course-x3d (if (the course-plotted?)
                   (with-output-to-string (s)
                     (with-format (geom-base::x3d s)
                       (write-the transfer transfer-orbit
                                  (geom-base::cad-output-tree))))
                   ""))

   (sky-x3d (starfield-x3d :radius 900000.0d0))

   ;; with-html-form writes to gwl's *stream*, which with-lhtml-string
   ;; does not bind -- so the form renders into its own string here.
   (chart-form-html
    (with-output-to-string (form-stream)
      (let ((gwl::*stream* form-stream))
        (gwl:with-html-form (:cl-who? t :suppress-border? t)
          (str (the destination-choice html-string))
          (htm (:input :type "submit" :value "plot course"
                :style "margin-top:8px;background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:4px 12px;font-size:12px;cursor:pointer;"))))))

   (body
    (with-lhtml-string ()
      (:div :style "position:fixed;inset:0;background:#000;"
        (:|x3d| :id "bridge-x3d" :width "100%" :height "100%"
          :style "width:100%;height:100%;display:block;"
          (:|Scene|
            (:|Background| :|skyColor| "0 0 0.012")
            (str (the viewpoints-x3d))
            (str (the sky-x3d))
            (str (the chart-x3d))
            (str (the course-x3d)))))
      (:div :style "position:fixed;top:14px;left:14px;z-index:10;display:flex;gap:10px;font-family:sans-serif;"
        (:button :id "port-eye-btn" :type "button" :onclick "bindEye('port-eye')"
          :style (the eye-button-style) "◐ port eye")
        (:button :id "starboard-eye-btn" :type "button" :onclick "bindEye('starboard-eye')"
          :style (the eye-button-style) "starboard eye ◑")
        (:button :id "chart-eye-btn" :type "button" :onclick "bindEye('chart-eye')"
          :style (the eye-button-style) "⊙ chart view")
        (:a :href "/" :style (string-append (the eye-button-style)
                                            "text-decoration:none;display:inline-block;")
          "to the cockpit"))
      ;; the chart table
      (:div :style "position:fixed;bottom:14px;right:14px;z-index:10;background:rgba(16,16,16,0.88);border:1px solid #e8c839;border-radius:10px;padding:12px 16px;font-family:sans-serif;color:#e8c839;font-size:13px;min-width:230px;"
        (:div :style "font-size:14px;margin-bottom:8px;letter-spacing:0.06em;" "CHART TABLE")
        (str (the chart-form-html))
        (when (the course-plotted?)
          (htm
           (:div :style "margin-top:10px;border-top:1px solid #7a6a1f;padding-top:8px;line-height:1.5;"
             (:div (fmt "course: ~a" (destination-label (the destination))))
             (:div :style "font-size:11px;font-style:italic;color:#c9a227;"
               (fmt "for the log: ~a" (destination-log-word (the destination))))
             ;; each figure explains itself on hover
             (:div :style "cursor:help;"
               :title "speed added leaving the ship's ring — the push onto the transfer ellipse"
               (fmt "burn one: ~,3f km/s" (the transfer first-burn-delta-v)))
             (:div :style "cursor:help;"
               :title "speed added on arrival — the push that settles onto the new ring"
               (fmt "burn two: ~,3f km/s" (the transfer second-burn-delta-v)))
             (:div :style "cursor:help;"
               :title "the whole fuel bill, counted in speed — every burn added up"
               (fmt "delta-v total: ~,3f km/s" (the transfer total-delta-v)))
             (:div :style "cursor:help;"
               :title "half a lap of the transfer ellipse, engines quiet the whole way"
               (fmt "time of flight: ~,1f hours~@[ (~,1f days)~]"
                        (the transfer transfer-time-hours)
                        (let ((hours (the transfer transfer-time-hours)))
                          (when (> hours 48) (/ hours 24.0)))))
             (:div :style "margin-top:6px;font-size:11px;color:#c9a227;opacity:0.85;"
               "the least-fuel two-burn road between rings — a Hohmann transfer")))))
      (:div :style "position:fixed;bottom:12px;left:14px;z-index:10;color:#c9a227;font-family:sans-serif;font-size:13px;opacity:0.85;"
        "Galaxy World — the view from the bridge (first light)")
      (:script (str "
function bindEye (id) {
  var vp = document.getElementById(id);
  if (vp) vp.setAttribute('set_bind', 'true');
  setTimeout(function () {
    var x = document.querySelector('x3d');
    if (x && x.runtime && x.runtime.resetView) x.runtime.resetView();
  }, 80);
}"))))

   (eye-button-style
    "background:#1a1a1a;color:#e8c839;border:1px solid #e8c839;border-radius:999px;padding:6px 14px;font-size:13px;cursor:pointer;"))

  :objects
  ((chart :type 'chartroom:assembly
          :orbit-specs
          (append
           (list (list :name "ship-ring"
                       :semi-major-axis (the ship-orbit-radius)
                       :eccentricity 0.0005
                       :inclination (the ship-orbit-inclination)
                       :raan (the ship-orbit-raan)
                       :true-anomaly (the ship-true-anomaly)
                       :display-color :green))
           ;; destination rings ride the chart too, all with their
           ;; nodes on the x-axis so a plotted arc meets its ring
           ;; exactly at arrival
           (list (list :name "geo-ring" :semi-major-axis 42164
                       :eccentricity 0 :inclination 0
                       :display-color :cyan)
                 (list :name "moon-road" :semi-major-axis 384400
                       :eccentricity 0 :inclination 20
                       :display-color :grey))))

   (destination-choice :type 'gwl:menu-form-control
                       :prompt "destination: "
                       :size 1
                       :default (the destination-default)
                       :choice-plist (destination-choice-plist))

   (transfer :type 'chartroom:hohmann-transfer
             :initial-radius (the ship-orbit-radius)
             :final-radius (if (the course-plotted?)
                               (destination-radius (the destination))
                               42164)
             :inclination (the ship-orbit-inclination)
             :raan (the ship-orbit-raan))))
