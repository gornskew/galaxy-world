;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.
;;;;
;;;; FLAG: PROMOTE TO CORE GENDL.  These are gap-filling lenses for
;;;; base geom-base primitives that the stock x3d format does not yet
;;;; cover.  They are defined straight into :geom-base so promotion is
;;;; a file move (geom-base/lenses/source/x3d.lisp), after which this
;;;; file shrinks and eventually disappears.  Ruling 2026-08-23:
;;;; additional lenses incubate here for now.

(in-package :geom-base)

;; Without this lens a bezier-curve silently emits NOTHING in an x3d
;; scene -- the gap was masked on surf-capable images, whose curves
;; ride surf's (x3d curve) lens instead.  Discovered when the Galaxy
;; World charts vanished on the free berths (first light must sail on
;; base articles: no guild engineer aboard a base basilisk).
(define-lens (x3d bezier-curve)()
  :output-functions
  (;; Sampled chords of the cubic, same emission shape as
   ;; global-polyline.  Sixteen chords keep a single cubic segment
   ;; below pixel scale at any sane zoom; callers drawing long curves
   ;; as bezier chains get smoothness segment by segment.
   (shape
    ()
    (let ((points (the (equi-spaced-points 17))))
      (cl-who:with-html-output (*stream* nil :indent nil)
        (:|Shape|
         (:|Appearance| (progn (when (getf (the display-controls) :linetype)
                                 (write-the line-properties))
                               (write-the material-properties)))
         (:|IndexedLineSet| :|coordIndex| (format nil "~{~a ~}-1" (list-of-numbers 0 (1- (length points))))
                            (:|Coordinate| :|point| (format nil "~{~a~^ ~}"
                                                            (let ((*read-default-float-format* 'single-float))
                                                              (mapcar #'(lambda(point) (format nil "~a ~a ~a"
                                                                                               (coerce (get-x point) 'single-float)
                                                                                               (coerce (get-y point) 'single-float)
                                                                                               (coerce (get-z point) 'single-float)))
                                                                      (mapcar #'(lambda(point) (the (global-to-local* point)))
                                                                              points))))))))))))
