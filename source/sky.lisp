;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

;; Deterministic sky: the same stars on every raising. Plain LCG so
;; no seed state leaks between pages.
(defun starfield-points (&key (count 700) (radius 150000.0d0))
  (let ((state 20260823))
    (flet ((next ()
             (setq state (mod (+ (* state 1103515245) 12345) 2147483648))
             (/ state 2147483648.0d0)))
      (let (points)
        (dotimes (n count points)
          (let* ((u (- (* 2 (next)) 1))
                 (phi (* 2 pi (next)))
                 (s (sqrt (max 0.0d0 (- 1 (* u u))))))
            (push (list (* radius s (cos phi))
                        (* radius s (sin phi))
                        (* radius u))
                  points)))))))

;; The sky is the same for every session -- deterministic stars --
;; so the markup is cut once and shared by all cockpits rather than
;; rendered per instance.
(defvar *starfield-x3d-cache* (make-hash-table :test 'equal))

(defun starfield-x3d (&key (count 700) (radius 150000.0d0))
  "The night behind everything, as an x3d PointSet."
  (let ((key (list count radius)))
    (or (gethash key *starfield-x3d-cache*)
        (setf (gethash key *starfield-x3d-cache*)
              (with-output-to-string (s)
                (write-string "<Shape><Appearance><Material emissiveColor=\"1 1 1\"></Material></Appearance><PointSet><Coordinate point=\"" s)
                (dolist (p (starfield-points :count count :radius radius))
                  (format s "~,1f ~,1f ~,1f, " (first p) (second p) (third p)))
                (write-string "\"></Coordinate></PointSet></Shape>" s))))))
