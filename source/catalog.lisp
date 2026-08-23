;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

(defun deg->rad (degrees) (* degrees (/ pi 180)))

;; The places worth sailing to, for now: coplanar rings reachable by
;; a two-burn transfer from the ship's own ring. Each entry: key,
;; label for the chart table, ring radius [km], a word for the log.
(defparameter *destinations*
  '((:low-berth "the low berth" 6778
     "down where the stations keep")
    (:geo-ring "the geostationary ring" 42164
     "one sidereal day per lap")
    (:moon-road "the Moon's road" 384400
     "the long climb")))

(defun destination-entry (key)
  (assoc key *destinations*))

(defun destination-label (key)
  (second (destination-entry key)))

(defun destination-radius (key)
  (third (destination-entry key)))

(defun destination-choice-plist ()
  (append (list :none "— hold station —")
          (loop for (key label) in *destinations*
                append (list key label))))
