;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

;; The keel: the chart the bridge view will render. The eyes (two
;; viewpoints), the starfield, and the plotted course land on this
;; next.
(define-object assembly (base-object)
  :objects
  ((chart :type 'chartroom:assembly)))
