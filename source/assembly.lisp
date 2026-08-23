;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

;; GW-0 keel: the chart the bridge view will render. GW-1a hangs the
;; eyes (two x3dom viewpoints) and the starfield on this; GW-1b adds
;; the plotted course. See PLAN.md.
(define-object assembly (base-object)
  :objects
  ((chart :type 'chartroom:assembly)))
