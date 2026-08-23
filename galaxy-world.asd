;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(asdf:defsystem #:galaxy-world
  :description
  "Galaxy World -- the game the Basilisk-class ships sail in. The
world model and the web lens (galaxyworld.space): the view out the
ship's two yellow eyes, and a chart table for plotting courses,
worked by the ship's own chartroom."
  :author "Gornskew Enterprises"
  :license "AGPL-3.0-or-later"
  :serial t
  :version "20260823"
  :depends-on (:chartroom)
  :components
  ((:file "source/package")
   (:file "source/assembly")))
