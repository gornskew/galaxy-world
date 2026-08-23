;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

(gwl:with-all-servers (server)
  (gwl:publish-gwl-app "/galaxy-world" 'galaxy-world:bridge-view
                       :server server))
