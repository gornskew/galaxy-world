;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

;; The hosts the game answers on.  The berth is shared and the proxy
;; hands every request through with its original Host header, so the
;; publishes are scoped to the game's own names -- the bridge does not
;; answer on the other properties that route to the same berth.
;; galaxyworld.test / galaxyworld.localhost are the local dev mirror.
(defparameter *galaxy-world-hosts*
  (list "galaxyworld.space" "www.galaxyworld.space"
        "galaxyworld.dev" "www.galaxyworld.dev"
        "galaxyworld.test" "galaxyworld.localhost"))

;; Crawlers are welcome on the bridge; session-phase URLs are minted,
;; mortal, and meaningless to index.
(defparameter *robots-txt*
  (format nil "User-agent: *~%Disallow: /sessions/~%Disallow: /answer~%~%Sitemap: https://galaxyworld.space/sitemap.xml~%"))

(defparameter *sitemap-xml*
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">~%  <url>~%    <loc>https://galaxyworld.space/</loc>~%    <lastmod>2026-09-01</lastmod>~%    <changefreq>weekly</changefreq>~%  </url>~%  <url>~%    <loc>https://galaxyworld.space/bridge</loc>~%    <lastmod>2026-09-01</lastmod>~%    <changefreq>weekly</changefreq>~%  </url>~%</urlset>~%"))

;; The X_ITE scout's scene road: the standing X3D document for one
;; session's cockpit, looked up by instance id.  The page at /xr
;; points its x3d-canvas here; a stale or foreign iid answers 404
;; and the canvas simply stays dark.
;; the hails endpoint: the page polls it by ship= (the transporter's
;; own hint, never iid=), the poll is the session's heartbeat on the
;; roster, and the answer is the roster and the log for everyone
(defun hails-responder (req ent)
  (let* ((iid (net.aserve:request-query-value "ship" req))
         (entry (and iid (gethash (gwl::make-keyword-sensitive iid)
                                  gwl::*instance-hash-table*)))
         (self (first entry))
         (doc (and self (typep self 'galaxy-world::cockpit-view)
                   (progn (gdl:the-object self (set-slot! :seen-at (galaxy-world::unix-now)))
                          (galaxy-world::hails-json self)))))
    (net.aserve:with-http-response
        (req ent :content-type "application/json"
             :response (if doc net.aserve:*response-ok*
                           net.aserve:*response-not-found*))
      (net.aserve:with-http-body (req ent)
        (write-string (or doc "{}") net.html.generator:*html-stream*)))))

;; THE ROOMS: the bridge's chat, polled and posted by ship= like the
;; hails; room= names the room (bridge by default), say= posts a
;; line (refused with a notice when the two-aboard rule shuts the
;; room), key= marks the captain when it is the captain's secret.
;; The poll is a heartbeat on the roster too.  The logs live as
;; s-expressions on the durable shelf; JSON is made here for the wire.
(defun chat-responder (req ent)
  (let* ((iid (net.aserve:request-query-value "ship" req))
         (room (net.aserve:request-query-value "room" req))
         (say (net.aserve:request-query-value "say" req))
         (key (net.aserve:request-query-value "key" req))
         (entry (and iid (gethash (gwl::make-keyword-sensitive iid)
                                  gwl::*instance-hash-table*)))
         (self (first entry))
         (doc (and self (typep self 'galaxy-world::cockpit-view)
                   (progn
                     (gdl:the-object self (set-slot! :seen-at (galaxy-world::unix-now)))
                     (when (and key (plusp (length key)))
                       (let ((captain? (galaxy-world::captain-key? key)))
                         (unless (eq captain? (gdl:the-object self captain?))
                           (gdl:the-object self (set-slot! :captain? captain?)))))
                     (let ((notice (and say (plusp (length say))
                                        (gdl:the-object self (chat-post! room say)))))
                       (gdl:the-object self (chat-json room notice)))))))
    (net.aserve:with-http-response
        (req ent :content-type "application/json"
             :response (if doc net.aserve:*response-ok*
                           net.aserve:*response-not-found*))
      (net.aserve:with-http-body (req ent)
        (write-string (or doc "{}") net.html.generator:*html-stream*)))))

(defun xr-scene-responder (req ent)
  ;; the session rides in as "ship", not "iid": an iid-named query
  ;; would wake the transporter's affinity module, which blackholes
  ;; hints that carry no affinity record
  (let* ((iid (net.aserve:request-query-value "ship" req))
         (entry (and iid (gethash (gwl::make-keyword-sensitive iid)
                                  gwl::*instance-hash-table*)))
         (self (first entry))
         (doc (and self (typep self 'galaxy-world::cockpit-xr-view)
                   (gdl:the-object self xr-scene-document))))
    (net.aserve:with-http-response
        (req ent :content-type "model/x3d+xml"
             :response (if doc net.aserve:*response-ok*
                           net.aserve:*response-not-found*))
      (net.aserve:with-http-body (req ent)
        (when doc
          (write-string doc (net.aserve:request-reply-stream req)))))))

;; the log book survives a recreate: read the predecessor's totals
;; and pilot pages off the host mount before the doors open
(restore-tallies!)
(restore-log-book!)

(gwl:with-all-servers (server)
  ;; the front door is the DRIVER'S SEAT: the public "/" (which the
  ;; proxy hands through as "/galaxy-world") lands you in the
  ;; cockpit, hands on the wheel -- UNDER X_ITE, the default
  ;; renderer as of 2026-08-31 (true no-reload turns, WebXR button
  ;; where a headset offers).  The x3dom build stands at /x3dom;
  ;; the bridge one deck up at /bridge; the old /cockpit route
  ;; stays good for bookmarks and rides the default too.
  (gwl:publish-gwl-app "/galaxy-world" 'galaxy-world::cockpit-xr-view
                       :host *galaxy-world-hosts*
                       :server server)
  (gwl:publish-gwl-app "/galaxy-world/bridge" 'galaxy-world:bridge-view
                       :host *galaxy-world-hosts*
                       :server server)
  (gwl:publish-gwl-app "/galaxy-world/cockpit" 'galaxy-world::cockpit-xr-view
                       :host *galaxy-world-hosts*
                       :server server)
  (gwl:publish-gwl-app "/x3dom" 'galaxy-world:cockpit-view
                       :host *galaxy-world-hosts*
                       :server server)
  (gwl:publish-gwl-app "/galaxy-world/x3dom" 'galaxy-world:cockpit-view
                       :host *galaxy-world-hosts*
                       :server server)
  ;; the X_ITE scout rides at /xr -- the second renderer of the
  ;; same standard, and the road to the WebXR button.  Bare paths
  ;; (the proxy hands unknown paths through unchanged, the way
  ;; robots.txt travels), plus the /galaxy-world twins for
  ;; direct-berth work.
  (gwl:publish-gwl-app "/xr" 'cockpit-xr-view
                       :host *galaxy-world-hosts*
                       :server server)
  (gwl:publish-gwl-app "/galaxy-world/xr" 'cockpit-xr-view
                       :host *galaxy-world-hosts*
                       :server server)
  ;; the .x3d suffix matters: X_ITE sniffs it alongside the
  ;; content-type when deciding how to parse
  (dolist (path (list "/xr-scene" "/xr-scene.x3d"
                      "/galaxy-world/xr-scene" "/galaxy-world/xr-scene.x3d"))
    (net.aserve:publish :path path
                        :host *galaxy-world-hosts*
                        :server server
                        :function 'xr-scene-responder))
  ;; the hails: roster + log, polled by every cockpit
  (dolist (path (list "/hails.json" "/galaxy-world/hails.json"))
    (net.aserve:publish :path path
                        :host *galaxy-world-hosts*
                        :server server
                        :function 'hails-responder))
  ;; the rooms: the bridge's chat, polled and posted by every cockpit
  (dolist (path (list "/chat.json" "/galaxy-world/chat.json"))
    (net.aserve:publish :path path
                        :host *galaxy-world-hosts*
                        :server server
                        :function 'chat-responder))
  ;; the real faces: NASA imagery (Blue Marble; LROC color mosaic)
  ;; as static routes -- public domain, courtesy NASA.  Path
  ;; resolution rides the system definition, so flat and bucketed
  ;; checkouts both find the files.
  (dolist (spec '(("/gw-tex/earth.jpg" "earth.jpg")
                  ("/galaxy-world/gw-tex/earth.jpg" "earth.jpg")
                  ("/gw-tex/earth-night.jpg" "earth-night.jpg")
                  ("/galaxy-world/gw-tex/earth-night.jpg" "earth-night.jpg")
                  ("/gw-tex/moon.jpg" "moon.jpg")
                  ("/galaxy-world/gw-tex/moon.jpg" "moon.jpg")
                  ("/gw-tex/mars.jpg" "mars.jpg")
                  ("/galaxy-world/gw-tex/mars.jpg" "mars.jpg")
                  ("/gw-tex/jupiter.jpg" "jupiter.jpg")
                  ("/galaxy-world/gw-tex/jupiter.jpg" "jupiter.jpg")
                  ("/gw-tex/saturn.jpg" "saturn.jpg")
                  ("/galaxy-world/gw-tex/saturn.jpg" "saturn.jpg")))
    (net.aserve:publish-file
     :path (first spec)
     :file (namestring (asdf:system-relative-pathname
                        :galaxy-world
                        (format nil "textures/~a" (second spec))))
     :content-type "image/jpeg"
     :host *galaxy-world-hosts*
     :server server))
  ;; the stats feed: the log book's totals, pilot figures, and the
  ;; handle-only log as one JSON channel for a watching board --
  ;; token-gated (log-book.lisp), riding a bare path the proxy
  ;; hands through unchanged (the robots.txt road) plus the
  ;; /galaxy-world twin for direct-berth work
  (dolist (path (list "/galaxy-world-stats" "/galaxy-world/galaxy-world-stats"))
    (net.aserve:publish :path path
                        :host *galaxy-world-hosts*
                        :server server
                        :content-type "application/json"
                        :function 'stats-responder))
  (gwl:publish-string-content "/robots.txt" *robots-txt*
                              :host *galaxy-world-hosts*
                              :content-type "text/plain"
                              :server server)
  (gwl:publish-string-content "/sitemap.xml" *sitemap-xml*
                              :host *galaxy-world-hosts*
                              :content-type "application/xml"
                              :server server))
