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
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">~%  <url>~%    <loc>https://galaxyworld.space/</loc>~%    <lastmod>2026-08-23</lastmod>~%    <changefreq>weekly</changefreq>~%  </url>~%  <url>~%    <loc>https://galaxyworld.space/cockpit</loc>~%    <lastmod>2026-08-28</lastmod>~%    <changefreq>weekly</changefreq>~%  </url>~%</urlset>~%"))

(gwl:with-all-servers (server)
  (gwl:publish-gwl-app "/galaxy-world" 'galaxy-world:bridge-view
                       :host *galaxy-world-hosts*
                       :server server)
  ;; the proxy hands the public "/" through as "/galaxy-world", so
  ;; sub-pages hang off that prefix
  (gwl:publish-gwl-app "/galaxy-world/cockpit" 'galaxy-world:cockpit-view
                       :host *galaxy-world-hosts*
                       :server server)
  (gwl:publish-string-content "/robots.txt" *robots-txt*
                              :host *galaxy-world-hosts*
                              :content-type "text/plain"
                              :server server)
  (gwl:publish-string-content "/sitemap.xml" *sitemap-xml*
                              :host *galaxy-world-hosts*
                              :content-type "application/xml"
                              :server server))
