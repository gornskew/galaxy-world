;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

;; ENGINEERING -- galaxyworld.dev, the game's developer property
;; (ruled 2026-09-16; canon.org "Property roles").  Where a player
;; makes their ship better at the game.  One shared page at the root
;; of its own hosts, no sessions.  Same register as the game and the
;; works, pushed as far as it will go: nothing on this deck says what
;; a word means ashore -- the Signal Book does that, and this page
;; points at it.  Real names (git, docker, GitHub, Docker Hub, Gendl,
;; Emacs, MCP) are native and never pierce.  Training is the one door
;; ashore (Genworks Learn, genworks.dev) and it is marked temporary:
;; it closes when this deck keeps drills of its own.
;;
;; The muster block shows what `./basilisk up' actually prints --
;; the same fact block the works' front door carries; keepers' names
;; are minted per raising, so any three names are as true as any
;; other three.  When upstream basilisk output changes shape, this
;; block follows.

(define-object engineering-deck (base-html-page)

  :computed-slots
  ((title "Galaxy World — Engineering")
   (use-ajax? nil)
   (use-tailwind? nil)
   (use-svgpanzoom? nil)
   ;; a page of words: neither X3D browser rides in the head
   (use-x3dom? nil)
   (use-x-ite? nil)
   (include-default-favicon? nil)

   ;; The ship's own face on the tab, as the bridge wears it:
   ;; Engineering is where you meet her insides.
   (favicon-type "image/svg+xml")
   (favicon-path "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'%3E%3Ccircle cx='16' cy='16' r='15.5' fill='%23000008'/%3E%3Cellipse cx='16' cy='16' rx='12.5' ry='6' fill='%231c2030' stroke='%233c4358' stroke-width='0.8'/%3E%3Ccircle cx='10.4' cy='16' r='2.5' fill='%23ffd000'/%3E%3Ccircle cx='16.6' cy='16' r='2.5' fill='%23ffd000'/%3E%3Cellipse cx='10.8' cy='16' rx='0.65' ry='1.1' fill='%2312141c'/%3E%3Cellipse cx='17' cy='16' rx='0.65' ry='1.1' fill='%2312141c'/%3E%3Ccircle cx='26' cy='7' r='0.7' fill='%23cfd8ee'/%3E%3Ccircle cx='6' cy='6.4' r='0.55' fill='%23cfd8ee'/%3E%3Ccircle cx='27.5' cy='24' r='0.55' fill='%23cfd8ee'/%3E%3C/svg%3E")

   (meta-description "Engineering, for Galaxy World: where a player makes their ship better at the game. Raise a Basilisk from plans on your own vatgrounds, refit her rooms, write her standing orders, teach her crew. None of it is required to fly.")

   (additional-header-content
    (with-cl-who-string ()
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:meta :name "description" :content (the meta-description))
      (:meta :property "og:title" :content (the title))
      (:meta :property "og:description" :content (the meta-description))
      (:link :rel "canonical" :href "https://galaxyworld.dev/")
      (:style (str (the page-style)))))

   ;; The game's own palette: night, the ship's yellow, brass, and
   ;; the pale ink the bridge writes in.  One page, so the sheet
   ;; rides inline; no build step, no asset route.
   (page-style
    ":root{--night:#000008;--ink:#cfd8ee;--dim:#8ea0cf;--gold:#e8c839;--brass:#c9a227;--rule:#3c4358;--panel:#0b0d16}
html{background:var(--night)}
body{margin:0;background:var(--night);color:var(--ink);font-family:system-ui,-apple-system,'Segoe UI',sans-serif;line-height:1.55}
a{color:var(--gold)}a:hover{color:#fff0a0}
main{max-width:64rem;margin:0 auto;padding:0 16px 64px}
.hero{padding:56px 0 24px}
.kicker{font-size:12px;letter-spacing:.14em;text-transform:uppercase;color:var(--brass)}
h1{font-size:clamp(2.2rem,6vw,4rem);margin:.2em 0 .3em;color:#fff;letter-spacing:-.01em;line-height:1.05}
h2{font-size:1.6rem;color:var(--gold);margin:2.4em 0 .5em;letter-spacing:.02em}
.lede{font-size:1.15rem;max-width:44rem}
.btns{display:flex;flex-wrap:wrap;gap:12px;margin-top:24px}
.btn{display:inline-block;padding:10px 16px;border-radius:999px;border:1px solid var(--gold);color:var(--gold);text-decoration:none;font-size:14px;letter-spacing:.04em}
.btn.solid{background:var(--gold);color:#101010}
.btn:hover{background:#fff0a0;color:#101010}
.muster{background:var(--panel);border:1px solid var(--rule);border-radius:12px;padding:18px 20px;overflow-x:auto;font:13px/1.7 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;color:#d7dcea;margin:20px 0}
.prompt{color:#4ade80}.info{color:#7b8296}.post{color:#34d399}
.cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(15rem,1fr));gap:16px}
.card{background:var(--panel);border:1px solid var(--rule);border-radius:12px;padding:18px}
.card h3{margin:0 0 .35em;color:#fff;font-size:1.05rem}
.card .file{display:block;font:12px ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;color:var(--brass);margin-bottom:.6em}
.card p{margin:0;color:var(--dim);font-size:.95rem}
.links dt{color:#fff;font-weight:600;margin-top:16px}
.links dd{margin:2px 0 0;color:var(--dim)}
.note{border-left:3px solid var(--brass);padding:12px 18px;background:var(--panel);max-width:44rem}
footer{border-top:1px solid var(--rule);margin-top:64px;padding-top:20px;font-size:13px;color:var(--dim);display:flex;flex-wrap:wrap;gap:8px 20px;justify-content:space-between}
footer nav a{margin-right:16px;white-space:nowrap}")

   (body
    (with-cl-who-string ()
      (:main
       ;; Hero
       ((:header :class "hero")
        ((:div :class "kicker") "galaxyworld.dev · Engineering")
        (:h1 "Make her better at the game.")
        ((:p :class "lede")
         "Every ship in Galaxy World is raised from plans, and the
plans are yours. This is Engineering: where a player refits a room,
rewrites the standing orders, or teaches the crew a new trick, so the
ship flies better than she was raised. Some of it is programming.
Most of it is not. None of it is required to fly.")
        ((:div :class "btns")
         ((:a :class "btn solid" :href "https://galaxyworld.space/") "Fly first →")
         ((:a :class "btn" :href "#raise") "Raise your own")
         ((:a :class "btn" :href "https://gornskew.com/glossary/") "The Signal Book")))

       ;; Raise your own -- the fact block
       ((:section :id "raise")
        (:h2 "Raise your own")
        (:p "Your vatgrounds are any machine with docker fitted: a
laptop, the box under the desk, a rented berth in someone else's
galaxy. One incantation grows the hull and musters the crew, and every
raising is a new ship, under a name never worn before.")
        ((:pre :class "muster")
         ((:span :class "prompt") "$ ") "git clone https://github.com/gornskew/basilisk" (:br)
         ((:span :class "prompt") "$ ") "cd basilisk && ./basilisk up" (:br)
         ((:span :class "info") "[INFO] ") "Crew muster:" (:br)
         "  " ((:span :class "post") "Captain ") " Thweed's ready room     6942->6942" (:br)
         "  " ((:span :class "post") "1st Off ") " Vossik's bridge         19080->9080" (:br)
         "  " ((:span :class "post") "Engineer") " Dulmer's engine room    29080->9090")
        (:p "The yard's own scrolls walk the raising, and the works keep a "
            ((:a :href "https://gornskew.com/get-started/") "Get Started")
            " page for a first one. When she is up, take her out at "
            ((:a :href "https://galaxyworld.space/") "galaxyworld.space")
            "."))

       ;; Ways in
       (:section
        (:h2 "Ways to make her better")
        ((:div :class "cards")
         ((:div :class "card")
          (:h3 "The articles")
          ((:span :class "file") "basilisk.sexp")
          (:p "What she carries is written down before she flies: her
rooms, their species, their postings, their hailing frequencies.
Change the articles and raise again, and you get a different ship. No
compiler in sight."))
         ((:div :class "card")
          (:h3 "Standing orders")
          ((:span :class "file") "cyclops.sexp")
          (:p "The Captain commands by writing them: which hails the
transporter room admits, and where each consignment goes. Symbolic
expressions, read at the raising and again on a reload."))
         ((:div :class "card")
          (:h3 "A stack pouch")
          ((:span :class "file") "beside the yard")
          (:p "A small repository of deviations and additions, kept
beside the yard. How a ship takes on more than the base complement
without the yard itself being touched."))
         ((:div :class "card")
          (:h3 "The engine room")
          ((:span :class "file") "Gendl · Emacs")
          (:p "A manned room carries a compiler, and the Ship's Engineer
can design a ship from inside one. Gendl is the species the yard grows;
Emacs is how the ready room is worked. This is where the programming
lives, for the player who wants it."))
         ((:div :class "card")
          (:h3 "Send a cyborg")
          ((:span :class "file") "subspace · MCP")
          (:p "Every room answers on subspace, so a cyborg — Claude,
Codex, any client that speaks MCP — can be sent aboard to do the
engineering. Say what you want the ship to do. The REPL is the
API."))
         ((:div :class "card")
          (:h3 "Fitted rooms")
          ((:span :class "file") "from the works")
          (:p "Some rooms arrive pre-staffed: a Cyclops transporter
room, an Eyes Only radio shack. Bought rather than built, and they slot
into any hull. "
              ((:a :href "https://gornskew.com/") "The works") " sells them."))))

       ;; The yard and the stores
       (:section
        (:h2 "The yard and the stores")
        ((:dl :class "links")
         (:dt ((:a :href "https://github.com/gornskew/basilisk") "The yard"))
         (:dd "Where hulls are laid down: the plans, the scrolls, and the log of ships. Lives on GitHub.")
         (:dt ((:a :href "https://hub.docker.com/u/gornskew") "The stores"))
         (:dd "The species a ship is grown from: readymax, gendl, cyclops, eyes-only, chartroom. Kept on Docker Hub.")
         (:dt ((:a :href "https://github.com/gornskew/basilisk/blob/devo/BASILISK.md") "The canon"))
         (:dd "What a Basilisk is, in the yard's own words. This deck follows it, never the other way around.")
         (:dt ((:a :href "https://gornskew.com/glossary/") "The Signal Book"))
         (:dd "Every word on this deck, mapped in both directions.")
         (:dt ((:a :href "https://gornskew.com/") "The works"))
         (:dd "Gornskew Enterprises: the yard's keeper, the fitted rooms, and the game's publisher.")))

       ;; Drills -- the one door ashore, marked temporary
       ((:section :id "drills")
        (:h2 "Drills")
        ((:div :class "note")
         (:p "Engineering does not yet keep drills of its own. Until it
does, the drills ashore stand in: "
             ((:a :href "https://genworks.dev/") "Genworks Learn")
             ", at genworks.dev, teaches the Gendl the engine room
speaks, in the tongue spoken ashore. A temporary door; it closes when
this deck has training of its own.")
         ((:a :class "btn" :href "https://genworks.dev/") "Genworks Learn, ashore →")))

       (:footer
        (:span "© 2026 Gornskew Enterprises · Basilisk is free, AGPL")
        (:nav
         ((:a :href "https://galaxyworld.space/") "galaxyworld.space")
         ((:a :href "https://gornskew.com/") "gornskew.com")
         ((:a :href "https://github.com/gornskew") "GitHub")
         ((:a :href "https://hub.docker.com/u/gornskew") "Docker Hub"))))))))
