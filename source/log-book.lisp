;;;; -*- coding: utf-8 -*-
;;;; Copyright © 2026 Gornskew Enterprises -- AGPL-3.0-or-later.

(in-package :galaxy-world)

;; ============================================================
;; THE LOG BOOK: what the helms aboard actually do, and who flies
;; them.  Two ledgers, both mirrored to the host bind mount (the
;; /projects/.state convention) so a container recreate hands the
;; running record to its successor instead of zeroing it:
;;
;;   - THE TALLIES: one count per kind of thing done at any helm
;;     (:moves, :landings, :crashes, :voyages, :MARS-VOYAGES ...).
;;     One image, one table, every cockpit.
;;   - THE PILOT BOOK: the same counts kept per PILOT.  A pilot is
;;     a returning hand at the wheel: the browser keeps a token of
;;     its own minting and signs the book with it at boarding
;;     (cockpit.lisp, check-in!), and the book answers with a
;;     HANDLE -- a minted, publishable name (Rook-27).  The raw
;;     token never leaves the book; every published figure wears
;;     the handle.  No token offered (scripts off, or a crawler)
;;     reads as an unsigned boarding: the tallies still count it,
;;     the book does not.
;;
;; The whole channel hangs on one seam: TALLY!.  Any helm function
;; that calls (tally! :whatever) feeds the totals, the per-pilot
;; book (via *current-pilot*, bound around the move by the
;; cockpit), the stats feed below, and any board watching that
;; feed -- no further wiring.  To count a new kind of thing, call
;; tally! with a new key and you are done.

;; ------------------------------------------------------------
;; the shared state shelf

(defparameter *state-directory* #P"/projects/.state/"
  "Host-side shelf shared across the ship's rooms; survives a
container recreate, unlike /tmp.")

(defun write-state-file! (path form)
  "PRIN1 FORM to PATH on the state shelf.  Self-healing 1777 on the
directory: rooms of different lineages share the shelf, and whoever
builds it first otherwise locks the others out (the eyes-only
persistence story)."
  (ensure-directories-exist path)
  (ignore-errors
    (uiop:run-program
     (list "chmod" "1777" (namestring (truename *state-directory*)))
     :ignore-error-status t))
  (with-open-file (out path :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-length* nil) (*print-level* nil))
      (prin1 form out))))

(defun read-state-file (path)
  "The saved form at PATH, or NIL when absent or unreadable -- first
boot ever, or a shelf that isn't there.  Both read as nothing to
restore, never an error."
  (ignore-errors
    (when (probe-file path)
      (let ((*read-eval* nil))
        (with-open-file (in path) (read in))))))

;; ------------------------------------------------------------
;; the tallies

(defvar *helm-tallies* (make-hash-table :test 'eq))

(defparameter *tallies-file*
  (merge-pathnames "galaxy-world-tallies.sexp" *state-directory*))

(defparameter *tallies-min-interval* 15
  "Floor between disk writes, seconds -- moves are human-paced, but a
full cockpit deck shouldn't turn every click into a write.")

(defvar *tallies-last-persisted* 0)

(defvar *current-pilot* nil
  "The pilot id the move under way is flown by, bound by the cockpit
around each helm post (after-set! and check-in!).  NIL = an unsigned
hand: the tallies count the deed, the pilot book does not.")

(defun persist-tallies! (&optional force?)
  "Mirror *HELM-TALLIES* to *TALLIES-FILE*, throttled.  Errors are
swallowed -- a failed save must never cost a player his move."
  (ignore-errors
    (let ((now (get-universal-time)))
      (when (or force? (>= (- now *tallies-last-persisted*)
                           *tallies-min-interval*))
        (let ((entries nil))
          (maphash (lambda (key count) (push (cons key count) entries))
                   *helm-tallies*)
          (write-state-file! *tallies-file*
                            (list :version 1 :saved-at now
                                  :tallies entries)))
        (setq *tallies-last-persisted* now)))))

(defun restore-tallies! ()
  "Repopulate *HELM-TALLIES* from *TALLIES-FILE* at boot, or leave it
empty when the file is absent or unreadable -- a corrupt file should
degrade to a fresh log, never a failed boot."
  (ignore-errors
    (let* ((saved (read-state-file *tallies-file*))
           (entries (and (eql (getf saved :version) 1)
                         (getf saved :tallies))))
      (when entries
        (clrhash *helm-tallies*)
        (dolist (entry entries)
          (setf (gethash (car entry) *helm-tallies*) (cdr entry)))
        (format *trace-output*
                "~&[galaxy-world] restored helm tallies: ~a keys~%"
                (length entries))))))

(defun tally! (key)
  "Count one KEY in the tallies -- and in the signing pilot's page of
the book, when a pilot is at the wheel.  This is the one seam the
whole stats channel hangs on: a new (tally! :new-thing) call anywhere
flows to the totals, the pilot book, and the stats feed unaided."
  (incf (gethash key *helm-tallies* 0))
  (when *current-pilot*
    (pilot-tally! *current-pilot* key))
  (persist-tallies!))

;; ------------------------------------------------------------
;; the pilot book

(defvar *log-book* (make-hash-table :test 'equal)
  "Pilot id -> the pilot's page: a plist of :handle :first-seen
:last-seen :sessions and :counts (an alist mirroring the tally
keys).")

(defparameter *log-book-file*
  (merge-pathnames "galaxy-world-log-book.sexp" *state-directory*))

(defvar *log-book-last-persisted* 0)

(defvar *log-book-random-state* (make-random-state t)
  "Entropy for handle minting, seeded per process.")

(defparameter *handle-onsets*
  '("Ash" "Bex" "Cal" "Dru" "Eko" "Fen" "Gil" "Hux" "Ivo" "Jax"
    "Kip" "Lor" "Mav" "Nix" "Oz" "Pax" "Quill" "Rook" "Sol" "Tam"
    "Uma" "Vex" "Wren" "Yara" "Zed")
  "First halves of a pilot handle -- short hails that carry over a
noisy channel.")

(defun mint-handle ()
  "A fresh publishable pilot handle (Rook-27), avoiding every handle
already in the book.  After 20 collisions, widen the numerals."
  (flet ((taken? (candidate)
           (block search
             (maphash (lambda (id page)
                        (declare (ignore id))
                        (when (equal candidate (getf page :handle))
                          (return-from search t)))
                      *log-book*)
             nil))
         (pick () (format nil "~a-~a"
                          (nth (random (length *handle-onsets*)
                                       *log-book-random-state*)
                               *handle-onsets*)
                          (random 100 *log-book-random-state*))))
    (loop repeat 20
          for candidate = (pick)
          unless (taken? candidate) return candidate
          finally (return (format nil "~a~a" (pick)
                                  (random 1000 *log-book-random-state*))))))

(defun clean-pilot-id (raw)
  "RAW pilot token normalized, or NIL when it isn't one: 8-64 chars
of letters, digits and dashes.  The token is browser-minted and rides
a form post, so it gets the harbor-gate treatment before it touches
the book."
  (when (and (stringp raw)
             (<= 8 (length raw) 64)
             (every (lambda (c) (or (alphanumericp c) (char= c #\-)))
                    raw))
    (string-downcase raw)))

(defun persist-log-book! (&optional force?)
  "Mirror *LOG-BOOK* to *LOG-BOOK-FILE*, throttled on the tallies'
own cadence.  Errors are swallowed, same doctrine."
  (ignore-errors
    (let ((now (get-universal-time)))
      (when (or force? (>= (- now *log-book-last-persisted*)
                           *tallies-min-interval*))
        (let ((entries nil))
          (maphash (lambda (id page) (push (cons id page) entries))
                   *log-book*)
          (write-state-file! *log-book-file*
                            (list :version 1 :saved-at now
                                  :pilots entries)))
        (setq *log-book-last-persisted* now)))))

(defun restore-log-book! ()
  "Repopulate *LOG-BOOK* at boot; absent or corrupt degrades to a
fresh book, never a failed boot."
  (ignore-errors
    (let* ((saved (read-state-file *log-book-file*))
           (entries (and (eql (getf saved :version) 1)
                         (getf saved :pilots))))
      (when entries
        (clrhash *log-book*)
        (dolist (entry entries)
          (setf (gethash (car entry) *log-book*) (cdr entry)))
        (format *trace-output*
                "~&[galaxy-world] restored the pilot book: ~a pilots~%"
                (length entries))))))

(defun check-in-pilot! (id)
  "Pilot ID signs the book at boarding: a first signing opens a page
and mints a handle; a returning signing turns to it.  Returns the
page.  Counts :check-ins always and :new-pilots on a first signing --
through TALLY!, so the totals and the feed see boardings too."
  (let* ((now (get-universal-time))
         (page (gethash id *log-book*))
         (new? (null page)))
    (when new?
      (setq page (list :handle (mint-handle)
                       :first-seen now :last-seen now
                       :sessions 0 :counts nil))
      (setf (gethash id *log-book*) page))
    (setf (getf page :last-seen) now)
    (incf (getf page :sessions))
    (setf (gethash id *log-book*) page)
    (let ((*current-pilot* id))
      (tally! :check-ins)
      (when new? (tally! :new-pilots)))
    (persist-log-book! t)
    page))

(defun pilot-tally! (id key)
  "Count KEY on pilot ID's page.  Unknown ids are ignored -- the book
only counts hands that signed at boarding."
  (let ((page (gethash id *log-book*)))
    (when page
      (setf (getf page :last-seen) (get-universal-time))
      (let ((entry (assoc key (getf page :counts))))
        (if entry
            (incf (cdr entry))
            (push (cons key 1) (getf page :counts))))
      (setf (gethash id *log-book*) page)
      (persist-log-book!))))

;; ------------------------------------------------------------
;; the stats feed: a token-gated JSON channel for a watching board
;; (an Eyes Only instance, or any other reader holding the token).

(defparameter *stats-token-path* #P"/projects/.secrets/eyes-only-token"
  "The watch token: the same secret file the Eyes Only chain shares,
one value across the nodes.  Absent = the feed is open (the dev
posture); present = requests must carry ?key=<token>, and a wrong or
missing key answers a plain 404, indistinguishable from no page at
all.")

(defun stats-token ()
  (ignore-errors
    (when (probe-file *stats-token-path*)
      (let ((tok (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (uiop:read-file-string *stats-token-path*))))
        (when (plusp (length tok)) tok)))))

(defun stats-query-value (req name)
  "Query-string value by NAME via the request-query alist -- plain
alist access, no dependence on either aserve lineage's conveniences."
  (cdr (assoc name (net.aserve:request-query req) :test #'equal)))

(defun iso-stamp (universal-time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour min sec)))

(defun json-key-name (key)
  "A tally keyword as a JSON key: downcased, dashes to underscores."
  (substitute #\_ #\- (string-downcase (symbol-name key))))

(defun page-count (page key)
  (or (cdr (assoc key (getf page :counts))) 0))

(defun pilot-book-figures ()
  "The book's headline figures, one walk: (values total new-24h
new-7d returning aboard-1h pages).  Returning = pilots the book has
seen board more than once."
  (let ((now (get-universal-time))
        (total 0) (new-24h 0) (new-7d 0) (returning 0) (aboard-1h 0)
        (pages nil))
    (maphash
     (lambda (id page)
       (declare (ignore id))
       (incf total)
       (push page pages)
       (when (>= (getf page :first-seen 0) (- now 86400)) (incf new-24h))
       (when (>= (getf page :first-seen 0) (- now 604800)) (incf new-7d))
       (when (> (getf page :sessions 0) 1) (incf returning))
       (when (>= (getf page :last-seen 0) (- now 3600)) (incf aboard-1h)))
     *log-book*)
    (values total new-24h new-7d returning aboard-1h pages)))

(defparameter *stats-log-length* 12
  "Pilot pages carried in the feed's log, best hands first.")

(defun stats-json ()
  "The whole channel as one JSON document.  Headline figures ride
FLAT AND FIRST -- a dumb number-after parser on the far end must
find the summary before any per-pilot figure repeats a key name.
Every pilot wears his minted handle; raw ids never board the feed,
so the log section is publishable as it stands."
  (multiple-value-bind (total new-24h new-7d returning aboard-1h pages)
      (pilot-book-figures)
    (let ((tallies nil))
      (maphash (lambda (key count) (push (cons key count) tallies))
               *helm-tallies*)
      (setq tallies (sort tallies #'string< :key (lambda (entry)
                                                   (symbol-name (car entry)))))
      (with-output-to-string (out)
        (format out "{\"pilots\":~a,\"new_24h\":~a,\"new_7d\":~a,\"returning\":~a,\"aboard_1h\":~a"
                total new-24h new-7d returning aboard-1h)
        (dolist (key '(:moves :landings :yard-landings :crashes :voyages
                       :check-ins))
          (format out ",\"~a\":~a" (json-key-name key)
                  (gethash key *helm-tallies* 0)))
        (format out ",\"as_of\":\"~a\"" (iso-stamp (get-universal-time)))
        (format out ",\"totals\":{~{~a~^,~}}"
                (loop for (key . count) in tallies
                      collect (format nil "\"~a\":~a"
                                      (json-key-name key) count)))
        (format out ",\"log\":[~{~a~^,~}]}"
                (loop for page in (subseq
                                   (sort pages #'>
                                         :key (lambda (p)
                                                (page-count p :moves)))
                                   0 (min *stats-log-length*
                                          (length pages)))
                      collect
                      (format nil "{\"handle\":\"~a\",\"sessions\":~a,\"moves\":~a,\"landings\":~a,\"yard_landings\":~a,\"crashes\":~a,\"voyages\":~a,\"first_seen\":\"~a\",\"last_seen\":\"~a\"}"
                              (getf page :handle)
                              (getf page :sessions 0)
                              (page-count page :moves)
                              (page-count page :landings)
                              (page-count page :yard-landings)
                              (page-count page :crashes)
                              (page-count page :voyages)
                              (iso-stamp (getf page :first-seen 0))
                              (iso-stamp (getf page :last-seen 0)))))))))

(defun stats-responder (req ent)
  (let ((tok (stats-token)))
    (if (or (null tok)
            (equal tok (stats-query-value req "key")))
        (net.aserve:with-http-response
            (req ent :content-type "application/json")
          (net.aserve:with-http-body (req ent)
            (write-string (stats-json)
                          (net.aserve:request-reply-stream req))))
        ;; wrong/missing key: indistinguishable from a missing page
        (net.aserve:with-http-response
            (req ent :response net.aserve:*response-not-found*)
          (net.aserve:with-http-body (req ent))))))
