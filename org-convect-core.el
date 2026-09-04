;;; org-convect-core.el --- The Horizons of Focus in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-convect
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The dependency root of org-convect: everything else requires this file and
;; this file requires nothing of ours.
;;
;; What lives here is the *ladder* -- reading the Horizons of Focus out of Org
;; files, resolving the links between them, and saying which rung is due.  No
;; rendering and no agenda.
;;
;; The altitudes are the ones GTD names, and the names are kept exactly as GTD
;; writes them so that anything written about the model can be read against
;; this file.  Everything said *about* them here -- what each rung is, how to
;; find yours, how often to look -- is written from scratch rather than copied.
;;
;;   HORIZON 5  Purpose and Principles            no schedule; when adrift
;;   HORIZON 4  Vision                            no schedule; when adrift
;;   HORIZON 3  Goals and Objectives              yearly, checked quarterly
;;   HORIZON 2  Areas of Focus and Accountability monthly, and when life shifts
;;   HORIZON 1  Projects                          weekly
;;   GROUND     Calendar/Actions                  daily
;;
;; H1 and Ground are org-foresight's; this package holds H2 upward.
;;
;; Getting Things Done and GTD are registered trademarks of the David Allen
;; Company.  This package is not affiliated with, authorised by or endorsed by
;; them; the names above identify the model it implements, nothing more.
;;
;; Two rules shape everything here.
;;
;; The ladder is declared, not located.  An entry belongs to a horizon because
;; it carries `CONVECT_HORIZON', never because of the file or the heading it
;; sits under.  Sections in a file are scaffolding for the eye; move an entry
;; anywhere, split the ladder across files, and it still reads.
;;
;; Nothing points downward.  A task never names its goal; it carries a CATEGORY
;; and nothing else, and CATEGORY is inherited, so acting on a five-minute task
;; costs no thought about purpose.  Only the review climbs, and only the code
;; doing the climbing pays for it.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'eldoc)
(require 'seq)
(require 'cl-lib)

(defgroup org-convect nil
  "The Horizons of Focus above the project level."
  :group 'org
  :prefix "org-convect-")

;;;; The ladder

(defconst org-convect-horizons
  '((area    . "Areas of Focus and Accountability")
    (goal    . "Goals and Objectives")
    (vision  . "Vision")
    (purpose . "Purpose and Principles"))
  "The horizons this package models, paired with their names in GTD.
Ordered *lowest first*, which is the order they get written: it is very hard
to reflect on purpose while drowning in the day, so the runway is cleared
first and the ladder is climbed from there.  Priority runs the other way --
purpose drives vision, which creates goals, which frame areas -- but that is
what the levels mean once written, not the order of writing them.")

(defun org-convect-horizon-p (symbol)
  "Return non-nil when SYMBOL names a horizon this package models."
  (and (assq symbol org-convect-horizons) t))

(defun org-convect-horizon-name (horizon)
  "Return the GTD name of HORIZON."
  (alist-get horizon org-convect-horizons))

;;;; Where the ladder lives

(defcustom org-convect-files
  (list (expand-file-name "horizons.org" (or (bound-and-true-p org-directory)
                                             default-directory)))
  "Files scanned for horizon entries.

Deliberately not `org-agenda-files'.  Nothing on the ladder carries a TODO
state -- a goal is reviewed quarterly, not engaged daily -- so a file holding
only the ladder never qualifies as an agenda file in the first place, and a
list of its own is the only way this package can find it."
  :type '(repeat file)
  :group 'org-convect)

(defcustom org-convect-serves-separator ";"
  "Separator between the several names in a `CONVECT_SERVES' property.

Org's own multivalued properties split on whitespace and escape spaces as
`%20', which is unreadable in a drawer that is written and read by hand.  An
area serving two goals is a sentence a person types, so it is punctuated like
one."
  :type 'string
  :group 'org-convect)

(defface org-convect-lineage-face
  '((t :inherit secondary-selection))
  "Face for the rungs linked to the one under the cursor.

Inherits from `secondary-selection\\=' rather than naming a colour: it means
\"marked, but not the thing you are acting on\", which is exactly what these
rows are, and a theme that has thought about that face has already thought
about this one."
  :group 'org-convect)

(defcustom org-convect-highlight-lineage t
  "Whether moving the cursor on a board lights up the linked rungs.

The mesh is the part of the ladder a file cannot show, and this is the cheapest
way to see it: no command, no second buffer, just what moves together when you
move.  Set to nil if the movement is more distracting than the links are worth."
  :type 'boolean
  :group 'org-convect)

(defcustom org-convect-area-width nil
  "Display columns an area's name may take, or nil to not ask.

Nil by default because this package no longer puts area names anywhere with a
fixed width.  It used to: areas were tasks' CATEGORY and the agenda prints
that in a narrow column, which made a long name a real nuisance.  That is
gone, and the constraint went with it.

Set it if you display areas somewhere narrow of your own -- a column in the
agenda through `%(...)\\=' in `org-agenda-prefix-format\\=', say -- and want to be
told when a name will not fit."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'org-convect)

(defcustom org-convect-heading-width 40
  "Display columns any other rung's name may take before it is worth a word.

Above the areas a heading is a handle: something a goal names in
`CONVECT_SERVES\\=', typed with completion, and read in a report.  Past this
width it has usually stopped being a name and started being the sentence that
belongs underneath it."
  :type 'integer
  :group 'org-convect)

(defcustom org-convect-review-cadence
  '((area . 30)
    (goal . 90))
  "Days after which a horizon is due to be looked at again, by horizon.

The two intervals follow GTD: areas are checked monthly, goals yearly with a
look each quarter.

`vision' and `purpose' are absent on purpose.  GTD puts no interval on them at
all; they are read when direction or motivation has gone, which is a condition
rather than a date.  Inventing a yearly deadline for them would be this
package making up a rule and attributing it to GTD.  What calls them is a
signal instead; see `org-convect-act-drift' for one."
  :type '(alist :key-type symbol :value-type integer)
  :group 'org-convect)

;;;; Reading an entry

(defun org-convect--split (value)
  "Split VALUE on `org-convect-serves-separator' into trimmed names."
  (and value
       (seq-remove #'string-empty-p
                   (mapcar #'string-trim
                           (split-string value org-convect-serves-separator t)))))

(defconst org-convect-review-lead "Reviewed on"
  "What opens a note that says the rung was reviewed.

The one mark in the file, and it is on the *review* rather than on everything
else.  Having looked at a rung is something you declare, not a side effect of
having written on it -- so an ordinary note costs nothing, and
\\[org-add-note] can be used on a rung as freely as anywhere else in Org.

The other way round was the original and it was a trap.  Any inactive stamp in
the entry counted as a look, so a passing thought silenced the rung's review
for a whole cadence, and `org-convect-reword' postponed a review by renaming
something.  Rewording is not reviewing.")

(defconst org-convect-note-lead "Note taken on"
  "What opens an ordinary note.  Org's own wording, so `\\[org-add-note]'
writes exactly what this package writes and neither can tell the other's
notes apart -- which is the point: they are the same thing.")

(defun org-convect--note-re (&optional lead)
  "What the first line of a note looks like, LEAD's kind or any.

Defined once because three readers depend on agreeing about it exactly.
`org-convect--notes' collects them, `org-convect--last-reviewed' looks for one
kind of them, and `org-convect--body' has to leave every one of them out --
and a body that kept a line the others called a note would read that note back
as part of the rung's standard."
  (concat "^[ \t]*- " (if lead (concat (regexp-quote lead) " ") "") ".*?"
          org-ts-regexp-inactive))

(defun org-convect--note-body (bound)
  "Move past the note whose first line point is on, stopping at BOUND.

Returns the region the note's text occupies, as (START . END).

A note's text is the indented lines under its first line.  That is the shape
Org writes, and it is the only one that can be told apart from the prose beside
it: reading to the next `- \=' instead swallows whatever plain text follows,
which in a rung\='s body is its standard.

Three readers walk over notes and all three use this, because a note that ended
in one place for `org-convect--notes\=' and another for `org-convect--body\='
would be a line belonging to both or to neither."
  (forward-line 1)
  (let ((start (point)))
    (while (and (< (point) bound) (looking-at "^[ \t]+[^ \t\n]"))
      (forward-line 1))
    (cons start (point))))

(defun org-convect--note-lead-p (lead)
  "Non-nil when the note the last search matched opens with LEAD."
  (save-excursion
    (goto-char (match-beginning 0))
    (looking-at-p (concat "^[ \t]*- " (regexp-quote lead) " "))))

(defun org-convect--last-reviewed ()
  "When the entry at point was last *declared* reviewed, or nil.

A review is a note (`org-add-note', \\[org-add-note]) opening with
`org-convect-review-lead': \"what happened to this subject\", which is exactly
what a note is for and exactly what a headline is not.  Depending on
`org-log-into-drawer' the note lands in a LOGBOOK drawer or as a plain list
item under the heading; both carry the timestamp, so both are read the same
way here.

Only that note counts.  Everything else written on a rung -- a thought filed
against it, a rename, a stamp inside a sentence -- is a thing you wrote, not a
look you took, and none of it moves the clock.

Only the entry's *own* text counts.  A choice point recorded under a principle
is a child, and recording one is not the same act as reviewing the principle."
  (save-excursion
    (org-back-to-heading t)
    (let ((bound (save-excursion (outline-next-heading) (point)))
          (newest nil))
      (org-end-of-meta-data)
      (when (looking-at org-property-drawer-re)
        (goto-char (match-end 0)))
      (while (re-search-forward (org-convect--note-re org-convect-review-lead)
                                bound t)
        (let ((time (org-time-string-to-time (match-string 1))))
          (when (or (null newest) (time-less-p newest time))
            (setq newest time))))
      newest)))

(defun org-convect--noted-since (when)
  "How many ordinary notes the entry at point carries newer than WHEN.

What has been written against a rung and not yet read.  Counted here rather
than by the board, because the scan is already standing on the entry and a
second walk to count three lines would be a second walk of every file.

Everything before the last review has been read by definition -- that is what
reviewing it was."
  (seq-count (lambda (note)
               (and (eq (nth 1 note) 'noted)
                    (or (null when) (time-less-p when (car note)))))
             (org-convect--notes)))

(defun org-convect--body ()
  "The prose the entry at point carries itself.

Children are excluded, and so is every drawer: what is left is what a person
wrote about this rung, which for an area is the standard the monthly look is
against.  An area with a month of review notes in its LOGBOOK and no standard
above them has still not said what it is.

`org-end-of-meta-data\\=' walks past the blank space after the drawers as well
as the drawers themselves, so on an entry with nothing written it can land
beyond the next heading.  The region is clamped rather than assumed to run
forwards, so the answer stays \"nothing\" instead of depending on which way a
substring happens to be read."
  (save-excursion
    (let ((bound (progn (org-back-to-heading t)
                        (save-excursion (outline-next-heading) (point))))
          parts)
      (org-convect--after-meta bound)
      ;; Every drawer, not only the ones that lead: a drawer is legal anywhere
      ;; under a heading, and one met after the prose used to be read out on
      ;; the board as though somebody had written it into the standard.
      ;;
      ;; And every note, because a note is prose and lands in the body.  Where
      ;; it lands is Org's own setting and the answer people give it differs;
      ;; a rung's body has a job either way, so the notes come out of it
      ;; wherever they were put.  A note's continuation lines are indented, so
      ;; they leave with it.
      (while (< (point) bound)
        (cond ((looking-at org-drawer-regexp)
               (if (re-search-forward "^[ \t]*:END:[ \t]*$" bound t)
                   (forward-line 1)
                 (goto-char bound)))
              ((looking-at (org-convect--note-re))
               (org-convect--note-body bound))
              (t
               (push (buffer-substring-no-properties
                      (point) (min (line-end-position) bound))
                     parts)
               (forward-line 1))))
      (string-trim (string-join (nreverse parts) "\n")))))

(defun org-convect--after-meta (bound)
  "Move past the heading's planning line and every drawer, stopping at BOUND.

Past *every* drawer, not only the properties.  Writing after the first one
puts text between PROPERTIES and LOGBOOK, which splits a run that Org expects
to be unbroken -- and it is easy to do, because `org-end-of-meta-data\\=' lands
before the drawers and the property drawer is the one everybody remembers.

BOUND is where the entry ends.  On a heading with nothing written the walk
lands exactly there -- on the next heading's own line -- which is the right
place to write and the wrong place to read, so the two callers clamp for their
own reasons rather than this one deciding for them."
  (org-end-of-meta-data)
  (while (and (< (point) bound) (looking-at org-drawer-regexp))
    (if (re-search-forward "^[ \t]*:END:[ \t]*$" bound t)
        (forward-line 1)
      (goto-char bound))
    (skip-chars-forward " \t\n"))
  (goto-char (min (point) bound)))

(defun org-convect--entry-at-point (file)
  "Return the horizon entry at point as a plist, or nil if there is none.

FILE is recorded so a finding can say where it came from.  The property is read
without inheritance: a child of an area is not an area, and picking one up
because its parent declared something is the exact mistake that declaring the
ladder rather than locating it is meant to prevent."
  (let ((horizon (org-entry-get nil "CONVECT_HORIZON")))
    (when (and horizon (not (string-empty-p (string-trim horizon))))
      (let ((symbol (intern (string-trim horizon))))
        (list :name     (org-get-heading t t t t)
              :horizon  symbol
              :known    (org-convect-horizon-p symbol)
              :serves   (org-convect--split (org-entry-get nil "CONVECT_SERVES"))
              :by       (let ((stamp (org-entry-get nil "CONVECT_BY")))
                          (and stamp (ignore-errors
                                       (org-time-string-to-time stamp))))
              :reviewed (org-convect--last-reviewed)
              :noted    (org-convect--noted-since (org-convect--last-reviewed))
              :bare     (string-empty-p (org-convect--body))
              :created  (let ((stamp (org-entry-get nil "CREATED")))
                          (and stamp (ignore-errors
                                       (org-time-string-to-time stamp))))
              :file     file
              :marker   (save-excursion (org-back-to-heading t)
                                        (point-marker)))))))

(defun org-convect-scan (&optional files)
  "Read every horizon entry in FILES (default `org-convect-files').

One pass, and the only walk this package makes: everything derived -- findings,
what is due, which rung to prompt next -- is computed from the list this
returns rather than by going back to the files."
  (let (entries)
    (dolist (file (or files org-convect-files))
      (when (and file (file-readable-p file))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-outline-regexp-bol nil t)
             (let ((entry (org-convect--entry-at-point file)))
               (when entry (push entry entries))))))))
    (nreverse entries)))

;;;; Asking the ladder

(defun org-convect-entries (entries horizon)
  "Those of ENTRIES that sit at HORIZON."
  (seq-filter (lambda (e) (eq (plist-get e :horizon) horizon)) entries))

(defun org-convect-name-index (entries)
  "Hash NAME -> list of ENTRIES carrying that name.

A list rather than a single entry, because two rungs sharing a name is the
condition that makes `CONVECT_SERVES' ambiguous, and a hash that kept only the
last one would hide it."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (entry entries index)
      (let ((name (plist-get entry :name)))
        (puthash name (cons entry (gethash name index)) index)))))

(defun org-convect-cadence-days (horizon)
  "Days after which HORIZON is due again, or nil when it has no calendar."
  (alist-get horizon org-convect-review-cadence))

(defun org-convect-overdue-p (entry &optional now)
  "Non-nil when ENTRY is past its review cadence at NOW.

Counted from the last time the entry was looked at, and when it has never been
looked at, from the day it was written.  A rung is not overdue the moment it is
created -- that would make writing one produce a complaint about it -- but it
does come due a cadence later whether or not anything has happened to it.

An entry with neither a review nor a `CREATED\=' stamp is of unknown age, and
due is the honest answer for that.

A horizon with no cadence is never overdue -- not because it never needs
looking at, but because a date is the wrong thing to call it with."
  (let ((days (org-convect-cadence-days (plist-get entry :horizon)))
        (since (or (plist-get entry :reviewed) (plist-get entry :created))))
    (when days
      (or (null since)
          (> (/ (float-time (time-subtract (or now (current-time)) since))
                86400)
             days)))))

(defun org-convect-next-rung (entries)
  "The lowest horizon nothing has been written at yet, or nil when all are
occupied.

GTD's order made mechanical: while areas are unwritten it asks for areas, not
for purpose, which is a question nobody buried in the day can answer honestly.
An empty level is the prescribed path, not a defect; what would be a defect is
never being asked to climb off it.

This asks where to go, not what is unfinished behind you.  Those are two
questions and they have different answers -- a level can be occupied and still
be half-done, and being told to stay there is not useful when the half that is
missing is missing on purpose.  `org-convect-unfinished' answers the other one,
and the `bare-rung' findings name the rungs.

Nothing here gates anything.  The ladder is worked on continually and is
partial at every moment it is any use at all; a tool that refuses to let you
climb until the level below is perfect would be describing a ladder nobody has."
  (seq-find (lambda (horizon) (null (org-convect-entries entries horizon)))
            (mapcar #'car org-convect-horizons)))

(defun org-convect-unfinished (entries)
  "Horizons of ENTRIES holding rungs that are named and nothing more.

The other half of `org-convect-next-rung'.  A heading with nothing written
under it is a noun, and a review of a list of nouns is a re-reading -- so this
is worth surfacing, and worth surfacing *separately*, because leaving a rung
bare is sometimes the right call.  A standard invented to fill a blank is
fiction, and the honest move is often to go up and let a goal say what the
area is being kept up *for*."
  (seq-filter (lambda (horizon)
                (seq-some (lambda (e) (plist-get e :bare))
                          (org-convect-entries entries horizon)))
              (mapcar #'car org-convect-horizons)))

;;;; What is due, and what sits next to it

(defun org-convect-due (entries &optional now)
  "Those of ENTRIES whose review cadence has run out at NOW.

Only the rungs GTD puts a calendar on can appear here.  Vision and purpose
never do, and their absence is the point rather than an oversight: what calls
them is a condition, not a date, and `org-convect-called' is where that is
asked."
  (seq-filter (lambda (e) (org-convect-overdue-p e now)) entries))

(defun org-convect-due-on (entry)
  "When ENTRY next wants looking at, or nil when it has no calendar.

Counted from the last look, or from the day it was written when there has not
been one."
  (let ((days (org-convect-cadence-days (plist-get entry :horizon)))
        (since (or (plist-get entry :reviewed) (plist-get entry :created))))
    (and days since (time-add since (days-to-time days)))))

(defvar org-convect-signal-functions nil
  "Predicates asked whether a rung is being called for, entry plist in hand.

Each returns a string saying why, or nil.  The rungs above the goals have no
cadence -- GTD reads them when direction or motivation has gone, which is a
condition and not a date -- so something has to notice the condition.  Nothing
in the ladder can: what a principle produces is conduct, and conduct is not a
rung.  `org-convect-act' registers the one signal there is evidence for.

With nothing registered this is empty, and the review board says so rather
than pretending the upper rungs are fine.")

(defun org-convect-called (entries)
  "Those of ENTRIES a signal is calling for, as (ENTRY . WHY)."
  (delq nil
        (mapcar (lambda (e)
                  (let ((why (run-hook-with-args-until-success
                              'org-convect-signal-functions e)))
                    (and why (cons e why))))
                entries)))

(defun org-convect-neighbourhood (entries entry)
  "What ENTRY serves and what serves it, as (ABOVE . BELOW).

Altitude decides which rungs come due; this decides what you are looking at
while you review one.  A standard read on its own is a sentence; read beside
the goal it was raised for and the areas that answer to it, it is a judgement
you can actually make."
  (let ((name (plist-get entry :name)))
    (cons (seq-filter (lambda (e) (member (plist-get e :name)
                                          (plist-get entry :serves)))
                      entries)
          (seq-filter (lambda (e) (member name (plist-get e :serves)))
                      entries))))

(defun org-convect-lineage (entries entry)
  "Every rung connected to ENTRY, following the links in both directions.

The board shows the ladder whole, which is what the file already looks like.
This shows one thread of it: what ENTRY was raised for, what that in turn
serves, and everything that answers to any of them.

It is the view the file cannot give.  Links only ever point up, so reading the
file tells you what a rung serves and never what serves it, and a thread has
to be reassembled by hand from both ends."
  ;; Walked by name rather than by object.  A rung's identity here is its name
  ;; -- that is what `CONVECT_SERVES' stores and what the ladder resolves --
  ;; and the entry handed in is often built fresh at point rather than taken
  ;; from ENTRIES, so comparing the plists themselves puts the rung you
  ;; started from into the thread twice.
  (let ((index (org-convect-name-index entries))
        (seen (make-hash-table :test 'equal))
        (queue (list (plist-get entry :name)))
        thread)
    (puthash (plist-get entry :name) t seen)
    (while queue
      (let* ((name (pop queue))
             (here (car (gethash name index))))
        (when here
          (push here thread)
          (dolist (up (plist-get here :serves))
            (unless (gethash up seen)
              (puthash up t seen) (push up queue)))
          (dolist (down entries)
            (when (and (member name (plist-get down :serves))
                       (not (gethash (plist-get down :name) seen)))
              (puthash (plist-get down :name) t seen)
              (push (plist-get down :name) queue))))))
    thread))

(defcustom org-convect-outline-glyphs '(" │ " " ├ " " └ " "   ")
  "The connectors an outline is drawn with: bar, tee, ell, gap.

No horizontal arm, and a space either side.  The arm is what makes a tree look
drawn; without it the line is thin enough to be a margin, which is what it is
-- the rungs are the page and the connectors say where each one sits.

All four must be the same width in display columns, because the width of one
is what a level of depth means.  They are checked at draw time and the ASCII
set is used instead when they are not -- `│\\=' and its family are East Asian
Ambiguous, so a `char-width-table\\=' set for CJK makes them two columns wide
and every column in the tree moves.

Not the glyphs org-foresight draws down the agenda's left edge, though they
are the same characters.  There they bracket a *stretch of hours*, and `├\\='
means a stretch that opens and closes on one row; here they are a descent.
The look is deliberately shared and the meaning is not."
  :type '(list string string string string)
  :group 'org-convect)

(defconst org-convect-outline-ascii '(" | " " + " " ` " "   ")
  "The connectors used when `org-convect-outline-glyphs\\=' will not line up.")

(defun org-convect-outline-glyphs ()
  "The connectors to draw with, in display columns that agree."
  (let ((widths (mapcar #'string-width org-convect-outline-glyphs)))
    (if (apply #'= widths) org-convect-outline-glyphs org-convect-outline-ascii)))

(defun org-convect-outline-under (prefix &optional glyphs)
  "The vertical strip that runs beneath a row whose connector is PREFIX.

The body of that row indents from this, and its children's connectors start
after it.  One function for both, because a body indented by any other measure
drifts out of the tree the moment an ancestor's last child goes past it.

A row's own connector is the last unit of its prefix.  Beneath the row that
connector becomes a bar when the row has later siblings and a gap when it does
not, and every ancestor column is left exactly as it was -- which is the whole
of what continuing a tree downward means."
  (pcase-let ((`(,bar ,tee ,ell ,_gap) (or glyphs (org-convect-outline-glyphs))))
    (cond ((string-suffix-p tee prefix)
           (concat (substring prefix 0 (- (length prefix) (length tee))) bar))
          ((string-suffix-p ell prefix)
           (concat (substring prefix 0 (- (length prefix) (length ell)))
                   (make-string (length ell) ?\s)))
          (t prefix))))

(defun org-convect--outline-index (entries)
  "Hash NAME -> ids into ENTRIES carrying it, in file order.

Deliberately not `org-convect-name-index\\=', whose buckets come out in reverse
file order: it conses onto the front as it walks.  Taking the first of one of
its buckets means the *last* rung of that name in the file, which is the
opposite of the rule the outline is built on."
  (let ((index (make-hash-table :test 'equal))
        (id 0))
    (dolist (entry entries index)
      (let ((name (plist-get entry :name)))
        (puthash name (append (gethash name index) (list id)) index))
      (setq id (1+ id)))))

(defun org-convect--outline-rank (entry)
  "How high ENTRY sits, lowest number highest, for ordering the roots.

A horizon this package does not know sorts last rather than first.  It has no
altitude to compare, and `org-convect-above\\=' answering nil for it would put
a typo above every purpose in the file."
  (let ((horizon (plist-get entry :horizon)))
    (if (org-convect-horizon-p horizon)
        (length (org-convect-above horizon))
      most-positive-fixnum)))

(defun org-convect-outline (entries)
  "ENTRIES as rows of a top-down descent: (ENTRY DEPTH PREFIX ALSO-SERVES).

A purpose, then indented beneath it whatever is in service of it, and so on
down.  ALSO-SERVES names the rungs it serves that it is not drawn under.

The ladder is a mesh rather than a tree -- a rung may serve several, and a link
may skip an altitude -- so a rule is needed for where a rung is *drawn*.  It is
drawn under the first name in its own `CONVECT_SERVES\\=' that resolves to
something else, and among rungs of that name, the first in the file.

That rule and no other, because it is the only one that is local.  Homing by
the order a walk happens to reach a rung, or by which parent comes first in the
file, means adding an unrelated heading at the top moves rows that have nothing
to do with it.  Homing at the highest parent empties the goals: every area that
mentions a purpose leaves the goal it was raised under, and what the goal frames
stops being visible.  This one depends on the rung's own property and nothing
else, so it is stable under every edit elsewhere -- and it is the one a person
can change, by putting the other name first.

The cost is that the order of `CONVECT_SERVES\\=' now means something.  It did
not before.

Every entry comes out exactly once, whatever the file says.  A rung serving
itself is a root; a cycle is broken at its earliest member and the broken link
is named in ALSO-SERVES rather than dropped."
  (let* ((n (length entries))
         (by-id (vconcat entries))
         (ids (org-convect--outline-index entries))
         (home (make-vector n nil))
         (kids (make-vector n nil))
         (also (make-vector n nil))
         (seen (make-vector n nil))
         (glyphs (org-convect-outline-glyphs))
         roots rows)
    ;; Where each rung is drawn, and what it serves besides.  Local to the
    ;; entry: no walk, nothing read from any other rung's answer.
    (dotimes (id n)
      (let (won rest)
        (dolist (name (plist-get (aref by-id id) :serves))
          (let ((found (and (null won)
                            (seq-find (lambda (other) (/= other id))
                                      (gethash name ids)))))
            (if found (setq won found) (push name rest))))
        (aset home id won)
        (aset also id (nreverse rest))))
    (dotimes (id n)
      (when-let ((parent (aref home id)))
        (push id (aref kids parent))))
    (dotimes (id n) (aset kids id (nreverse (aref kids id))))
    (dotimes (id n) (unless (aref home id) (push id roots)))
    ;; Highest first, the way the file reads, and file order within an
    ;; altitude -- `sort' on a list is stable, so no tiebreak is needed.
    (setq roots (sort (nreverse roots)
                      (lambda (a b) (< (org-convect--outline-rank (aref by-id a))
                                       (org-convect--outline-rank (aref by-id b))))))
    (cl-labels
        ((descend (id depth prefix)
           (aset seen id t)
           (push (list (aref by-id id) depth prefix (aref also id)) rows)
           (let* ((children (aref kids id))
                  (last (car (last children)))
                  (strip (org-convect-outline-under prefix glyphs)))
             (pcase-let ((`(,_bar ,tee ,ell ,_gap) glyphs))
               (dolist (child children)
                 (descend child (1+ depth)
                          (concat strip (if (eq child last) ell tee))))))))
      (dolist (root roots) (descend root 0 ""))
      ;; What is left is reachable from no root, which by construction means it
      ;; is in a cycle or hangs off one.  Chase the home chain until it repeats,
      ;; and cut at the *earliest member of the cycle* -- cutting where the
      ;; chase entered would leave the cycle itself unvisited and promote a rung
      ;; that is not in it.
      (dotimes (id n)
        (unless (aref seen id)
          (let ((marked (make-hash-table :test 'eq))
                (path nil)
                (at id))
            (while (not (gethash at marked))
              (puthash at t marked)
              (push at path)
              (setq at (aref home at)))
            (let* ((cycle (seq-take path (1+ (seq-position path at))))
                   (cut (apply #'min cycle))
                   (parent (aref home cut)))
              (aset kids parent (delq cut (aref kids parent)))
              (push (plist-get (aref by-id parent) :name) (aref also cut))
              (aset home cut nil)
              (descend cut 0 "")))))
      ;; Unreachable by the argument above, and kept so that "no rung is
      ;; dropped" is something the code guarantees rather than something the
      ;; comment above claims.
      (dotimes (id n) (unless (aref seen id) (descend id 0 ""))))
    (nreverse rows)))

(defun org-convect-thread-ends-at-p (entry)
  "Non-nil when nothing above ENTRY is named, though something could be.

A purpose is allowed to serve nothing -- there is nothing above it.  Anything
lower that serves nothing is a thread that stops, which is worth seeing when
the question is what a piece of work is finally for."
  (and (null (plist-get entry :serves))
       (org-convect-above (plist-get entry :horizon))
       t))

;;;; Findings

(defvar org-convect-in-use-functions nil
  "Predicates asked whether a rung is in use, called with its entry plist.

The ladder holds commitments, and `unserved-rung' reads it: a rung nothing
below was derived from is shaping nothing.  That reading is sound for a goal
or a vision, whose whole output is the rungs beneath them.

It is unsound for a principle.  A principle's output is not a lower rung -- it
is behaviour in the moment, which the ladder does not contain at all.  So a
principle can shape everything you did today and still have nothing pointing at
it, and something outside the ladder has to be able to say so.

`org-convect-act' registers one: a rung with choice points recorded under it is
in use, whatever the links say.  Remove that feature and the question goes back
to being asked, which is the honest answer without the evidence.")

(defun org-convect-findings (entries &optional now)
  "Everything wrong with the ladder in ENTRIES, as a list of plists.

Each is (:kind :name :horizon :marker :detail).  The kinds:

  unknown-horizon   `CONVECT_HORIZON' names no rung -- a typo, silently
                    invisible to every other question if it were dropped
  duplicate-name    two rungs share a name, so pointing at it means nothing
  unresolved-serves `CONVECT_SERVES' names something that is not there
  misaimed-serves   `CONVECT_SERVES' names a rung that is not above this one.
                    Nothing the commands can produce -- they offer only what
                    sits above -- so it is a link written by hand.  GTD puts no
                    ceiling on how far up a rung may point, and an area serving
                    a purpose directly is ordinary; what has no reading is a
                    rung in service of something at or below its own altitude
  unserved-rung     a rung above the areas that nothing points at.  The one
                    direction worth asking: an upper rung is there to shape
                    what is under it, and one shaping nothing is either not in
                    use yet or not really held.  Areas are exempt -- most serve
                    nothing and that is what they are for
  bare-rung         named, but with nothing written under it -- for an area,
                    no standard, which leaves the monthly look nothing to be a
                    look *at*
  long-heading      a name wide enough to be cut short where it is shown, or
                    wide enough to be a sentence that wanted to be the body.
                    Which of those it is cannot be decided from the text, so
                    the finding reports the width and leaves the reading to you
  undated-goal      a goal with no `CONVECT_BY\='.  \"Roughly when\" is half of
                    what makes an outcome a goal rather than a wish
  past-its-date     a goal whose `CONVECT_BY\=' has gone by and which is still
                    sitting there.  Not a scolding: a goal that outlives its
                    date has either been reached, been abandoned, or was never
                    a goal, and all three want the entry changed
  overdue-review    past the cadence GTD gives that horizon

Note which question is *not* asked: whether a project or an area serves
anything.  Most work is justified on its own -- keeping the engines running is
what an area is for, and the company's chores are a real area of
accountability.  Asking every rung to name a purpose would turn an honest
answer into a defect, so the ladder is only ever read upward."
  (let ((index (org-convect-name-index entries))
        findings)
    (dolist (entry entries)
      (let ((name (plist-get entry :name))
            (horizon (plist-get entry :horizon))
            (marker (plist-get entry :marker)))
        (cl-flet ((finding (kind detail)
                    (push (list :kind kind :name name :horizon horizon
                                :marker marker :detail detail)
                          findings)))
          (unless (plist-get entry :known)
            (finding 'unknown-horizon (format "%s" horizon)))
          (when (cdr (gethash name index))
            (finding 'duplicate-name
                     (format "%d entries" (length (gethash name index)))))
          (dolist (target (plist-get entry :serves))
            (let ((found (gethash target index)))
              (cond
               ((null found) (finding 'unresolved-serves target))
               ;; Both ends have to stand on a rung this package knows before
               ;; their altitudes can be compared, and an unknown one is
               ;; already reported on its own account.
               ((and (org-convect-horizon-p horizon)
                     (not (seq-some
                           (lambda (e)
                             (memq (plist-get e :horizon)
                                   (org-convect-above horizon)))
                           found)))
                (finding 'misaimed-serves
                         (format "%s [%s]" target
                                 (plist-get (car found) :horizon)))))))
          (when (plist-get entry :bare)
            (finding 'bare-rung nil))
          ;; Whether a long heading is a name or a stray sentence cannot be
          ;; decided from the text, and this does not try.  It reports the
          ;; width and says whether anything is written underneath, because a
          ;; long heading over an empty body is the shape content takes when
          ;; there was nowhere else to put it.
          (let* ((width (string-width name))
                 (limit (if (eq horizon 'area)
                            org-convect-area-width
                          org-convect-heading-width)))
            (when (and limit (> width limit))
              (finding 'long-heading
                       (format "%d columns, over %d%s" width limit
                               (if (plist-get entry :bare)
                                   ", and nothing written underneath"
                                 "")))))
          (let ((by (plist-get entry :by)))
            (cond ((and by (time-less-p by (or now (current-time))))
                   (finding 'past-its-date (format-time-string "%Y-%m-%d" by)))
                  ((and (null by) (org-convect-guide horizon :dated))
                   (finding 'undated-goal nil))))
          (when (org-convect-overdue-p entry now)
            (let ((reviewed (plist-get entry :reviewed))
                  (created (plist-get entry :created)))
              (finding 'overdue-review
                       (cond (reviewed (format-time-string "%Y-%m-%d" reviewed))
                             (created (concat "written "
                                              (format-time-string "%Y-%m-%d" created)))
                             (t "never"))))))))
    ;; Everything above the areas, not only the goals.  A rung up here exists
    ;; to shape the ones below it -- a goal frames areas, a vision creates
    ;; goals, a purpose drives the vision -- so one that shapes nothing is
    ;; either not being used yet or is not really held.  The areas are exempt
    ;; and stay exempt: keeping the engines running is what an area is for.
    (dolist (entry entries)
      (let ((horizon (plist-get entry :horizon))
            (name (plist-get entry :name)))
        (when (and (org-convect-horizon-p horizon)
                   (not (eq horizon 'area))
                   ;; from below, which is what the finding says.  A link
                   ;; running the other way is broken rather than an answer,
                   ;; and letting it count would silence a real gap.
                   (not (seq-some
                         (lambda (e)
                           (and (member name (plist-get e :serves))
                                (memq horizon (org-convect-above
                                               (plist-get e :horizon)))))
                         entries))
                   (not (run-hook-with-args-until-success
                         'org-convect-in-use-functions entry)))
          (push (list :kind 'unserved-rung :name name :horizon horizon
                      :marker (plist-get entry :marker) :detail nil)
                findings))))
    (nreverse findings)))

;;;; Where the time actually went

(defcustom org-convect-clock-window 30
  "Days of clock history read when no scan is supplied.
Matched to the areas cadence: a monthly check-in asks about the month."
  :type 'integer
  :group 'org-convect)

;; Declared so the binding below is dynamic.  org-foresight is a soft
;; dependency and may not be loaded when this file is compiled, and `let' on a
;; symbol the compiler has never seen made special produces a lexical binding
;; -- which would leave the scan grouping by its own default and this function
;; quietly answering the wrong question.
(defvar org-foresight-clock-property)

(defun org-convect-clock-rows (&optional scan)
  "Clocked minutes per area, as (AREA . MINUTES), with \"?\" for the rest.

SCAN is a plist from `org-foresight-clock-scan'; one is fetched when that
package is loaded and none is given.  The dependency is soft both ways: the
ladder reads without a clock and the clock reads without a ladder.

The scan is asked to group by `CONVECT_AREA' rather than by its usual CATEGORY
-- one walk, a different question.  Entries carrying no area land under \"?\"."
  (or scan
      (and (fboundp 'org-foresight-clock-scan)
           (let ((org-foresight-clock-property "CONVECT_AREA"))
             (org-foresight-clock-scan org-convect-clock-window)))))

(defun org-convect-unclaimed-time (entries &optional scan)
  "Clocked time that no area accounts for, as two lists.

Returns (UNATTRIBUTED . UNKNOWN), each an alist of (AREA . MINUTES):

  UNATTRIBUTED  hours clocked against entries carrying no area at all.  This
                is the question the whole package exists for -- time went
                somewhere, and nothing claims to be answerable for it
  UNKNOWN       hours booked to a name that is not a declared area, which is
                a typo rather than a finding about your life

Neither is an accusation.  A chore that shows up unattributed every month is
usually an area of accountability that was never named, and naming it is the
fix."
  (let* ((rows (plist-get (org-convect-clock-rows scan) :rows))
         (declared (mapcar (lambda (e) (plist-get e :name))
                           (org-convect-entries entries 'area))))
    (cons (seq-filter (lambda (row) (equal (car row) "?")) rows)
          (seq-remove (lambda (row) (or (equal (car row) "?")
                                        (member (car row) declared)))
                      rows))))

;;;; Input

(defun org-convect--candidate (entry)
  "ENTRY as a completion candidate: its name, and the rung it stands on.

The name alone is not enough where it matters most.  Picking what something is
in service of is a choice between altitudes as much as between names -- two
rungs can be worded alike, and which one you meant is the difference between a
principle and the goal that serves it."
  (format "%s  [%s]" (plist-get entry :name) (plist-get entry :horizon)))

(defun org-convect--chosen (pick table)
  "The name PICK stands for in TABLE, or PICK itself when it stands for nothing.

The prompts that use TABLE do not require a match, because a link may be
written before the rung it points at exists and refusing that would make the
order of writing matter.  What is typed freely comes back unchanged."
  (or (cdr (assoc pick table)) pick))

(defun org-convect-read-entry (prompt &optional entries horizons)
  "Read one horizon entry with completion and return its plist.

ENTRIES defaults to a fresh scan.  HORIZONS, when given, restricts the
candidates to those rungs.  The entry at point, when it is one, is the default,
so recording something about the rung already on screen takes no typing."
  (let* ((entries (or entries (org-convect-scan)))
         (entries (if horizons
                      (seq-filter (lambda (e)
                                    (memq (plist-get e :horizon) horizons))
                                  entries)
                    entries))
         (here (and (derived-mode-p 'org-mode)
                    (org-at-heading-p)
                    (org-convect--entry-at-point (buffer-file-name))))
         (default (and here (seq-find (lambda (e)
                                        (equal (plist-get e :name)
                                               (plist-get here :name)))
                                      entries)))
         (table (mapcar (lambda (e) (cons (org-convect--candidate e) e))
                        entries)))
    (unless table
      (user-error "No horizon entries yet -- see `org-convect-open'"))
    (cdr (assoc (completing-read
                 prompt table nil t nil nil
                 (and default (car (rassq default table))))
                table))))

;;;; What to write, and how to find yours

(defcustom org-convect-horizon-guide
  '((purpose
     :prompt "Principle or purpose"
     :hint   "what you hold to even when holding to it costs something"
     :what   "Why any of this matters, and the standards you keep whatever the
outcome.  These never finish and are never scored."
     :find   "Ask what the work is finally for.  Then, separately, ask what you
would refuse to do even when refusing costs you something.  The second question
is usually the easier one, and it is half the answer."
     :write  "One heading per principle -- not a numbered list under this one.
A principle is pointed at by name, reviewed on its own and has choice points
recorded under it, and none of that can happen to a line in a list.

Under each heading, in ordinary prose, what it rules out: the behaviour you
would refuse even at a cost.  A principle with nothing it forbids is a slogan,
and nothing can be measured against it."
     :shape  "  ** I do not let a number stand that I know is wrong
     Rules out: staying quiet in a review because the meeting is
     nearly over.  Rules out: repeating a figure I have not checked
     because someone senior said it first."
     :test   "Three questions, and the third is the one that decides.

Does it finish?  A principle never does.  Is it something you do, or something
that is?  A principle is something you do.  And: if the reason for it turned
out to be wrong, would you still hold to it?  If yes it belongs here; if no it
is an instrument, and instruments live lower down.

Having a reason does not make it an instrument -- a principle with no reason is
merely arbitrary.  The test is whether the reason is load-bearing."
     :examples "Yes: \"I do not let a number stand that I know is wrong.\"  It
never finishes, it is a way of behaving, and it holds even when speaking up
costs something.

No: \"Become the person the team trusts.\"  That is a state to arrive at rather
than a way of behaving, which makes it a vision."
     :when   "No schedule.  Read these when direction or motivation has gone."
     :review "Am I still behaving like this?  A principle does not go out of
date, so asking whether it is current asks nothing -- the question that carries
is whether it is being kept, and where it was not."
     :note   "Choice points are usually recorded under these, because a value
in ACT is a way of behaving and so is a principle.  They are not confined to
them: an area can be the honest answer when the pull was away from a
responsibility rather than from a value.

A rung with choice points under it counts as in use even when nothing in the
ladder points at it.  What a principle produces is conduct, not a lower rung,
so the ladder is the wrong place to look for its output."
     :do     "Add one with M-x org-convect-add, or write several as child
headings here, mark them, and run M-x org-convect-declare.  Then open each and
write what it rules out.")
    (vision
     :prompt "Vision"
     :hint   "the finished state, written as though already true"
     :what   "What it is like once it has worked.  Some years out, and concrete
enough to picture rather than agree with."
     :find   "Describe the finished state as though it were already the case.
What is different, who notices, how an ordinary day goes."
     :write  "A heading, then the picture under it.  The heading is only a
handle -- something short enough to point at from a goal -- and the body is
where the picture goes.  This is the one rung whose body is meant to run long,
but it still needs the heading, because loose prose under the section is not a
rung and nothing can be said about it."
     :shape  "  ** The team ships without me in the room
     A release goes out on a Thursday and I hear about it afterwards.
     The review that catches the bad number is somebody else's, and it
     catches it.  Nobody waits to be told it is safe to say so."
     :test   "Can you write it in the present tense, as though it were already
the case?  A vision reads that way and a goal does not.

And does it stop when you get there?  A vision is approached rather than
finished.  If it has a date and a test for having arrived, you have written a
goal."
     :examples "Yes: \"A day goes by and nobody needed me in the room.\"  A
state, describable as though already true, with no date attached.

No: \"Hand the on-call rota over by March.\"  Dated and completable, so it is a
goal -- and probably one that serves the vision above it."
     :when   "No schedule.  Rewrite it when the picture stops fitting."
     :review "Is this still the picture, and has anything actually moved toward
it this year?  A vision nothing has moved toward is either not held or not
being worked from."
     :do     "Add one with M-x org-convect-add, then write the picture
underneath it.")
    (goal
     :dated  t
     :prompt "Goal"
     :hint   "an outcome you could tell you had reached, a year or two out"
     :what   "What has to become true within a year or two for the vision to be
on its way.  Unlike the rungs above it, a goal finishes."
     :find   "Name an outcome you could tell you had reached, and roughly when.
If you cannot say what would count as having reached it, it is still a vision.

There are fewer of these than there are areas, and they cut across them.  Do
not go down the list of areas looking for one each -- that is the commonest
way this level goes wrong.  One goal usually touches several areas, and most
areas have no goal at all at any given moment.

A goal appears when something has to become *different*: a standard has to
rise, or something is in the way of meeting one, or a thing that does not exist
yet is wanted.  So read the areas for pressure rather than for material.

The goals themselves usually come from somewhere else, and usually they are
already written down: this year's objectives, a plan someone is holding you to,
the thing you told your family you would do.  Copy them, the way the areas were
copied."
     :write  "Under each one, as plain lines, how you would know it had
happened.  Nothing else -- no label, no date in the text.  A goal you cannot
judge is a vision that has been given a date."
     :shape  "  ** Two other people review releases, unprompted
     Reached when: two releases in a row go out with a review I did not
     ask anyone for."
     :test   "Can you say what would count as having reached it, and roughly
when?

If you cannot say what counts, it is still a vision.  If it never finishes at
all, it is an area you maintain or a principle you hold."
     :examples "Yes: \"Vo2max of 55 by the end of the year.\"  There is a
number and a date, and on the day it is true you are done.

No: \"Stay fit.\"  Nothing about that finishes, which makes it an area."
     :when   "Yearly, with a look each quarter."
     :review "Reached, still reachable, or no longer a goal?  All three are
answers; leaving it sitting there is not."
     :note   "The date a goal is for and the date it is next looked at are two
different dates, and only the first of them is a property.

`CONVECT_BY' is the goal's own target: when it is meant to be true.  It is a
property so that it can be read rather than noticed; M-x org-convect-set-date
writes it, or answer the prompt when adding one.  A goal still sitting there
after that date has been reached, abandoned, or was never a goal, and the
review says so.

The quarterly look is an interval rather than a date -- 90 days, in
`org-convect-review-cadence' -- counted from the last note written on the
entry, or from the day it was written when there is none.  So looking at a
goal is what postpones the next look at it, and a target two years out does
not buy two years of quiet.

Links only ever run upward.  The property is `CONVECT_SERVES', it sits on the
lower rung and names the higher one, and there is none pointing the other way:
two directions would be two answers that could disagree, and nothing would say
which was right.  Writing it is not confined to the lower end, though --
standing on the goal, where you know which areas it changes, C-u M-x
org-convect-link writes it onto the areas you pick."
     :do     "Add one with M-x org-convect-add, or write several as child
headings here, mark them, and run M-x org-convect-declare.  Then, on each:
write how you would know it had happened, set the date with M-x
org-convect-set-date, and point the areas it changes at it with C-u M-x
org-convect-link.")
    (area
     :prompt "Area"
     :hint   "copy it off your job description, or off who does what at home"
     :what   "Something you are answerable for that never finishes.  It is held
to a standard rather than completed, and letting one slip is how things quietly
stop working.

Name the role or the function, never the thing it concerns.  \"Children\" is
not an area -- there is no way to keep children to a standard.  \"Parent\" is:
there is a way you mean to do it, and you can tell when you are not.  The test
is whether the words \"kept up\" attach to it at all.  At work the organisation
chart has usually named the functions already, so they read like functions
(procurement, engineering); at home nobody has named anything, so you have to
say what you are answerable for, and they read like roles."
     :find   "Do not invent these.  Copy them off your job description, the
organisation chart, and who does what at home.  Most of yours are already
written down somewhere, and reading them off is faster and more honest than
thinking them up."
     :write  "Under each one, as plain lines, the standard: what \"kept up\"
means here.  The test has a clock in it, and asking it that way is what keeps
the answer at this altitude --

  If this had slipped this month, what would I see?

Answer with things you would notice, not with why the area matters.  For a
parent: there is time with them alone each week; the bedtime conversation
still happens; they tell me about their day unprompted; I have not shouted.
For a codebase: reviews come back the same day; no branch is older than a
week; the build is green when I leave.

If what comes out instead is why it matters, or a picture of how it turns out
years from now, that is not a bad answer -- it is a good answer to a different
rung.  Move it up to Purpose or Vision and ask the question again with the
clock in it.  Being asked for a standard and producing a purpose is the most
common thing that happens here."
     :shape  "  ** engineering
     Kept up means: reviews come back the same day; no branch is older
     than a week; the build is green when I leave."
     :test   "Does it finish?  It must not.  Do the words \"kept up\" attach to
it?  They must.  Are you answerable for it?  You must be.

If it finishes it is a goal or a project.  If it is a way of behaving rather
than a thing held to a standard, it is a principle."
     :examples "Yes: \"engineering\", kept up meaning reviews come back the same
day and no branch is older than a week.

No: \"Ship the migration.\"  It finishes, so it is a project -- it belongs in
the task system rather than here."
     :when   "Monthly, and whenever the job or the household changes."
     :review "Is the standard being met -- and is this still yours to keep up?
GTD asks the second out loud: should you be answerable for this at all, and
could it be delegated or dropped?  A standard that is being met perfectly is
still worth losing if it was never yours."
     :note   "An area's name is what a task carries in `CONVECT_AREA', which is
how the clock reports against it.  The property is read with inheritance, so
marking a project marks everything under it -- but a task filed straight into
a date tree has no project to inherit from and carries its own.

Deliberately not CATEGORY.  That slot is Org's, every entry already has one,
and other packages read it for their own purposes; taking it would mean
telling you what your own categories have to say.

Nothing here needs to point at a goal.  Most areas never will -- keeping the
engines running is what they are for, and a chore is a real accountability.
Only the other direction is ever asked about: a goal nothing serves."
     :do     "Write the names as child headings under this one -- one line
each, no properties -- then mark them and run M-x org-convect-declare.  Then
open each and write its standard.  For a single one, M-x org-convect-add."))
  "What each horizon is, how to find your own, and what to write under it.

Used in two places and written once: the `:GUIDE' drawers in a fresh file are
built from it, and so are the prompts `org-convect-add' asks.  Keeping the
substance in one place is what stops a file written years ago from disagreeing
with the command being run today.

The fields are listed, labelled and ordered by `org-convect-guide-fields'.

`:write' is the one that gets forgotten.  A rung is a heading so that it can go
on accumulating, and a file full of bare headings is a file where the review
has nothing to be a review *of*.

`:shape' is the one that answers fastest.  Everything else describes a rung and
it describes one, so a reader who has followed every paragraph can still be
unsure whether the standard goes in the heading or underneath.  Being shown one
settles that without a sentence.  It is written out verbatim, so its line breaks
are the content -- see `org-convect-guide-verbatim-fields' for the one thing it
may not contain.

`:prompt' and `:hint' are the minibuffer's share of the same text.

Everything here is written from scratch.  GTD's altitude names are kept exactly
because they are the interface to everything written about the model, but the
descriptions are not the David Allen Company's text."
  :type '(alist :key-type symbol :value-type plist)
  :group 'org-convect)

(defconst org-convect-guide-fields
  '((:what     "What this is")
    (:find     "Where to find yours")
    (:write    "What goes underneath")
    (:shape    "One written out"
               "The heading and the body, which is all anybody types.  The
properties are the commands' work, and are left out so that nothing in the
example looks like something to copy by hand.")
    (:test     "The test")
    (:examples "Telling them apart")
    (:when     "How often")
    (:review   "What to ask when you come back")
    (:note     "How the file works")
    (:do       "What to type"))
  "The parts of a guide drawer in reading order, as (FIELD LABEL [GLOSS]).

The labels are the point.  Nine fields answering nine different questions used
to arrive as nine paragraphs that looked exactly alike, leaving the reader to
work out which question each one was answering -- which is the writer's work,
not the reader's.  Three words at the front of a paragraph do it.

The order is a teaching order.  A test is something applied to what you have,
so it comes after there is something to apply it to: what this is, where to find
yours, what goes underneath, one written out, and only then the test.

The labels also mark a change of register, which is the other thing that made
these drawers hard to read.  Everything up to `:when' is about the thinking:
what you are being asked for and how to tell whether you have it.  `:note' is
about the file -- which property holds what, which command writes it, what the
package will and will not do.  Mixing the two leaves the reader unable to say
whether a sentence is advice or a rule, so mechanism is confined to the field
labelled for it.

GLOSS is wording that belongs to the field itself rather than to any one
horizon, and so has nowhere in `org-convect-horizon-guide' to live.")

(defconst org-convect-guide-verbatim-fields '(:shape)
  "Guide fields whose line breaks are the content and must not be refilled.

An example of a rung is Org text, and Org text reflowed to 72 columns is no
longer an example of anything.

What such a field may not contain is a line reading `:END:', indented or not.
Org ends a drawer at the first one it finds, so it would close the `:GUIDE:'
drawer around it and spill the rest of the guidance into the entry's body.
That rules out showing a property drawer in an example -- which is no loss,
since the properties are written by the commands rather than typed by hand.")

(defun org-convect-guide (horizon field)
  "The FIELD of HORIZON's entry in `org-convect-horizon-guide'."
  (plist-get (alist-get horizon org-convect-horizon-guide) field))

(defun org-convect--guide-field (horizon field)
  "HORIZON's FIELD as it appears in the guide drawer, or nil when it has none.

The label is set into the same paragraph as the answer, so the question arrives
first and costs no line of its own.  Its full stop goes outside the emphasis:
`.*' is not a sentence end to `fill-region', which would then close the two
spaces after the label to one and set it apart from every other sentence.

A verbatim field gets its label on a paragraph of its own, because running an
example into prose would be running prose into the example."
  (let ((text (org-convect-guide horizon field))
        (entry (assq field org-convect-guide-fields)))
    (when (and entry (org-string-nw-p text))
      (let* ((gloss (nth 2 entry))
             (lead (concat "*" (nth 1 entry) "*."
                           (and gloss (concat "  " gloss)))))
        (if (memq field org-convect-guide-verbatim-fields)
            (concat (org-convect--fill lead) "\n\n" (string-trim-right text))
          (org-convect--fill (concat lead "  " text)))))))

(defun org-convect--one-line (horizon &optional field)
  "The first sentence of HORIZON's FIELD (default `:what').

Short enough for a completion annotation or the echo area, which is where a
command has to teach if it is going to teach at all."
  (let ((what (or (org-convect-guide horizon (or field :what)) "")))
    (replace-regexp-in-string
     "\n" " " (if (string-match "\\`\\([^.]*\\.\\)" what) (match-string 1 what) what))))

;;;; The one thing that points the other way

(defun org-convect-area-names (&optional entries)
  "The names of the declared areas, which are the categories tasks may carry."
  (mapcar (lambda (e) (plist-get e :name))
          (org-convect-entries (or entries (org-convect-scan)) 'area)))

;;;###autoload
(defun org-convect-read-area (&optional prompt)
  "Read one declared area by name and return it.

Meant for a capture template, through `%(org-convect-read-area)': the
candidates are the areas that actually exist, so a task cannot be filed
against a responsibility nobody has claimed.  Returns the empty string when
nothing is picked, which a template renders as no area at all."
  (let ((names (org-convect-area-names)))
    (if (null names)
        ""
      (completing-read (or prompt "Area: ") names nil nil))))

;;;###autoload
(defun org-convect-set-area ()
  "Put a declared area's name in the `CONVECT_AREA\\=' of the entry at point.

The only binding that runs downward, and it is not a pointer: the property is
read with inheritance, so marking a project marks everything under it and the
task itself carries nothing and is asked nothing.  That is what keeps the
ladder out of the way of a five-minute job.

It is deliberately not CATEGORY.  CATEGORY is Org's own, every Org user
already has one on every entry, and other packages read it for their own
purposes -- org-calsync writes what a thing *is* there, org-foresight asks
which of *your* values mean private.  Those ask; prescribing that CATEGORY
must name an area would take a slot that was never this package's to take.
Which area a task serves is a different question from what kind of thing it
is, and both are needed at once, so they need two slots.

It is the binding the clock reports through.  Until an entry carries an area,
its hours are unattributed -- which `org-convect-unclaimed-time' reports, and
which is the point: time that went somewhere nothing claims to care about.

Works on the entry at point in an Org buffer, or on the entry behind the line
in an agenda -- which is where most of this gets decided."
  (interactive)
  (let ((marker (if (derived-mode-p 'org-agenda-mode)
                    (or (org-get-at-bol 'org-hd-marker)
                        (user-error "No entry on this line"))
                  (point-marker))))
    (org-with-point-at marker
      (org-back-to-heading t)
      (let ((area (org-convect-read-area
                   (format "Area for \"%s\": "
                           (truncate-string-to-width
                            (org-get-heading t t t t) 40)))))
        (if (org-string-nw-p area)
            (org-entry-put nil "CONVECT_AREA" area)
          (org-entry-delete nil "CONVECT_AREA"))
        (save-buffer)
        (message (if (org-string-nw-p area)
                     (format "Filed under %s" area)
                   "Area cleared"))))))

;;;; The file itself

(defun org-convect--fill (text &optional columns)
  "TEXT wrapped to COLUMNS, or to a width that reads in a narrow window.

The source strings are broken where the source is easiest to read, which is
not where the file should break.  Joining and refilling keeps the two apart:
a sentence end keeps its two spaces, every other line break becomes one space.

Two things in the source do mean what they say and survive.  A blank line is a
paragraph, so each paragraph is filled on its own.  And a paragraph indented in
the source stays indented, which is how a question meant to be asked on its own
goes on looking like one instead of joining the prose around it."
  (mapconcat
   (lambda (paragraph)
     (let ((indent (if (string-match "\\`[ \t]+" paragraph)
                       (match-string 0 paragraph)
                     "")))
     (with-temp-buffer
       (insert indent
               (replace-regexp-in-string
                "\n[ \t]*" " "
                (replace-regexp-in-string "\\([.?!]\\)\n[ \t]*" "\\1  "
                                          (string-trim paragraph))))
       (let ((fill-column (or columns 72))
             (fill-prefix indent)
             ;; Off, because a paragraph here may open with a bold label and
             ;; `adaptive-fill-regexp' counts a leading `*' as a bullet: every
             ;; line after the first would be indented under a list that is not
             ;; there.
             (adaptive-fill-mode nil))
         (fill-region (point-min) (point-max)))
       (buffer-string))))
   (split-string (string-trim text) "\n[ \t]*\n" t)
   "\n\n"))

(defun org-convect--guide-drawer (horizon)
  "HORIZON's guidance as a `:GUIDE:' drawer.

A drawer rather than plain prose because Org folds one by default: it is a
single line until wanted, and deleting it once the rung is understood costs
nothing, since nothing reads it back.  What it does *not* hold is the mechanics
of writing an entry -- those live in `org-convect-add', which cannot go stale
the way a file written once at creation can."
  (concat ":GUIDE:\n"
          (mapconcat #'identity
                     (delq nil (mapcar (lambda (entry)
                                         (org-convect--guide-field horizon (car entry)))
                                       org-convect-guide-fields))
                     "\n\n")
          "\n"
          ":END:\n"))

(defcustom org-convect-columns
  '("%40ITEM(Item)" "%CONVECT_HORIZON(Horizon)" "%CONVECT_SERVES(Serves)")
  "Columns written into a fresh horizons file's `#+COLUMNS\=' line.

A list rather than a line so that a layer above can add its own without
replacing the whole frame: `org-convect-act\=' appends its two, and removing
that feature removes them again.

The line is what makes the property drawers worth writing at all -- with it,
\\[org-columns] over a rung is the table of what is underneath it, so nothing
has to be stored twice."
  :type '(repeat string)
  :group 'org-convect)

(defun org-convect--preamble ()
  "The Org comment block at the head of a horizons file.

Comment lines rather than prose: they are addressed to whoever opens the file
and are not part of what the file records, and `#\\=' is how Org says so."
  (concat "# Read down: purpose shapes the vision, the vision the goals,\n"
          "# the goals the areas.  Write up: start from what you already\n"
          "# carry.  A rung with nothing in it yet is normal.\n"
          "#\n"
          "# Add one with M-x org-convect-add.  Write or paste several under a\n"
          "# section and mark them with M-x org-convect-declare.  Open a :GUIDE:\n"
          "# drawer with TAB, and delete it once it is in the way; M-x\n"
          "# org-convect-refresh-guides puts the current wording back.\n"
          "#\n"
          "# M-x org-convect-doctor shows what is still blank.  A heading with\n"
          "# an empty body looks exactly like a heading, so the file cannot.\n"))

(defun org-convect--build-skeleton ()
  "The frame written into a fresh horizons file.

Ordered highest first, which is the direction the ladder is *read*: purpose
shapes the vision, the vision shapes the goals, the goals frame the areas.  It
is deliberately the reverse of `org-convect-horizons', which stays lowest first
because that is the direction the ladder is *written* and is what
`org-convect-next-rung' walks.  Both orders are real, and collapsing them into
one would lose half the model."
  (concat "#+title: Horizons\n"
          "#+COLUMNS: " (string-join org-convect-columns " ") "\n\n"
          (org-convect--preamble) "\n"
          (mapconcat
           (lambda (horizon)
             (format "* %s\n:PROPERTIES:\n:CONVECT_SECTION: %s\n:END:\n%s\n"
                     (org-convect-horizon-name horizon) horizon
                     (org-convect--guide-drawer horizon)))
           (reverse (mapcar #'car org-convect-horizons)) "")))

(defcustom org-convect-skeleton nil
  "Written into the first of `org-convect-files' when it does not exist.

Nil means build it from `org-convect-horizon-guide' and `org-convect-columns'
at the moment the file is created, which is what makes customising either of
those take effect.  A string here replaces the frame entirely.

Sections are scaffolding.  An entry is found by `CONVECT_HORIZON', so moving
one out of its section changes nothing; `CONVECT_SECTION' marks a section only
so `org-convect-add' knows where to put things, and is not itself a rung.

What this creates is the frame and the guidance, not a suggestion of what to
put in it.  The one piece of advice that matters for areas is to copy them off
documents that already describe your responsibilities, and a template guessing
at them would get in the way of that inventory."
  :type 'string
  :group 'org-convect)

(defun org-convect--frame ()
  "Write the frame into this buffer when it is empty."
  (when (= (point-min) (point-max))
    (insert (or org-convect-skeleton (org-convect--build-skeleton)))))

;;;###autoload
(defun org-convect-open ()
  "Open the horizons file, writing its frame the first time."
  (interactive)
  (let ((file (car org-convect-files)))
    (unless file (user-error "`org-convect-files' is empty"))
    (find-file file)
    (save-excursion (org-convect--frame))
    (when (buffer-modified-p) (save-buffer))
    (goto-char (point-min))))

(defun org-convect--replace-guide (horizon)
  "Put HORIZON's current guidance under the section heading at point."
  (org-back-to-heading t)
  (org-end-of-meta-data)
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0)))
  (let ((bound (save-excursion (outline-next-heading) (point))))
    ;; out with the old, if there is one
    (save-excursion
      (when (re-search-forward "^[ \t]*:GUIDE:[ \t]*$" bound t)
        (let ((start (match-beginning 0)))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" bound t)
            (delete-region start (min (1+ (match-end 0)) (point-max)))))))
    (unless (bolp) (insert "\n"))
    (insert (org-convect--guide-drawer horizon))))

;;;###autoload
(defun org-convect-refresh-guides ()
  "Rewrite the guidance in the ladder file and leave everything else alone.

Guidance written into a file once, at creation, is guidance that cannot learn
anything afterwards -- and this package has already taught it two things it
did not know on the first day.  So the drawers are replaceable: this puts the
current wording back under every section, adds one where it was deleted, and
touches nothing else.  Rungs, their properties, their notes and their children
are not read and not moved."
  (interactive)
  (let ((refreshed 0))
    (dolist (file org-convect-files)
      (when (and file (file-readable-p file))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           ;; the comment block at the head, which names the commands
           (when (re-search-forward "^#\\+COLUMNS:.*$" nil t)
             (forward-line 1)
             (let ((start (point)))
               (while (looking-at "^\\(#.*\\)?$") (forward-line 1))
               (delete-region start (point))
               (insert "\n" (org-convect--preamble) "\n")))
           (goto-char (point-min))
           (while (re-search-forward org-outline-regexp-bol nil t)
             (let ((section (org-entry-get nil "CONVECT_SECTION")))
               (when (and section (org-convect-horizon-p (intern section)))
                 (org-convect--replace-guide (intern section))
                 (cl-incf refreshed)))))
          (save-buffer))))
    (message "Refreshed guidance under %d section%s"
             refreshed (if (= refreshed 1) "" "s"))
    refreshed))

(defun org-convect--rung-at-point ()
  "The rung at point as a plist, or nil when point is not on one.

In an Org buffer that is the heading.  In a board -- the review, a lineage,
the doctor -- it is the entry the row stands for, read through the marker the
row carries.  Boards are where most of this gets decided, so a command that
only worked in the file would be one you had to leave the board to run."
  (cond
   ((derived-mode-p 'org-mode)
    (and (ignore-errors (org-back-to-heading t) t)
         (org-convect--entry-at-point (buffer-file-name))))
   ((derived-mode-p 'org-agenda-mode)
    (let ((marker (org-get-at-bol 'org-hd-marker)))
      (and (markerp marker) (marker-buffer marker)
           (org-with-point-at marker
             (org-convect--entry-at-point (buffer-file-name))))))))

;;;###autoload
(defun org-convect-set-date (&optional date)
  "Set `CONVECT_BY\\=' on the rung at point, or clear it with an empty answer.

Only goals carry one.  The rungs above have no calendar -- GTD gives them
none, and a date on a purpose would be this package inventing a rule -- and an
area is maintained rather than reached, so a date on one would be asking when
you intend to stop being answerable for it."
  (interactive)
  (let ((rung (or (org-convect--rung-at-point) (user-error "Not on a rung"))))
    (unless (org-convect-guide (plist-get rung :horizon) :dated)
      (user-error "A %s carries no date" (plist-get rung :horizon)))
    (let ((date (or date (org-read-date nil nil nil "Reached by"))))
      ;; through the marker rather than at point: the rung may have been read
      ;; from a board row, and point is then in the board.
      (org-with-point-at (plist-get rung :marker)
        (if (org-string-nw-p date)
            (org-entry-put nil "CONVECT_BY" (format "[%s]" date))
          (org-entry-delete nil "CONVECT_BY"))))))

;;;###autoload
(defun org-convect-link (&optional downward)
  "Record what the rung at point is in service of.

Links run one way only: a rung names what it serves, and nothing names what
serves it.  That is what keeps a five-minute task free of the ladder, and it
is why an area that serves nothing is never a defect while a goal nothing
serves is worth asking about.

So the link for \"this goal changes what these three areas are for\" is stored
on the three areas, not on the goal.  With DOWNWARD (\\[universal-argument]),
that is what this writes: it asks which lower rungs should point at the one
here and puts the link on each of them.  The direction of the data does not
change -- only the end you are standing at when you say it, which for a goal
is the end where you know the answer."
  (interactive "P")
  (let* ((entries (org-convect-scan))
         (here (or (org-convect--rung-at-point) (user-error "Not on a rung")))
         (name (plist-get here :name))
         (horizon (plist-get here :horizon)))
    (if downward
        (let* ((below (seq-filter
                       (lambda (e) (memq horizon (org-convect-above
                                                  (plist-get e :horizon))))
                       entries))
               (table (mapcar (lambda (e) (cons (org-convect--candidate e) e))
                              below))
               (chosen (and below
                            (completing-read-multiple
                             (format "Rungs served by %s: " name) table)))
               (written 0))
          (unless below (user-error "Nothing sits below a %s" horizon))
          (dolist (pick (seq-remove #'string-empty-p chosen))
            ;; only what the table knows: this end writes onto another rung,
            ;; and a name typed freely has no rung to write onto
            (when-let ((entry (cdr (assoc pick table))))
              (org-convect--add-serves (plist-get entry :marker) name)
              (cl-incf written)))
          (message "%s is now served by %d rung%s" name written
                   (if (= 1 written) "" "s")))
      (let* ((above (seq-filter (lambda (e)
                                  (memq (plist-get e :horizon)
                                        (org-convect-above horizon)))
                                entries))
             (table (mapcar (lambda (e)
                              (cons (org-convect--candidate e)
                                    (plist-get e :name)))
                            above))
             (chosen (and above
                          (completing-read-multiple
                           (format "%s is in service of: " name) table))))
        (unless above (user-error "Nothing sits above a %s" horizon))
        (dolist (pick (seq-remove #'string-empty-p chosen))
          (org-convect--add-serves (plist-get here :marker)
                                   (org-convect--chosen pick table)))
        (message "%s now serves %d rung%s" name (length chosen)
                 (if (= 1 (length chosen)) "" "s"))))))

;;;###autoload
(defun org-convect-relink ()
  "Repoint or drop one of the links written on the rung at point.

`org-convect-link' only ever adds, which is right for the ordinary case: saying
what something is in service of is no reason to disturb what it already serves.
It leaves nothing able to repair a name that has stopped resolving, though, and
that is exactly what `unresolved-serves' reports -- a typo, or a rung renamed
without its references following.  Neither is fixed by adding a second name
beside the broken one, and `org-convect-reword' renames a rung rather than the
link that points at it.

Offers the links this rung carries, marking the ones that resolve to nothing,
and asks what to point at instead.  An empty answer drops the link, which is
the honest repair when the thing it named is gone."
  (interactive)
  (let* ((entries (org-convect-scan))
         (here (or (org-convect--rung-at-point) (user-error "Not on a rung")))
         (index (org-convect-name-index entries))
         (serves (plist-get here :serves)))
    (unless serves
      (user-error "\"%s\" points at nothing" (plist-get here :name)))
    (let* ((table (mapcar (lambda (name)
                            (cons (if (gethash name index)
                                      name
                                    (format "%s  (not in the file)" name))
                                  name))
                          serves))
           ;; the broken one first, because a broken one is why you are here
           (broken (seq-find (lambda (cell) (not (gethash (cdr cell) index))) table))
           (old (cdr (assoc (completing-read "Repoint which link: " table nil t
                                             nil nil (car (or broken (car table))))
                            table)))
           (above (mapcar (lambda (e) (cons (org-convect--candidate e)
                                            (plist-get e :name)))
                          (seq-filter
                           (lambda (e)
                             (memq (plist-get e :horizon)
                                   (org-convect-above (plist-get here :horizon))))
                           entries)))
           (new (string-trim
                 (org-convect--chosen
                  (completing-read
                   (format "\"%s\" now points at (empty drops it): " old)
                   above nil nil)
                  above))))
      (org-with-point-at (plist-get here :marker)
        (let ((kept (seq-uniq
                     (seq-remove #'string-empty-p
                                 (mapcar (lambda (name)
                                           (if (equal name old) new name))
                                         serves)))))
          (if kept
              (org-entry-put nil "CONVECT_SERVES"
                             (string-join
                              kept (concat org-convect-serves-separator " ")))
            (org-entry-delete nil "CONVECT_SERVES"))
          (save-buffer)))
      (message (if (string-empty-p new)
                   (format "Dropped \"%s\"" old)
                 (format "\"%s\" now points at \"%s\"" old new))))))

(defun org-convect--add-serves (marker name)
  "Add NAME to the `CONVECT_SERVES\\=' of the rung at MARKER, without repeating it."
  (org-with-point-at marker
    (let ((have (org-convect--split (org-entry-get nil "CONVECT_SERVES"))))
      (unless (member name have)
        (org-entry-put nil "CONVECT_SERVES"
                       (string-join (append have (list name))
                                    (concat org-convect-serves-separator " "))))
      (save-buffer))))

;;;; Writing one

(defun org-convect-above (horizon)
  "The horizons that sit above HORIZON, highest last."
  (let ((order (mapcar #'car org-convect-horizons)))
    (cdr (memq horizon order))))

(defun org-convect--read-horizon (entries)
  "Read a horizon, annotated with what each one is.

Defaults to the lowest empty rung, so the answer offered is the one GTD would
ask for next rather than the one highest up."
  (let* ((default (or (org-convect-next-rung entries) 'area))
         (completion-extra-properties
          (list :annotation-function
                (lambda (candidate)
                  (concat "  " (org-convect--one-line (intern candidate)))))))
    (intern (completing-read
             (format "Horizon (%s): " default)
             (mapcar (lambda (h) (symbol-name (car h))) org-convect-horizons)
             nil t nil nil (symbol-name default)))))

(defun org-convect--read-serves (entries horizon)
  "Read the higher rungs a new HORIZON entry is in service of.

Asks nothing at all when no higher rung has been written yet.  Pointing is
optional even when there is something to point at -- most work is justified on
its own -- but being asked to point at an empty level would make the bottom-up
order feel like a mistake, which it is not."
  (let ((above (seq-filter (lambda (e)
                             (memq (plist-get e :horizon)
                                   (org-convect-above horizon)))
                           entries)))
    (when above
      (seq-remove #'string-empty-p
                  (completing-read-multiple
                   "In service of (optional, comma-separated): "
                   (mapcar (lambda (e) (plist-get e :name)) above))))))

(defun org-convect--goto-insertion-point (horizon)
  "Move point where a new HORIZON entry belongs; return the level to write.

Prefers the end of the section marked `CONVECT_SECTION', so entries gather
where their guidance is.  Falls back to the end of the buffer: the ladder is
declared rather than located, so an entry outside every section still reads."
  (goto-char (point-min))
  (let ((section (org-find-property "CONVECT_SECTION" (symbol-name horizon))))
    (cond (section
           (goto-char section)
           (let ((level (1+ (org-current-level))))
             (org-end-of-subtree t t)
             (unless (bolp) (insert "\n"))
             level))
          (t
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           1))))

(defvar org-convect-add-property-functions nil
  "Functions asked for extra properties when `org-convect-add' is called
interactively.  Each is called with the horizon being written and returns an
alist of (PROPERTY . VALUE), or nil to contribute nothing.

The seam a layer above writes through.  The ladder itself knows only the two
properties that make a rung a rung; anything else -- `org-convect-act' asking
which life domain an area belongs to, say -- is registered here and leaves
again with the feature that registered it.

Only the interactive path consults these, so calling `org-convect-add' from
Lisp still asks nothing and writes exactly what it was given.")

;;;###autoload
(defun org-convect-add (horizon name &optional serves properties)
  "Write a rung called NAME at HORIZON, in service of SERVES.

PROPERTIES is an alist of (PROPERTY . VALUE) written alongside the two the
ladder needs; interactively it is gathered from
`org-convect-add-property-functions'.

The command that makes the ladder writable without reading this file first:
the horizon is chosen from a list that says what each one is, the prompt for
the name says where to look for yours, and pointing upward is only offered
when there is something above to point at.

Leaves point on the new entry, which is where the rest of it gets typed --
what the standard is, what it is really for.  A rung is a heading precisely
because it goes on accumulating that."
  (interactive
   (let* ((entries (org-convect-scan))
          (horizon (org-convect--read-horizon entries))
          (name (read-string (format "%s -- %s: "
                                     (org-convect-guide horizon :prompt)
                                     (org-convect-guide horizon :hint)))))
     (when (string-empty-p (string-trim name))
       (user-error "Nothing to add"))
     (list horizon (string-trim name)
           (org-convect--read-serves entries horizon)
           (append
            (when (org-convect-guide horizon :dated)
              (let ((date (org-read-date nil nil nil "Reached by")))
                (and (org-string-nw-p date)
                     (list (cons "CONVECT_BY" (format "[%s]" date))))))
            (apply #'append
                   (mapcar (lambda (f) (funcall f horizon))
                           org-convect-add-property-functions))))))
  (let ((file (car org-convect-files))
        marker)
    (unless file (user-error "`org-convect-files' is empty"))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (let ((level (org-convect--goto-insertion-point horizon)))
         (insert (make-string level ?*) " " name "\n")
         (forward-line -1)
         (org-entry-put nil "CONVECT_HORIZON" (symbol-name horizon))
         (org-entry-put nil "CREATED"
                        (format-time-string
                         (org-time-stamp-format t t) (current-time)))
         (when serves
           (org-entry-put nil "CONVECT_SERVES"
                          (string-join serves
                                       (concat org-convect-serves-separator " "))))
         (pcase-dolist (`(,property . ,value) properties)
           (when (org-string-nw-p value)
             (org-entry-put nil property value)))
         (setq marker (point-marker))))
      (save-buffer))
    (when (called-interactively-p 'any)
      (pop-to-buffer (marker-buffer marker))
      (goto-char marker)
      (org-fold-show-entry))
    marker))

(defun org-convect--section-horizon ()
  "The horizon the heading at point sits under, or nil.

Read by inheritance from the enclosing section's `CONVECT_SECTION', which is
why headings written straight into a section need not say what they are: the
place already said it.  Only for guessing -- what makes a rung a rung is still
its own `CONVECT_HORIZON'."
  (let ((section (org-entry-get nil "CONVECT_SECTION" t)))
    (and section (intern (string-trim section)))))

(defun org-convect--headings-in-scope ()
  "Markers for the headings `org-convect-declare' should act on.
Every heading in the region when there is one, otherwise the one at point."
  (if (use-region-p)
      (let ((end (region-end)) markers)
        (save-excursion
          (goto-char (region-beginning))
          (unless (org-at-heading-p) (outline-next-heading))
          (while (and (< (point) end) (org-at-heading-p))
            (push (point-marker) markers)
            (unless (outline-next-heading) (goto-char (point-max)))))
        (nreverse markers))
    (save-excursion (org-back-to-heading t) (list (point-marker)))))

;;;###autoload
(defun org-convect-declare (&optional horizon)
  "Make the heading at point, or every heading in the region, a rung.

The bulk half of writing a ladder, and the half that matches the advice.
Areas are not supposed to be invented one at a time -- they are copied off a
job description, an organisation chart, the household's division of labour --
and copying is a paste, not thirty answers to a prompt.  So: write or paste
the headings under a section, mark them, and run this.

HORIZON is taken from the section each heading sits under, so nothing is asked
at all in the ordinary case, and a region spanning two sections comes out
right rather than uniformly wrong.  It is prompted for once, and only for the
headings that sit outside every section.

Headings that are already rungs are left alone, so running it twice over the
same region changes nothing.  Section headings are skipped: a section is
scaffolding, and marking one would make the ladder claim a rung called
\"Areas of Focus and Accountability\".

Unlike `org-convect-add' this asks nothing else -- no upward links, no
properties from `org-convect-add-property-functions'.  It is a structural
marking, and the rest is worth answering per rung rather than in bulk."
  (interactive)
  (let ((markers (org-convect--headings-in-scope))
        (asked horizon)
        (declared 0)
        (skipped 0)
        (marked nil))
    (unless markers (user-error "No heading here"))
    (save-excursion
      (dolist (marker markers)
        (goto-char marker)
        (cond
         ;; a section is scaffolding and was never a candidate, so it is passed
         ;; over without being counted -- saying "one already was" about the
         ;; heading you are standing under would read as a mistake you made.
         ((org-entry-get nil "CONVECT_SECTION"))
         ((org-entry-get nil "CONVECT_HORIZON") (cl-incf skipped))
         (t
          (let ((this (or (org-convect--section-horizon)
                          asked
                          (setq asked
                                (intern (completing-read
                                         "Horizon for headings outside any section: "
                                         (mapcar (lambda (h) (symbol-name (car h)))
                                                 org-convect-horizons)
                                         nil t))))))
            (org-entry-put nil "CONVECT_HORIZON" (symbol-name this))
            (org-entry-put nil "CREATED"
                           (format-time-string
                            (org-time-stamp-format t t) (current-time)))
            (cl-pushnew this marked)
            (cl-incf declared))))))
    ;; Say what to do next, not only what was done.  A heading with nothing
    ;; under it is not yet a rung anyone can review, and the moment the marking
    ;; lands is the moment that is worth saying -- the drawer says it too, but
    ;; the drawer is folded and you have just stopped reading it.
    (message "Declared %d rung%s%s%s" declared (if (= declared 1) "" "s")
             (if (zerop skipped) "" (format " (%d already were)" skipped))
             (if (and (= 1 (length marked)) (> declared 0))
                 (concat ".  Now: " (org-convect--one-line (car marked) :write))
               ""))
    declared))

;;;; What has happened to a rung

(defvar org-convect-history-functions nil
  "Functions contributing entries to a rung's history.

Each is called with an entry plist and returns a list of (TIME KIND TEXT),
KIND a symbol naming what sort of thing happened.  `org-convect-act' adds the
choice points, which are the half of a rung's history the ladder never sees:
what was written about it is in its notes, what actually happened is not.")

(defun org-convect--notes ()
  "Notes on the entry at point, as (TIME KIND TEXT), newest first.

Three kinds, told apart two different ways.  A review opens with
`org-convect-review-lead', which is checked on the line the stamp is on --
`org-convect--last-reviewed' reads that same line, and a mark it could not see
would be a mark that failed at the one job it has.  A rewording says so in the
note's first line instead, since it is an ordinary note in every other way.
Anything else is `noted'.

Both are text matching and both can be fooled by someone writing the same
words by hand -- but the words are machine-written, the failure is a
mislabelled row, and the alternative is giving notes a property, which would
make them addressable when the whole point of a note is that it is not."
  (save-excursion
    (org-back-to-heading t)
    (let ((bound (save-excursion (outline-next-heading) (point)))
          notes)
      (org-end-of-meta-data)
      (when (looking-at org-property-drawer-re)
        (goto-char (match-end 0)))
      (while (re-search-forward (org-convect--note-re) bound t)
        (let* (;; First, before anything else looks at a string.  The lead is
               ;; the one thing here that can only be read from the match, and
               ;; both of the bindings below destroy it -- finding where the
               ;; note ends searches, and `org-time-string-to-time' parses.
               (declared (org-convect--note-lead-p org-convect-review-lead))
               (time (org-time-string-to-time (match-string 1)))
               (region (org-convect--note-body bound))
               (text (string-trim (buffer-substring-no-properties
                                   (car region) (min (cdr region) bound)))))
          (push (list time
                      (cond (declared 'reviewed)
                            ((string-match-p "\\`Reworded from" text) 'reworded)
                            (t 'noted))
                      text)
                notes)))
      ;; By the stamp, because that is what the record claims -- and where
      ;; stamps tie, by the order the file has them, which is newest first.
      ;; Six thoughts filed out of an inbox in one minute all carry the same
      ;; stamp, and a sort with nothing to fall back on would hand them back
      ;; in whichever order the walk happened to build.  `sort' is stable, so
      ;; the fallback is whatever order it is given: document order.
      (sort (nreverse notes) (lambda (a b) (time-less-p (car b) (car a)))))))

(defun org-convect-history (entry)
  "Everything that has happened to ENTRY, newest first, as (TIME KIND TEXT).

Three things you might want to look back over -- what was concluded about a
rung, when its wording changed, and how the moments went -- turn out to be one
thing recorded three ways in the same place.  So they are read as one stream
and the kind is a column, rather than as three views that would have to be
read side by side to see an order."
  (sort (append (org-with-point-at (plist-get entry :marker) (org-convect--notes))
                (apply #'append
                       (mapcar (lambda (f) (funcall f entry))
                               org-convect-history-functions)))
        (lambda (a b) (time-less-p (car b) (car a)))))

;;;; Changing a rung

(defun org-convect--note (text &optional lead)
  "Write TEXT as a dated note on the entry at point, opening with LEAD.

LEAD defaults to `org-convect-note-lead', which is Org's own wording -- so a
note written here and a note written by \\[org-add-note] are the same thing,
and nothing can tell them apart because there is nothing to tell.

Written straight rather than through `org-add-log-setup': that machinery sends
the note wherever `org-log-into-drawer' happens to point, and its value belongs
to whatever was logged last.  A note this package writes should land somewhere
it can predict.  The shape is Org's own, so it reads as any other note and
`org-convect--last-reviewed' finds its timestamp either way.

Below the standard, and above the notes already there.

Below, because a rung's body is what the rung *is* -- the standard the monthly
look is against -- and a file where you read a month of passing thoughts before
reaching it has buried the thing it is about.  Org has no setting for this: it
puts a note after the drawers, which on a rung is above the standard.  So this
is the one place the package writes somewhere Org would not, and it is worth
knowing that \\[org-add-note] pressed on a rung still writes at the top.
Both are found either way -- `org-convect--notes' reads the whole entry and
`org-convect--body' leaves every note out of it, wherever it sits.

Above the others, because the newest is the one worth reading first, which is
`org-log-states-order-reversed' set the way this configuration sets it."
  (org-back-to-heading t)
  (let ((bound (save-excursion (outline-next-heading) (point))))
    (org-convect--after-meta bound)
    ;; Where the trailing run of notes begins, or the end of the entry when
    ;; there is none.  A blank line does not break the run: a note followed by
    ;; the blank before the next heading is still the run.
    (let (run)
      (while (< (point) bound)
        (cond ((looking-at (org-convect--note-re))
               (unless run (setq run (point)))
               (org-convect--note-body bound))
              ((looking-at "^[ \t]*$") (forward-line 1))
              (t (setq run nil) (forward-line 1))))
      (goto-char (or run bound))))
  (unless (bolp) (insert "\n"))
  (insert (format "- %s %s \\\\\n  %s\n"
                  (or lead org-convect-note-lead)
                  (format-time-string (org-time-stamp-format t t))
                  text)))

;;;###autoload
(defun org-convect-reviewed (&optional conclusion)
  "Write CONCLUSION onto the rung at point and call it reviewed.

The only thing that moves the rung's clock.  Having looked at something is a
claim you make, not a residue of having typed near it -- so an ordinary note
costs nothing and this one says what it is.

Works on the rung at point in a file, on the row behind a board line, or on
one read by name."
  (interactive)
  (let* ((rung (or (org-convect--rung-at-point)
                   (org-convect-read-entry "Reviewed which rung: ")))
         (text (or conclusion
                   (read-string (format "Reviewed \"%s\" -- what did you conclude: "
                                        (plist-get rung :name))))))
    (when (string-empty-p (string-trim text))
      (user-error "A review with nothing to say is not a review"))
    (org-with-point-at (plist-get rung :marker)
      (org-convect--note (string-trim text) org-convect-review-lead)
      (save-buffer))
    (message "Reviewed %s" (plist-get rung :name))))

;;;###autoload
(defun org-convect-note (&optional text)
  "Write TEXT as an ordinary note on a rung read by name.

Not a new kind of record.  What lands is what \\[org-add-note] writes and
nothing can tell the two apart, because there is nothing to tell -- this is
the same note, reachable from somewhere other than the rung.

That is the whole of it, and it is the point: what gets noticed about a rung
is noticed while doing something else.  Filing it should not mean going to
find the ladder first, and it must not count as having reviewed anything.

The text offered is the region, or the heading you are standing on when that
heading is not itself a rung -- which is what an item caught in the inbox is."
  (interactive)
  (let* ((default (org-convect--note-default))
         (rung (org-convect-read-entry "Note about which rung: "))
         (text (or text (read-string "Note: " default))))
    (when (string-empty-p (string-trim text))
      (user-error "Nothing to note"))
    (org-with-point-at (plist-get rung :marker)
      (org-convect--note (string-trim text))
      (save-buffer))
    (message "Noted against %s" (plist-get rung :name))))

(defun org-convect--note-default ()
  "What to offer as the text of a note, or nil.

The region if there is one; otherwise the heading at point when it is not a
rung, since a rung's own heading is never what you meant to say about it."
  (cond ((use-region-p)
         (string-trim (buffer-substring-no-properties
                       (region-beginning) (region-end))))
        ((and (derived-mode-p 'org-mode)
              (ignore-errors (org-at-heading-p))
              (not (org-entry-get nil "CONVECT_HORIZON")))
         (org-get-heading t t t t))))

(defun org-convect--rename-references (old new)
  "Point every `CONVECT_SERVES\\=' naming OLD at NEW instead, in every file.
Returns how many rungs were changed."
  (let ((changed 0))
    (dolist (entry (org-convect-scan))
      (let ((serves (plist-get entry :serves)))
        (when (member old serves)
          (org-with-point-at (plist-get entry :marker)
            (org-entry-put nil "CONVECT_SERVES"
                           (string-join
                            (mapcar (lambda (n) (if (equal n old) new n)) serves)
                            (concat org-convect-serves-separator " ")))
            (save-buffer))
          (cl-incf changed))))
    changed))

;;;###autoload
(defun org-convect-reword (&optional new reason)
  "Rename the rung at point to NEW, saying REASON, and follow the references.

Rewording is not a text edit.  Rungs are pointed at by name, so changing one
breaks every `CONVECT_SERVES\\=' that named it -- silently, because a link that
resolves to nothing is only visible in a report nobody has run yet.  This does
the three things that make up the one act: rewrites the heading, repoints
everything that referred to it, and writes down what it used to say.

The last is the reason this is a command and not \\[org-edit-headline].  A
ladder that changes is not a ladder going wrong -- understanding deepens and
circumstances move, and a principle that never once got reworded is usually one
nobody has looked at.  What is worth keeping is *that* it changed and why, and
keeping it here means the next review reads it without going through a version
history to find it."
  (interactive)
  (let* ((rung (or (org-convect--rung-at-point) (user-error "Not on a rung")))
         (old (plist-get rung :name))
         (new (or new (read-string "Reword to: " old)))
         (reason (or reason (read-string (format "Why, from \"%s\": " old)))))
    (when (or (string-empty-p (string-trim new)) (equal new old))
      (user-error "Unchanged"))
    (let ((new (string-trim new)))
      (org-with-point-at (plist-get rung :marker)
        (org-back-to-heading t)
        (org-edit-headline new)
        (org-convect--note
         (if (org-string-nw-p reason)
             (format "Reworded from \"%s\".  %s" old reason)
           (format "Reworded from \"%s\"." old)))
        (save-buffer))
      (let ((followed (org-convect--rename-references old new)))
        (message "Reworded%s"
                 (if (zerop followed) ""
                   (format ", and repointed %d rung%s at it"
                           followed (if (= 1 followed) "" "s"))))
        followed))))

;;;; What the ladder is missing

(defconst org-convect-doctor-buffer "*Horizons*"
  "Where `org-convect-doctor' writes.")

(defvar org-convect-doctor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-agenda-mode-map)
    (define-key map (kbd "r") #'org-convect-doctor)
    map)
  "The one key the doctor adds to the agenda's own.

Redrawing, and nothing else.  Everything the report needs -- going to a row,
writing a note on it, turning follow on with `v f' -- is already an agenda key
acting on the marker the row carries, and rebinding those would be replacing
working commands with worse ones.  `r' earns its place because a report you
have to re-run by name after every fix is a report you stop consulting.

`g' is deliberately absent.  It is a motion prefix for a great many people --
`gg', `gj', `gs' -- and binding it to a command here would take the prefix and
every motion under it away in this buffer alone.

A sequence under it, `g r', would in fact be safe: `define-key' through an
inherited prefix builds a child map that itself inherits, so the parent's other
`g' keys go on working.  It is left out anyway, because `r' already redraws and
a second key for one command is a second thing to remember.")

(defconst org-convect--finding-labels
  '((bare-rung "Nothing written under it"
     "open it and write what goes underneath -- or leave it, if a rung above
still has to say what it is being kept up for")
    (long-heading "Long for a name -- if it is a sentence, it belongs in the body"
     "move the sentence down into the body and leave a name behind")
    (undated-goal "No date"
     "M-x org-convect-set-date.  Roughly when is half of what makes it a goal")
    (past-its-date "Past its date"
     "reached, abandoned, or never a goal -- say which, then close it or move
the date")
    (unserved-rung "Nothing below points at it"
     "C-u M-x org-convect-link, standing on the rung: it asks which rungs below
should point at it, and writes the link onto each")
    (unresolved-serves "Points above at something that is not there"
     "M-x org-convect-relink, standing on the rung: it offers the links this
one carries and asks what to point at instead, or drops it for an empty answer")
    (misaimed-serves "Points at something that is not above it"
     "M-x org-convect-relink offers only what sits above, so repointing it is a
matter of picking one.  Written by hand, most likely -- the commands cannot
offer a rung below")
    (duplicate-name "Shares its name with another rung"
     "two rungs answering to one name, so a link cannot say which it meant.
M-x org-convect-reword one of them")
    (unknown-horizon "Not a horizon this package knows"
     "`CONVECT_HORIZON\=' holds something that is not a rung of this ladder")
    (overdue-review "Due to be looked at"
     "z on the row writes your conclusion onto the rung, which is also what
postpones the next look at it"))
  "Per finding kind: the line of English, and what answers it.

The remedy is the half that was missing.  A report that names a condition and
not the command that resolves it leaves the reader to go and find the command,
which is how a finding comes to be read as scenery.

The two link findings are worded as a pair because they are read as one, but
they are not opposites.  `unserved-rung\=' is an absence: nothing below points
here.  `unresolved-serves\=' is a break: something does point, at a name that
is not in the file.  The true opposite of the first -- a rung that points at
nothing -- is deliberately not a finding at all, because most areas serve
nothing and the top rung serves nothing by definition.")

(defun org-convect--finding-label (kind)
  "The line of English for finding KIND."
  (nth 1 (assq kind org-convect--finding-labels)))

(defun org-convect--finding-remedy (kind)
  "What answers finding KIND, as one line."
  (nth 2 (assq kind org-convect--finding-labels)))

;;;###autoload
(defun org-convect-doctor ()
  "Show what the ladder has and what it is missing.  Writes nothing.

The guidance says what goes under each rung and the file cannot show that it
is missing -- a heading with an empty body looks exactly like a heading.  So
this counts them.

It is a report and not a scold.  Several of the things it lists are fine to
leave: a rung can stay bare on purpose while you go up and fetch the goal that
says what it is for, and most areas serve nothing at all.  What it is for is
that you should be choosing to leave them, rather than not noticing.

Which is why the rows are live and each group carries the thing that answers
it.  Deciding to leave a finding means looking at the rung, so \\[org-agenda-switch-to]
goes to it and the cursor opens it alongside; and deciding to fix one means
knowing the command, so the command is written under the complaint rather than
left to be looked up."
  (interactive)
  (let* ((entries (org-convect-scan))
         (findings (org-convect-findings entries))
         (buffer (get-buffer-create org-convect-doctor-buffer))
         ;; where to land after a redraw.  The row, not the line: fixing a
         ;; finding removes lines above the one you were on, so a line number
         ;; would put you somewhere else every time.
         (was (and (eq (current-buffer) buffer)
                   (org-get-at-bol 'org-convect-rung))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        ;; an agenda buffer, for the same reason the review board is one: the
        ;; rows carry markers, and Org's own keys then act on the rung behind
        ;; the row without this package binding anything.
        (unless (derived-mode-p 'org-agenda-mode) (org-agenda-mode))
        (setq-local org-agenda-type 'agenda)
        (erase-buffer)
        (insert "Horizons\n\n")
        ;; the shape of the ladder, highest first, the way the file reads
        (dolist (horizon (reverse (mapcar #'car org-convect-horizons)))
          (let* ((at (org-convect-entries entries horizon))
                 (bare (seq-count (lambda (e) (plist-get e :bare)) at)))
            (insert (format "  %-34s %2d %s%s\n"
                            (org-convect-horizon-name horizon)
                            (length at)
                            (if (= 1 (length at)) "rung " "rungs")
                            (cond ((null at) "   -- empty")
                                  ((zerop bare) "")
                                  (t (format "   %d with nothing written" bare)))))))
        (insert "\n")
        (let ((climb (org-convect-next-rung entries))
              (behind (org-convect-unfinished entries)))
          (insert (format "  Next to start   %s\n"
                          (if climb (org-convect-horizon-name climb)
                            "nothing -- every level is occupied")))
          (insert (format "  Unfinished      %s\n"
                          (if behind
                              (mapconcat #'symbol-name behind ", ")
                            "nothing"))))
        ;; then the findings, gathered by what they are rather than by rung,
        ;; because the question being asked is "what is missing", not "what is
        ;; wrong with this one"
        (dolist (kind (mapcar #'car org-convect--finding-labels))
          (let ((these (seq-filter (lambda (f) (eq (plist-get f :kind) kind))
                                   findings)))
            (when these
              (insert (format "\n%s\n" (org-convect--finding-label kind)))
              (dolist (f these)
                ;; a row, not a picture of one: the marker is what makes RET
                ;; go to it and the follow window show it
                (insert (org-convect--review-line
                         (format "  %-8s %s%s\n"
                                 (plist-get f :horizon)
                                 (plist-get f :name)
                                 (if (plist-get f :detail)
                                     (format "  (%s)" (plist-get f :detail))
                                   ""))
                         (plist-get f :marker)
                         (plist-get f :name))))
              ;; the answer, after the list rather than before it, so that the
              ;; rows stay a column and the remedy stays prose
              (dolist (line (split-string (org-convect--fill (org-convect--finding-remedy kind))
                                  "\n" t))
                (insert (format "      %s\n" line))))))
        (unless findings (insert "\nNothing missing.\n"))
        (insert "\n  RET  go to it   r  redraw   v f  follow   q  quit\n")
        (use-local-map org-convect-doctor-mode-map)
        (goto-char (point-min))
        (when was
          (let ((found (text-property-search-forward
                        'org-convect-rung was #'equal)))
            (goto-char (if found (prop-match-beginning found) (point-min)))))))
    (pop-to-buffer buffer)))

;;;; Planning a project

(defcustom org-convect-plan-fields
  '(("Purpose" . "why this is worth doing at all, and what should guide how")
    ("Outcome" . "what is true once it has worked, written in the present tense"))
  "The parts of a project's plan that are worth keeping, and what each asks.

GTD's planning model has five steps and only these two leave anything behind.
Brainstorming and organising are *acts*, not fields: the brainstorm is raw
material for the organising, and what organising produces is the shape of the
outline itself.  Next actions are headings with a keyword on them.

So a form with five blanks would be asking for three things that have nowhere
to sit, and would leave two of them empty forever."
  :type '(alist :key-type string :value-type string)
  :group 'org-convect)

;;;###autoload
(defun org-convect-plan ()
  "Open a planning space under the project at point.

Puts down the two fields worth keeping and a first empty child to brainstorm
into, then gets out of the way.  What follows is Org's own: \\[org-meta-return]
for the next thought, \\[org-metaleft] and \\[org-metaright] to organise them
into a shape, \\[org-todo] on the ones you are committing to.

The child carries no keyword, and that is the point rather than an omission.
org-foresight gives a heading with no TODO keyword no record at all -- it
reads the absence as scaffolding, a place to put things -- so a brainstorm can
be as long and as wrong as it needs to be without a single thought of it being
counted as work anybody has taken on.  Only what you mark becomes work.

Works on the heading at point, or on the entry behind an agenda line -- which
is where you usually are when you notice something needs breaking down."
  (interactive)
  (let ((marker (if (derived-mode-p 'org-agenda-mode)
                    (or (org-get-at-bol 'org-hd-marker)
                        (user-error "No entry on this line"))
                  (point-marker)))
        target)
    (org-with-point-at marker
      (org-back-to-heading t)
      ;; Held, because inserting the fields leaves point at the beginning of
      ;; the line after them -- which is the *next* heading when the entry had
      ;; no body.  Going "back to heading" from there arrives at the wrong
      ;; entry, and the space to think then opens under somebody else's work.
      (let* ((here (point-marker))
             (level (org-current-level))
             (body (org-convect--body))
             (missing (seq-remove
                       (lambda (field)
                         (string-match-p (concat "^- " (regexp-quote (car field))
                                                 " ::")
                                         body))
                       org-convect-plan-fields)))
        ;; the fields first, in order, above whatever is already there
        (when missing
          (org-convect--after-meta
           (save-excursion (outline-next-heading) (point)))
          (unless (bolp) (insert "\n"))
          (dolist (field missing)
            (insert (format "- %s :: \n" (car field)))))
        ;; then somewhere to think
        (goto-char here)
        (let ((end (save-excursion (org-end-of-subtree t t))))
          (if (save-excursion (outline-next-heading)
                              (and (< (point) end) (> (org-current-level) level)))
              (goto-char end)
            (goto-char end)
            (unless (bolp) (insert "\n"))
            ;; A line of its own.  `org-end-of-subtree' with TO-HEADING lands
            ;; on the *start* of whatever follows, so a heading inserted here
            ;; without a newline runs into that one and eats it.
            (save-excursion (insert (make-string (1+ level) ?*) " \n"))
            (end-of-line)))
        (setq target (point-marker))))
    (when (derived-mode-p 'org-agenda-mode)
      (pop-to-buffer (marker-buffer target)))
    (goto-char target)
    (message "%s.  Then %s for the next thought, %s to shape them, %s on what you commit to"
             (mapconcat (lambda (f) (format "%s: %s" (car f) (cdr f)))
                        org-convect-plan-fields " / ")
             (substitute-command-keys "\\[org-meta-return]")
             (substitute-command-keys "\\[org-metaright]")
             (substitute-command-keys "\\[org-todo]"))))

;;;; What the thing under the cursor is asking for

(defcustom org-convect-eldoc t
  "Whether to say in the echo area what the line at point is asking for.

Guidance has three places it can live and each has a failure.  In the file it
goes stale, because it is written once and never learns anything.  In a
command it is only there when you thought to run it.  In the echo area it is
there while you are actually filling the thing in, which is the moment the
question matters and the moment you are least likely to go looking.

Eldoc is idle-triggered, so this does not compete with typing.  It returns
nothing anywhere it does not recognise, so other Org buffers are untouched."
  :type 'boolean
  :group 'org-convect)

(defun org-convect-eldoc-function (&rest _)
  "Say what the line at point is asking for, or nil.

For `eldoc-documentation-functions\\=', whose contract is either/or: a function
may call the callback it is handed, *or* ignore it and return the string.
Doing both hands Eldoc the same answer twice and it prints two identical
lines.  Deciding here costs a regexp and a property read, so this takes the
second road and simply returns."
  (when org-convect-eldoc
    (let ((said
           (save-excursion
             (beginning-of-line)
             (cond
              ;; a field of a project's plan
              ((looking-at "^[ \t]*- \\([A-Za-z]+\\) ::")
               (let ((asks (assoc-default (match-string 1)
                                          org-convect-plan-fields)))
                 (and asks (format "%s -- %s" (match-string 1) asks))))
              ;; a rung of the ladder: what goes underneath it
              ((org-at-heading-p)
               (let ((horizon (org-entry-get nil "CONVECT_HORIZON")))
                 (and horizon
                      (org-convect-horizon-p (intern horizon))
                      (format "%s -- %s"
                              (org-convect-horizon-name (intern horizon))
                              (org-convect--one-line (intern horizon) :write)))))))))
      said)))

(defun org-convect-eldoc-setup ()
  "Let Eldoc ask this package about the line at point, in this buffer."
  (add-hook 'eldoc-documentation-functions #'org-convect-eldoc-function nil t))

;;;###autoload
(add-hook 'org-mode-hook #'org-convect-eldoc-setup)

;;;; The review

(defconst org-convect-review-buffer "*Horizons Review*"
  "Where `org-convect-review' draws.")

(defvar org-convect-review-evidence-functions nil
  "Functions asked what to show under a rung on the review board.

Each is called with an entry plist and returns a list of strings, or nil.  The
board shows what the ladder can see by itself -- when it was last looked at,
what is written under it, what it serves; a layer above adds what it can see
and the ladder cannot.  `org-convect-act' adds the choice points.

Remove that feature and the lines go with it.  The board still stands.")

(defun org-convect--review-line (text &optional marker name)
  "TEXT as a board row, acting on MARKER when there is one.

The marker is what makes the row a row rather than a picture of one: with it,
Org's agenda keys work here, so `\\[org-agenda-add-note]' writes the
conclusion onto the actual rung and `\\[org-agenda-switch-to]' goes to it.
Without it the board looks identical and does nothing, which is the failure
worth guarding against."
  (if (not (markerp marker))
      text
    (propertize text
                'org-marker marker
                'org-hd-marker marker
                'org-convect-rung name
                'org-agenda-type 'agenda
                'help-echo "z add note · RET go to it")))

(defcustom org-convect-review-columns 80
  "How wide the review board sets its prose.

A rung's body is filled to this less whatever the row is indented by, so the
standard reads as a paragraph rather than as however it happened to be typed."
  :type 'integer
  :group 'org-convect)

(defconst org-convect-review-mark "↳"
  "What marks a line as belonging to the row above it.

org-foresight's, and deliberately the same: it means one thing there -- this
elaborates the line above -- and it should not come to mean a second thing on
a board the same person reads in the same session.")

(defun org-convect--evidence-body (entry indent)
  "ENTRY's own prose, filled to what is left of the line after INDENT columns.

Whole rather than glimpsed.  The board used to show the first line cut at 68
columns, which is enough to recognise a standard and not enough to review one
-- and reviewing is what the board is for.  What paid for it was the four
lines of tally underneath, which are one line now.

Filled rather than reproduced: the source breaks where it was convenient to
type, and a review reads better as a paragraph than as a transcript of a
window that no longer exists."
  (let ((body (org-with-point-at (plist-get entry :marker) (org-convect--body)))
        (width (max 20 (- org-convect-review-columns indent))))
    (unless (string-empty-p body)
      ;; Filled as paragraphs, because that is what these bodies are: the
      ;; guidance's own worked examples write a standard as a sentence with
      ;; semicolons in it, and a rung hard-wrapped at one width and reproduced
      ;; at another comes out ragged for no reason.
      ;;
      ;; A list item keeps its own line, though.  Somebody who wrote one
      ;; criterion per line and marked them as a list meant them to be read as
      ;; separate things, and running them together would be answering a
      ;; different question from the one they wrote down.
      (apply #'append
             (mapcar (lambda (block)
                       (split-string (org-convect--fill block width) "\n" t))
                     (org-convect--blocks body))))))

(defun org-convect--blocks (text)
  "TEXT split where it must not be filled across: blank lines and list items."
  (let (blocks current)
    (dolist (line (split-string text "\n"))
      (cond ((string-blank-p line)
             (when current (push (string-join (nreverse current) "\n") blocks))
             (setq current nil))
            ((string-match-p "\\`[ \t]*\\([-+*]\\|[0-9]+[.)]\\)[ \t]" line)
             (when current (push (string-join (nreverse current) "\n") blocks))
             (setq current (list line)))
            (t (push line current))))
    (when current (push (string-join (nreverse current) "\n") blocks))
    (nreverse blocks)))

(defun org-convect--evidence-tally (entry entries scan &optional threaded)
  "One line of what is known about ENTRY besides its own prose, or nil.

Four lines once, and each of them short: the clock, what it serves, how many
answer to it, and whatever a layer above adds.  Joined into one, most specific
first, because when the line will not fit it is the end that should go.

THREADED says the board is drawing a descent, where what a rung serves and
what answers to it are already on the page as position.  Saying them again in
words is the same fact twice, so they are left out and only the links the tree
could not draw are named."
  (let* ((near (org-convect-neighbourhood entries entry))
         (hours (cdr (assoc (plist-get entry :name) (plist-get scan :rows))))
         (noted (or (plist-get entry :noted) 0))
         (terms (append
                 (and hours (list (format "%s clocked"
                                          (org-duration-from-minutes hours))))
                 ;; Written against it and not yet read.  Without this the
                 ;; notes are write-only: nothing on the page would ever say
                 ;; there was something waiting to be looked at.
                 (and (> noted 0) (list (format "%d noted" noted)))
                 (unless threaded
                   (append
                    (and (car near)
                         (list (concat "serves "
                                       (mapconcat (lambda (e) (plist-get e :name))
                                                  (car near) ", "))))
                    (and (cdr near)
                         (list (format "%d below" (length (cdr near)))))))
                 (apply #'append
                        (mapcar (lambda (f) (funcall f entry))
                                org-convect-review-evidence-functions)))))
    (and terms (concat org-convect-review-mark " " (string-join terms " · ")))))

(defun org-convect--review-status (entry now called)
  "How ENTRY stands at NOW, as a short phrase for its row.

CALLED is the alist `org-convect-called' returned, so a rung with no calendar
can still say something -- which is the only way the top of the ladder ever
speaks up."
  (let ((why (cdr (assq entry called)))
        (due-on (org-convect-due-on entry)))
    (cond (why (concat "called -- " why))
          ((org-convect-overdue-p entry now) "due")
          (due-on (concat "due " (format-time-string "%Y-%m-%d" due-on)))
          ((org-convect-cadence-days (plist-get entry :horizon)) "due -- never looked at")
          (t "no calendar"))))

(defvar-local org-convect--lineages nil
  "Hash of rung name -> the names linked to it, for the board being shown.

Built once when the board is drawn.  Following the cursor means answering
\"what is linked to this\" on every keystroke, and answering it by walking the
files each time would make moving the cursor the most expensive thing in the
buffer.")

(defvar-local org-convect--lit nil
  "Overlays currently lighting up a thread.")

(defvar-local org-convect--lit-for nil
  "The rung the overlays belong to, so an unchanged row costs nothing.")

(defun org-convect--light-lineage ()
  "Light up the rungs linked to the one under the cursor.

On `post-command-hook\\=' in a board, which means it runs on every keystroke --
so it does as little as possible: reads a text property, compares it with what
is already lit, and returns.  The work only happens when the cursor has
actually moved onto a different rung."
  (when (and org-convect-highlight-lineage org-convect--lineages)
    (let ((name (get-text-property (point) 'org-convect-rung)))
      (unless (equal name org-convect--lit-for)
        (setq org-convect--lit-for name)
        (mapc #'delete-overlay org-convect--lit)
        (setq org-convect--lit nil)
        (when name
          (let ((thread (gethash name org-convect--lineages)))
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (let ((here (get-text-property (point) 'org-convect-rung)))
                  (when (and here (member here thread))
                    (let ((overlay (make-overlay (line-beginning-position)
                                                 (1+ (line-end-position)))))
                      (overlay-put overlay 'face 'org-convect-lineage-face)
                      (overlay-put overlay 'priority -50)
                      (push overlay org-convect--lit))))
                (forward-line 1)))))))))

(defun org-convect--remember-lineages (entries)
  "Work out every thread once, for the board about to be drawn."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries table)
      (puthash (plist-get entry :name)
               (mapcar (lambda (e) (plist-get e :name))
                       (org-convect-lineage entries entry))
               table))))

(defcustom org-convect-review-threaded nil
  "Whether the board opens as a descent rather than as four sections.

Sections group by altitude, which is what the file looks like.  A descent
follows the links instead: a purpose, then indented beneath it whatever is in
service of it.  Neither is the true one -- the ladder is read both ways, and
`\\<org-convect-review-mode-map>\\[org-convect-review-thread]' moves between
them without redrawing from the files."
  :type 'boolean
  :group 'org-convect)

(defvar-local org-convect--review-threaded nil
  "Whether the board on screen is drawn as a descent.")

(defvar-local org-convect--review-args nil
  "The arguments the board on screen was drawn with, so it can redraw itself.")

(defvar org-convect-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-agenda-mode-map)
    (define-key map (kbd "t") #'org-convect-review-thread)
    (define-key map (kbd "r") #'org-convect-review-redraw)
    (define-key map (kbd "z") #'org-convect-reviewed)
    map)
  "Keys the board adds to the agenda's own.

`t' is `org-agenda-todo' underneath, and taking it is a repair rather than a
displacement: a rung deliberately carries no TODO keyword, so pressing it on
one of these rows would *give* it one and make the ladder look like a task
list.  `r' is redrawing, which the board needs more than most views do -- the
key beside it writes a note that changes what the board says about the rung
you are standing on.

`z' is `org-agenda-add-note', which writes an ordinary note, and an ordinary
note is no longer a review.  Reviewing is done on this board, so this is where
the key that says so belongs -- and a conclusion written here would otherwise
leave the rung looking exactly as unlooked-at as before.

`g' is left alone.  It opens the motions for a great many people, and a
command on it would take the prefix and everything under it away here alone.")

(defun org-convect--review-altitude (entry)
  "ENTRY's altitude as a fixed-width gutter.

A column rather than a heading, because a descent cannot group by altitude:
links may skip one, so two rows at the same indentation can be a goal and an
area.  Depth says how far the justification runs; this says how high the rung
is; neither can be read off the other.

Printed on every row, never blanked when it repeats -- a blank would read as
\"the same as above\", which is the one inference the column exists to stop."
  (let ((horizon (symbol-name (plist-get entry :horizon))))
    (format "  %-7s " (truncate-string-to-width horizon 7 nil ?\s t))))

(defun org-convect--review-under (gutter prefix &optional threaded kids)
  "Where a rung's prose is laid, given its GUTTER and its PREFIX.

Blank where the altitude was: it is a fact about the rung, and a body line
repeating it would read as four more rungs at that altitude rather than as one
rung's prose.

In a descent the prose goes one step further in than the row -- level with
where the rung's own children will be named, rather than two spaces in.  Two
spaces put it *past* the column its children's connectors start at, so a
rung's prose crossed the boundary of the block below it and nothing on the
page said where one rung ended and the next began.

KIDS says the rung has children, and then the step is a bar rather than a
blank: the same bar the children hang from, run up through the prose so that
the row, what it says, and what answers to it are one bracketed block.  A rung
with no children takes the blank -- a bar there would promise a subtree that
never arrives."
  (let ((blank (make-string (string-width gutter) ?\s)))
    (if (not threaded)
        blank
      (pcase-let ((`(,bar ,_tee ,_ell ,gap) (org-convect-outline-glyphs)))
        (concat blank (org-convect-outline-under prefix) (if kids bar gap))))))

(defun org-convect--review-insert (entry gutter prefix column now called
                                        entries scan &optional threaded also
                                        kids)
  "Insert one rung's block: its row, its prose, and one line of tally.

GUTTER is what stands to the left of the tree -- the altitude column, or the
plain indent when there is no tree.  PREFIX is the row's connectors, KIDS
whether anything is drawn under it.  Everything below the row is laid against
`org-convect--review-under', so the tree cannot break where the prose begins.

The prose carries the same `org-convect-rung' the row does.  Following the
cursor reads that property off the character under point, and a body of five
lines with no property on it is five lines on which the thread goes dark."
  (let* ((name (plist-get entry :name))
         (lead (concat gutter prefix))
         (under (org-convect--review-under gutter prefix threaded kids))
         ;; the step is already in UNDER when there is a tree; without one the
         ;; prose still has to sit in from the name
         (pad (if threaded "" "  "))
         (indent (+ (string-width under) (string-width pad))))
    (insert (org-convect--review-line
             (format "%s%s%s  %s\n" lead name
                     (make-string (max 0 (- column (string-width lead)
                                            (string-width name)))
                                  ?\s)
                     (org-convect--review-status entry now called))
             (plist-get entry :marker) name))
    (dolist (line (org-convect--evidence-body entry indent))
      (insert (org-convect--review-line (format "%s%s%s\n" under pad line)
                                        (plist-get entry :marker) name)))
    (when also
      (insert (org-convect--review-line
               (format "%s%s%s also serves %s\n" under pad
                       org-convect-review-mark (string-join also ", "))
               (plist-get entry :marker) name)))
    (when-let ((tally (org-convect--evidence-tally entry entries scan threaded)))
      (insert (org-convect--review-line (format "%s%s%s\n" under pad tally)
                                        (plist-get entry :marker) name)))))

;;;###autoload
(defun org-convect-review (&optional only-wanting now)
  "Show the ladder, with what wants looking at marked.

The whole thing, highest first, the way the file reads.  Reviewing is not
something a date gives permission for -- a cadence says how often to come back,
not that looking is forbidden until then -- and the relationships are worth
seeing on any day, which is otherwise only possible by reading the file and
reconstructing them in your head.

So what is due is marked rather than filtered for.  With ONLY-WANTING
\\(\\[universal-argument]) the rest is left out, which is the shape of a
monthly sitting rather than a look.

The two ways a rung asks for attention stay visible in the marking.  Areas and
goals have a cadence and come due on a date.  The rungs above have none -- GTD
reads them when direction or motivation has gone, which is a condition -- so
they say \"no calendar\" until something notices the condition, and \"called\"
when something does.

Each rung's own prose is shown whole.  The board is where reviewing happens,
and a standard cut to its first line is enough to recognise and not enough to
judge.

Rows are live: \\[org-agenda-add-note] writes your conclusion onto the rung,
\\[org-agenda-switch-to] goes to it when the conclusion is that the rung
itself should change, and \\<org-convect-review-mode-map>\\[org-convect-review-thread]
follows the links instead of the altitudes."
  (interactive "P")
  (let* ((now (or now (current-time)))
         (entries (org-convect-scan))
         (called (org-convect-called entries))
         (due (org-convect-due entries now))
         (scan (org-convect-clock-rows))
         (buffer (get-buffer-create org-convect-review-buffer))
         (threaded (if (eq (current-buffer) buffer)
                       org-convect--review-threaded
                     org-convect-review-threaded))
         (was (and (eq (current-buffer) buffer)
                   (org-get-at-bol 'org-convect-rung)))
         (wanted (lambda (e) (or (memq e due) (assq e called))))
         (rows (when threaded (org-convect--review-rows entries only-wanting
                                                        wanted)))
         ;; Where the status column starts: the widest row there is, measured
         ;; the way it will be drawn -- gutter, connectors and name together.
         ;; Measuring the name alone leaves exactly the widest row two columns
         ;; past every other one, which is the shape the bug took.  A fixed
         ;; column would cut a principle in half, and a principle read in half
         ;; is a different principle.  Measured over what will be drawn: a name
         ;; that was filtered out has no business widening the page.
         (column (if threaded
                     (apply #'max 24
                            (mapcar (lambda (r)
                                      (+ (string-width
                                          (org-convect--review-altitude (car r)))
                                         (string-width (nth 2 r))
                                         (string-width
                                          (plist-get (car r) :name))))
                                    rows))
                   (apply #'max 24
                          (mapcar (lambda (e)
                                    (+ 2 (string-width (plist-get e :name))))
                                  (if only-wanting
                                      (seq-filter wanted entries)
                                    entries))))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'org-agenda-mode) (org-agenda-mode))
        (setq-local org-agenda-type 'agenda)
        ;; The date sits where the statuses do, so the head of the page is
        ;; ruled by the same column as everything under it.
        (insert (format "Horizons Review%s%s\n"
                        (make-string (max 2 (- (+ column 2) 15)) ?\s)
                        (format-time-string "%Y-%m-%d" now)))
        (insert (format "  %d due, %d called%s\n\n"
                        (length due) (length called)
                        (if org-convect-signal-functions ""
                          " (nothing is watching the rungs with no calendar)")))
        (if threaded
            (progn
              ;; The questions have nowhere else to go here: a descent has no
              ;; section headings to put them under, and one per row would say
              ;; the same four things over and over.
              (dolist (horizon (reverse (mapcar #'car org-convect-horizons)))
                (when-let ((asks (org-convect-guide horizon :review)))
                  (let ((lines (split-string
                                (org-convect--fill
                                 asks (- org-convect-review-columns 10))
                                "\n" t)))
                    (insert (format "  %-7s %s\n" (symbol-name horizon)
                                    (car lines)))
                    (dolist (line (cdr lines))
                      (insert (format "%s%s\n" (make-string 10 ?\s) line))))))
              (insert "\n")
              (let ((rest rows))
                (while rest
                  (pcase-let ((`(,entry ,depth ,prefix ,also) (car rest)))
                    (org-convect--review-insert
                     entry (org-convect--review-altitude entry) prefix
                     column now called entries scan t also
                     ;; the next row is a child of this one exactly when it is
                     ;; deeper: the descent emits a rung and then its subtree
                     (and (cadr rest) (> (nth 1 (cadr rest)) depth))))
                  (setq rest (cdr rest))))
              (insert "\n"))
          (dolist (horizon (reverse (mapcar #'car org-convect-horizons)))
            (let ((at (seq-filter (lambda (e)
                                    (or (not only-wanting) (funcall wanted e)))
                                  (org-convect-entries entries horizon))))
              (when (or at (not only-wanting))
                (insert (org-convect-horizon-name horizon) "\n")
                (when-let ((asks (org-convect-guide horizon :review)))
                  (dolist (line (split-string
                                 (org-convect--fill
                                  asks (- org-convect-review-columns 2))
                                 "\n" t))
                    (insert (format "  %s\n" line))))
                (if (null at)
                    (insert "  nothing\n")
                  (dolist (entry at)
                    (org-convect--review-insert entry "  " "" column now called
                                                entries scan)))
                (insert "\n")))))
        (insert (org-convect--review-legend threaded only-wanting))
        (use-local-map org-convect-review-mode-map)
        (setq org-convect--review-threaded threaded
              org-convect--review-args (list only-wanting now)
              org-convect--lineages (org-convect--remember-lineages entries)
              org-convect--lit nil
              org-convect--lit-for :none)
        (add-hook 'post-command-hook #'org-convect--light-lineage nil t)
        (goto-char (point-min))
        (when was
          (let ((found (text-property-search-forward 'org-convect-rung was #'equal)))
            (goto-char (if found (prop-match-beginning found) (point-min)))))))
    (pop-to-buffer buffer)))

(defun org-convect-review-thread ()
  "Draw the board the other way: by the links, or by the altitudes.

Not a filter and not a second buffer -- the same rungs, ordered by the other
thing that is true about them.  The cursor stays on the rung it was on, which
is the whole use of the toggle: you are asking what this one is for."
  (interactive)
  (unless (equal (buffer-name) org-convect-review-buffer)
    (user-error "Not the review board"))
  (setq org-convect--review-threaded (not org-convect--review-threaded))
  (apply #'org-convect-review org-convect--review-args))

(defun org-convect-review-redraw ()
  "Draw the board again from the files, keeping the rung under the cursor.

Wanted more here than on most views: `\\[org-agenda-add-note]' writes the
conclusion onto the rung, and what the board says about that rung -- when it
was last looked at, whether it is still due -- is what the note has just
changed."
  (interactive)
  (unless (equal (buffer-name) org-convect-review-buffer)
    (user-error "Not the review board"))
  ;; NOW is dropped: redrawing means asking again, and asking again at the
  ;; moment the board was first opened would be asking the old question.
  (org-convect-review (car org-convect--review-args)))

(defconst org-convect-review-commands
  '(("note" . "write against a rung from anywhere")
    ("lineage-show" . "one thread of the ladder")
    ("history-show" . "what has happened to this rung")
    ("link" . "what it is in service of  (C-u: what serves it)")
    ("relink" . "repoint or drop one of its links")
    ("set-date" . "when a goal is meant to be true")
    ("reword" . "rename it, and follow the references")
    ("doctor" . "what the ladder is missing"))
  "The commands worth naming at the foot of the board, and what each is for.

Named rather than bound.  Keys are worth taking from Org only where the board
is the natural place to press them, and that is three keys -- the rest are
things you reach for a few times a year, where the cost of a key you have to
remember is higher than the cost of typing a name.  Naming them is what stops
that being the same as hiding them.")

(defun org-convect--review-legend (threaded only-wanting)
  "The foot of the board: the keys it binds, then the commands it does not.

A view whose whole point is that you come back to it every month is a view you
will have forgotten the keys to.  So it says them, and it says what else there
is -- a command nobody can find is a command that is not there."
  (concat
   "\n"
   (format "  RET  go to it     z  review it     %s     r  redraw\n"
           (if threaded "t  by altitude" "t  follow the links"))
   (if only-wanting
       "  M-x org-convect-review  shows the whole ladder again\n"
     "  C-u M-x org-convect-review  shows only what wants looking at\n")
   "\n  M-x org-convect-...\n"
   (mapconcat (lambda (pair)
                (format "    %-14s %s" (car pair) (cdr pair)))
              org-convect-review-commands "\n")
   "\n"))

(defun org-convect--review-rows (entries only-wanting wanted)
  "The descent's rows, narrowed to what WANTED keeps when ONLY-WANTING.

A tree cannot be filtered row by row: dropping a rung orphans everything drawn
under it, and the connectors then point at nothing.  So an ancestor of a rung
that is wanted is kept as well, and the prefixes are built again afterwards --
which of a rung's siblings is the last one changes when the others go."
  (let ((rows (org-convect-outline entries)))
    (if (not only-wanting)
        rows
      (let* ((keep (make-hash-table :test 'eq))
             (stack nil))
        ;; Walking backwards, a row's ancestors are the rows above it whose
        ;; depth is smaller -- which is what the stack holds.
        (dolist (row rows)
          (setq stack (cons row (seq-drop-while
                                 (lambda (r) (>= (nth 1 r) (nth 1 row)))
                                 stack)))
          (when (funcall wanted (car row))
            (dolist (up stack) (puthash up t keep))))
        (org-convect--review-reprefix
         (seq-filter (lambda (r) (gethash r keep)) rows))))))

(defun org-convect--review-reprefix (rows)
  "ROWS with their connectors built again for the tree that is left."
  (let ((glyphs (org-convect-outline-glyphs))
        (prefixes (make-vector (1+ (apply #'max 0 (mapcar (lambda (r) (nth 1 r))
                                                          rows)))
                               ""))
        out)
    (pcase-let ((`(,_bar ,tee ,ell ,_gap) glyphs))
      (let ((rest rows))
        (while rest
          (pcase-let ((`(,entry ,depth ,_prefix ,also) (car rest)))
            (let* ((parent (if (zerop depth) "" (aref prefixes (1- depth))))
                   ;; the last sibling is the last row at this depth before
                   ;; anything shallower comes along
                   (last (not (seq-find (lambda (r) (= (nth 1 r) depth))
                                        (seq-take-while
                                         (lambda (r) (> (nth 1 r) (1- depth)))
                                         (cdr rest)))))
                   (prefix (if (zerop depth) ""
                             (concat (org-convect-outline-under parent glyphs)
                                     (if last ell tee)))))
              (when (< depth (length prefixes)) (aset prefixes depth prefix))
              (push (list entry depth prefix also) out)))
          (setq rest (cdr rest)))))
    (nreverse out)))

;;;; One thread, and one rung's past

(defun org-convect--pick (entries prompt)
  "The rung at point when there is one, otherwise one read with PROMPT."
  (or (let ((marker (and (derived-mode-p 'org-agenda-mode)
                         (org-get-at-bol 'org-hd-marker))))
        (and marker
             (seq-find (lambda (e) (equal (marker-position (plist-get e :marker))
                                          (marker-position marker)))
                       entries)))
      (let ((here (org-convect--rung-at-point)))
        (and here (seq-find (lambda (e) (equal (plist-get e :name)
                                               (plist-get here :name)))
                            entries)))
      (org-convect-read-entry prompt entries)))

;;;###autoload
(defun org-convect-lineage-show ()
  "Show one thread of the ladder: the rung at point and everything linked to it.

Reached from the review board, from the file, or by name.  The board answers
\"what wants looking at\"; this answers \"what is this finally for\", which is
a different question and the one the file cannot be read for -- links point up
only, so what serves a rung is never written near it."
  (interactive)
  (let* ((now (current-time))
         (entries (org-convect-scan))
         (entry (org-convect--pick entries "Thread through: "))
         (thread (org-convect-lineage entries entry))
         (called (org-convect-called entries))
         (scan (org-convect-clock-rows))
         (buffer (get-buffer-create "*Horizons Thread*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'org-agenda-mode) (org-agenda-mode))
        (setq-local org-agenda-type 'agenda)
        (insert (format "Thread through %s\n\n" (plist-get entry :name)))
        (dolist (horizon (reverse (mapcar #'car org-convect-horizons)))
          (let ((at (seq-filter (lambda (e) (eq (plist-get e :horizon) horizon))
                                thread)))
            (when at
              (insert (org-convect-horizon-name horizon) "\n")
              (dolist (e at)
                (insert (org-convect--review-line
                         (format "  %s%s%s\n" (plist-get e :name)
                                 (if (equal (plist-get e :name)
                                            (plist-get entry :name))
                                     "   <- from here" "")
                                 (if (org-convect-thread-ends-at-p e)
                                     "   -- serves nothing above" ""))
                         (plist-get e :marker)
                         (plist-get e :name)))
                (insert (format "      %s\n"
                                (org-convect--review-status e now called)))
                ;; The whole body here, not the glimpse the board gives.  A
                ;; view that narrowed to a handful of rungs and then said less
                ;; about each of them would be narrowing for nothing.
                (dolist (line (append (org-convect--evidence-body e 6)
                                      (delq nil (list (org-convect--evidence-tally
                                                       e entries scan)))))
                  (insert (format "      %s\n" line))))
              (insert "\n"))))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun org-convect-history-show ()
  "Show what has happened to the rung at point, newest first."
  (interactive)
  (let* ((entries (org-convect-scan))
         (entry (org-convect--pick entries "History of: "))
         (history (org-convect-history entry))
         (buffer (get-buffer-create "*Horizons History*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'org-agenda-mode) (org-agenda-mode))
        (setq-local org-agenda-type 'agenda)
        (insert (org-convect--review-line
                 (format "%s\n" (plist-get entry :name))
                 (plist-get entry :marker)
                 (plist-get entry :name)))
        ;; What it says now, before how it came to say it.  A history read
        ;; without the present tense of the thing is a list of edits.
        (dolist (line (split-string
                       (org-with-point-at (plist-get entry :marker)
                         (org-convect--body))
                       "\n" t))
          (insert (format "  %s\n" (string-trim line))))
        (insert "\n")
        (if (null history)
            (insert "  nothing yet\n")
          (pcase-dolist (`(,time ,kind ,text) history)
            (insert (format "  %s  %-9s %s\n"
                            (format-time-string "%Y-%m-%d" time) kind
                            (car (split-string (string-trim text) "\n" t))))
            (dolist (line (cdr (split-string (string-trim text) "\n" t)))
              (insert (format "  %22s%s\n" "" (string-trim line))))))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(provide 'org-convect-core)

;;; org-convect-core.el ends here
