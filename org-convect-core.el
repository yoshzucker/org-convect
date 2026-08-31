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

(defun org-convect--last-reviewed ()
  "Newest inactive timestamp written on the entry at point, or nil.

A review is a note (`org-add-note', \\[org-add-note]): \"what happened to this
subject\", which is exactly what a note is for and exactly what a headline is
not.  Depending on `org-log-into-drawer' the note lands in a LOGBOOK drawer or
as a plain list item under the heading; both carry the timestamp, so both are
read the same way here.

Only the entry's *own* text counts.  A choice point recorded under a principle
is a child, and recording one is not the same act as reviewing the principle."
  (save-excursion
    (org-back-to-heading t)
    (let ((bound (save-excursion (outline-next-heading) (point)))
          (newest nil))
      (org-end-of-meta-data)
      (when (looking-at org-property-drawer-re)
        (goto-char (match-end 0)))
      (while (re-search-forward org-ts-regexp-inactive bound t)
        (let ((time (org-time-string-to-time (match-string 1))))
          (when (or (null newest) (time-less-p newest time))
            (setq newest time))))
      newest)))

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
    (org-back-to-heading t)
    (let ((bound (save-excursion (outline-next-heading) (point))))
      (org-end-of-meta-data)
      ;; Past every drawer, not only the properties.  A LOGBOOK is the
      ;; machine's record of what happened to the rung; what is being looked
      ;; for here is the sentence a person wrote saying what the rung *is*.
      (while (and (< (point) bound) (looking-at org-drawer-regexp))
        (if (re-search-forward "^[ \t]*:END:[ \t]*$" bound t)
            (forward-line 1)
          (goto-char bound))
        (skip-chars-forward " \t\n"))
      (string-trim (buffer-substring-no-properties (min (point) bound) bound)))))

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
            (unless (gethash target index)
              (finding 'unresolved-serves target)))
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
                   (not (seq-some (lambda (e) (member name (plist-get e :serves)))
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
         (table (mapcar (lambda (e)
                          (cons (format "%s  [%s]"
                                        (plist-get e :name)
                                        (plist-get e :horizon))
                                e))
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

Under each heading, what it rules out: the behaviour you would refuse even at
a cost.  A principle with nothing it forbids is a slogan, and nothing can be
measured against it."
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

An area with no goal is an area in good order.  It is maintained, not achieved;
if the standard is being met there is nothing to aim at, and inventing
something produces a goal you will not pursue.  A goal appears when something
has to become *different*: the standard itself needs to rise, or something is
in the way of meeting it, or a thing that does not exist yet is wanted.

So look at the areas for pressure, not for material -- an area you could not
write a standard for is often one waiting to be told what it is for, and
coming back down with a goal in hand is how that gets said.  But the goals
themselves usually come from somewhere else, and usually they are already
written down: this year's objectives, a plan someone is holding you to, the
thing you told your family you would do.  Copy them, the way the areas were
copied."
     :write  "Under each one, as plain lines, how you would know it had
happened.  Nothing else -- no label, no date in the text.  A goal you cannot
judge is a vision that has been given a date.

The date is a property, `CONVECT_BY', so that it can be read rather than
noticed: M-x org-convect-set-date, or answer the prompt when adding one.  A
goal still sitting there after its date has been reached, abandoned, or was
never a goal, and the review says so.

Which areas a goal changes is a link, and links only ever run upward -- the
area names the goal, never the reverse.  Standing on the goal, where you know
the answer, C-u M-x org-convect-link writes it onto the areas you pick."
     :test   "Can you say what would count as having reached it, and roughly
when?

If you cannot say what counts, it is still a vision.  If it never finishes at
all, it is an area you maintain or a principle you hold."
     :examples "Yes: \"Vo2max of 55 by the end of the year.\"  There is a
number and a date, and on the day it is true you are done.

No: \"Stay fit.\"  Nothing about that finishes, which makes it an area -- and
its standard is what \"fit\" means from week to week."
     :when   "Yearly, with a look each quarter."
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
     :test   "Does it finish?  It must not.  Do the words \"kept up\" attach to
it?  They must.  Are you answerable for it?  You must be.

If it finishes it is a goal or a project.  If it is a way of behaving rather
than a thing held to a standard, it is a principle."
     :examples "Yes: \"engineering\", kept up meaning reviews come back the same
day and no branch is older than a week.

No: \"Ship the migration.\"  It finishes, so it is a project -- it belongs in
the task system, carrying this area's name in `CONVECT_AREA'."
     :when   "Monthly, and whenever the job or the household changes."
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

The fields, in the order they are read:

  :what   what this rung is
  :find   where to look for yours
  :write  what goes underneath, once the heading exists
  :when   how often to come back to it
  :note   anything mechanical worth knowing
  :do     the keystrokes, last, because that is what you want when the
          reading is done

`:write' is the one that gets forgotten.  A rung is a heading so that it can go
on accumulating, and a file full of bare headings is a file where the review
has nothing to be a review *of*.

`:prompt' and `:hint' are the minibuffer's share of the same text.

Everything here is written from scratch.  GTD's altitude names are kept exactly
because they are the interface to everything written about the model, but the
descriptions are not the David Allen Company's text."
  :type '(alist :key-type symbol :value-type plist)
  :group 'org-convect)

(defun org-convect-guide (horizon field)
  "The FIELD of HORIZON's entry in `org-convect-horizon-guide'."
  (plist-get (alist-get horizon org-convect-horizon-guide) field))

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

(defun org-convect--fill (text)
  "TEXT wrapped to a width that reads in a narrow window.

The source strings are broken where the source is easiest to read, which is
not where the file should break.  Joining and refilling keeps the two apart:
a sentence end keeps its two spaces, every other line break becomes one space.

A blank line is not a line break in that sense -- it is a paragraph, and the
one thing in the source that means what it says -- so those survive and each
paragraph is filled on its own."
  (mapconcat
   (lambda (paragraph)
     (with-temp-buffer
       (insert (replace-regexp-in-string
                "\n[ \t]*" " "
                (replace-regexp-in-string "\\([.?!]\\)\n[ \t]*" "\\1  "
                                          (string-trim paragraph))))
       (let ((fill-column 72))
         (fill-region (point-min) (point-max)))
       (buffer-string)))
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
          (mapconcat #'org-convect--fill
                     (delq nil (mapcar (lambda (field)
                                         (org-convect-guide horizon field))
                                       '(:what :test :find :examples :write :when :note :do)))
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
  "The rung at point as a plist, or nil when point is not on one."
  (and (derived-mode-p 'org-mode)
       (ignore-errors (org-back-to-heading t) t)
       (org-convect--entry-at-point (buffer-file-name))))

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
      (if (org-string-nw-p date)
          (org-entry-put nil "CONVECT_BY" (format "[%s]" date))
        (org-entry-delete nil "CONVECT_BY")))))

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
               (chosen (and below
                            (completing-read-multiple
                             (format "Rungs served by %s: " name)
                             (mapcar (lambda (e) (plist-get e :name)) below)))))
          (unless below (user-error "Nothing sits below a %s" horizon))
          (dolist (pick (seq-remove #'string-empty-p chosen))
            (let ((entry (seq-find (lambda (e) (equal (plist-get e :name) pick))
                                   below)))
              (org-convect--add-serves (plist-get entry :marker) name)))
          (message "%s is now served by %d rung%s" name (length chosen)
                   (if (= 1 (length chosen)) "" "s")))
      (let* ((above (seq-filter (lambda (e)
                                  (memq (plist-get e :horizon)
                                        (org-convect-above horizon)))
                                entries))
             (chosen (and above
                          (completing-read-multiple
                           (format "%s is in service of: " name)
                           (mapcar (lambda (e) (plist-get e :name)) above)))))
        (unless above (user-error "Nothing sits above a %s" horizon))
        (dolist (pick (seq-remove #'string-empty-p chosen))
          (org-convect--add-serves (plist-get here :marker) pick))
        (message "%s now serves %d rung%s" name (length chosen)
                 (if (= 1 (length chosen)) "" "s"))))))

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

A note this package wrote when a rung was reworded says so in its first line,
which is how the two kinds are told apart.  That is text matching and it can
be fooled by someone writing the same words by hand -- but the words are
machine-written, the failure is a mislabelled row, and the alternative is
giving notes a property, which would make them addressable when the whole
point of a note is that it is not."
  (save-excursion
    (org-back-to-heading t)
    (let ((bound (save-excursion (outline-next-heading) (point)))
          notes)
      (org-end-of-meta-data)
      (when (looking-at org-property-drawer-re)
        (goto-char (match-end 0)))
      (while (re-search-forward
              (concat "^[ \t]*- .*?" org-ts-regexp-inactive) bound t)
        (let* ((time (org-time-string-to-time (match-string 1)))
               (start (progn (forward-line 1) (point)))
               (end (save-excursion
                      (if (re-search-forward "^[ \t]*- \\|\\'" bound t)
                          (match-beginning 0)
                        bound)))
               (text (string-trim (buffer-substring-no-properties
                                   start (min end bound)))))
          (push (list time
                      (if (string-match-p "\\`Reworded from" text) 'reworded 'reviewed)
                      text)
                notes)))
      notes)))

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

(defun org-convect--note (text)
  "Write TEXT as a dated note on the entry at point.

Written straight rather than through `org-add-log-setup': that machinery sends
the note wherever `org-log-into-drawer' happens to point, and its value belongs
to whatever was logged last.  A note this package writes should land somewhere
it can predict.  The shape is Org's own, so it reads as any other note and
`org-convect--last-reviewed' finds its timestamp either way."
  (org-back-to-heading t)
  (org-end-of-meta-data)
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0)))
  (unless (bolp) (insert "\n"))
  (insert (format "- Note taken on %s \\\\\n  %s\n"
                  (format-time-string (org-time-stamp-format t t))
                  text)))

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

(defconst org-convect--finding-labels
  '((bare-rung         . "Nothing written under it")
    (long-heading      . "Long for a name -- if it is a sentence, it belongs in the body")
    (undated-goal      . "No date")
    (past-its-date     . "Past its date")
    (unserved-rung     . "Nothing below points at it")
    (unresolved-serves . "Serves something that is not there")
    (duplicate-name    . "Shares its name with another rung")
    (unknown-horizon   . "Not a horizon this package knows")
    (overdue-review    . "Due to be looked at"))
  "One line of English per finding kind, for the doctor's report.")

;;;###autoload
(defun org-convect-doctor ()
  "Show what the ladder has and what it is missing.  Writes nothing.

The guidance says what goes under each rung and the file cannot show that it
is missing -- a heading with an empty body looks exactly like a heading.  So
this counts them.

It is a report and not a scold.  Several of the things it lists are fine to
leave: a rung can stay bare on purpose while you go up and fetch the goal that
says what it is for, and most areas serve nothing at all.  What it is for is
that you should be choosing to leave them, rather than not noticing."
  (interactive)
  (let* ((entries (org-convect-scan))
         (findings (org-convect-findings entries))
         (buffer (get-buffer-create org-convect-doctor-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
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
              (insert (format "\n%s\n"
                              (alist-get kind org-convect--finding-labels)))
              (dolist (f these)
                (insert (format "  %-8s %s%s\n"
                                (plist-get f :horizon)
                                (plist-get f :name)
                                (if (plist-get f :detail)
                                    (format "  (%s)" (plist-get f :detail))
                                  "")))))))
        (unless findings (insert "\nNothing missing.\n"))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

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

(defun org-convect--review-line (text &optional marker)
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
                'org-agenda-type 'agenda
                'help-echo "z add note · RET go to it")))

(defun org-convect--review-evidence (entry entries scan)
  "Lines of evidence to show under ENTRY on the board."
  (let* ((body (org-with-point-at (plist-get entry :marker)
                 (org-convect--body)))
         (first (car (split-string body "\n" t)))
         (near (org-convect-neighbourhood entries entry))
         (hours (cdr (assoc (plist-get entry :name)
                            (plist-get scan :rows)))))
    (append
     (and first (list (truncate-string-to-width (string-trim first) 68 nil nil t)))
     (and hours (list (format "%s clocked" (org-duration-from-minutes hours))))
     (and (car near)
          (list (concat "serves "
                        (mapconcat (lambda (e) (plist-get e :name))
                                   (car near) ", "))))
     (and (cdr near)
          (list (format "%d below point at it" (length (cdr near)))))
     (apply #'append
            (mapcar (lambda (f) (funcall f entry))
                    org-convect-review-evidence-functions)))))

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

Rows are live: \\[org-agenda-add-note] writes your conclusion onto the rung,
\\[org-agenda-switch-to] goes to it when the conclusion is that the rung
itself should change."
  (interactive "P")
  (let* ((now (or now (current-time)))
         (entries (org-convect-scan))
         (called (org-convect-called entries))
         (due (org-convect-due entries now))
         (scan (org-convect-clock-rows))
         ;; Wide enough for the widest name there is.  A fixed column would
         ;; cut a principle in half, and a principle read in half is a
         ;; different principle.
         (column (apply #'max 24 (mapcar (lambda (e)
                                           (string-width (plist-get e :name)))
                                         entries)))
         (buffer (get-buffer-create org-convect-review-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'org-agenda-mode) (org-agenda-mode))
        (setq-local org-agenda-type 'agenda)
        (insert (format "Horizons Review%s%s\n"
                        (make-string 38 ?\s) (format-time-string "%Y-%m-%d" now)))
        (insert (format "  %d due, %d called%s\n\n"
                        (length due) (length called)
                        (if org-convect-signal-functions ""
                          " (nothing is watching the rungs with no calendar)")))
        (dolist (horizon (reverse (mapcar #'car org-convect-horizons)))
          (let ((at (seq-filter
                     (lambda (e)
                       (or (not only-wanting)
                           (memq e due) (assq e called)))
                     (org-convect-entries entries horizon))))
            (when (or at (not only-wanting))
              (insert (org-convect-horizon-name horizon) "\n")
              (if (null at)
                  (insert "  nothing\n")
                (dolist (entry at)
                  (insert (org-convect--review-line
                           (format "  %s%s  %s\n"
                                   (plist-get entry :name)
                                   (make-string
                                    (max 0 (- column
                                              (string-width (plist-get entry :name))))
                                    ?\s)
                                   (org-convect--review-status entry now called))
                           (plist-get entry :marker)))
                  (dolist (line (org-convect--review-evidence entry entries scan))
                    (insert (format "      %s\n" line)))))
              (insert "\n"))))
        (insert "  z  write the conclusion here      RET  go to it\n")
        (when (not only-wanting)
          (insert "  C-u M-x org-convect-review  shows only what wants looking at\n"))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

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
  (let* ((entries (org-convect-scan))
         (entry (org-convect--pick entries "Thread through: "))
         (thread (org-convect-lineage entries entry))
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
                         (plist-get e :marker))))
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
                 (format "%s\n\n" (plist-get entry :name))
                 (plist-get entry :marker)))
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
