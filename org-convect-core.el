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
;; The names and the review frequencies are David Allen Company's, taken from
;; "Horizons of Focus" (05OCT2016-v1), not paraphrased:
;;
;;   HORIZON 5  Purpose and Principles            whenever clarity is needed
;;   HORIZON 4  Vision                            whenever clarity is needed
;;   HORIZON 3  Goals and Objectives              annually; quarterly
;;   HORIZON 2  Areas of Focus and Accountability monthly personal check-in's
;;   HORIZON 1  Projects                          weekly review
;;   GROUND     Calendar/Actions                  multiple times daily
;;
;; H1 and Ground are org-foresight's; this package holds H2 upward.
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

(defcustom org-convect-review-cadence
  '((area . 30)
    (goal . 90))
  "Days after which a horizon is due to be looked at again, by horizon.

The two numbers are GTD's own: areas get \"monthly personal check-in's\" and
goals get \"quarterly reviews and recalibrations\".

`vision' and `purpose' are absent on purpose.  GTD gives them no calendar at
all -- they are visited \"whenever additional clarity, direction, alignment,
and motivation are needed\" -- and inventing a yearly deadline for them would
be this package making up a rule and attributing it to GTD.  What calls them
is a signal, not a date; see `org-convect-act-drift' for one."
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
              :reviewed (org-convect--last-reviewed)
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

A horizon with no cadence is never overdue -- not because it never needs
looking at, but because a date is the wrong thing to call it with."
  (let ((days (org-convect-cadence-days (plist-get entry :horizon)))
        (reviewed (plist-get entry :reviewed)))
    (when days
      (or (null reviewed)
          (> (/ (float-time (time-subtract (or now (current-time)) reviewed))
                86400)
             days)))))

(defun org-convect-next-rung (entries)
  "The horizon that should be prompted for next, or nil when all are filled.

GTD's order made mechanical: the lowest empty rung, and only that one.  While
areas are still unwritten it asks for areas -- not for purpose, which is a
question nobody buried in the day can answer honestly.  An empty rung is the
prescribed path, not a defect; what would be a defect is never being asked to
climb off it."
  (seq-find (lambda (horizon) (null (org-convect-entries entries horizon)))
            (mapcar #'car org-convect-horizons)))

;;;; Findings

(defun org-convect-findings (entries &optional now)
  "Everything wrong with the ladder in ENTRIES, as a list of plists.

Each is (:kind :name :horizon :marker :detail).  The kinds:

  unknown-horizon   `CONVECT_HORIZON' names no rung -- a typo, silently
                    invisible to every other question if it were dropped
  duplicate-name    two rungs share a name, so pointing at it means nothing
  unresolved-serves `CONVECT_SERVES' names something that is not there
  unserved-goal     a goal nothing points at.  The one direction worth asking
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
          (when (org-convect-overdue-p entry now)
            (finding 'overdue-review
                     (if (plist-get entry :reviewed)
                         (format-time-string "%Y-%m-%d" (plist-get entry :reviewed))
                       "never"))))))
    (dolist (goal (org-convect-entries entries 'goal))
      (let ((name (plist-get goal :name)))
        (unless (seq-some (lambda (e) (member name (plist-get e :serves))) entries)
          (push (list :kind 'unserved-goal :name name :horizon 'goal
                      :marker (plist-get goal :marker) :detail nil)
                findings))))
    (nreverse findings)))

;;;; Where the time actually went

(defcustom org-convect-clock-window 30
  "Days of clock history read when no scan is supplied.
Matched to the areas cadence: a monthly check-in asks about the month."
  :type 'integer
  :group 'org-convect)

(defun org-convect-unclaimed-categories (entries &optional scan)
  "Clocked categories that no area in ENTRIES claims, as (CATEGORY . MINUTES).

SCAN is a plist from `org-foresight-clock-scan'; one is fetched when that
package is loaded and none is given.  The dependency is soft both ways: the
ladder reads without a clock, and the clock reads without a ladder.

This is the question the whole package exists for.  Hours went somewhere, and
some of that somewhere is not among the things claimed to matter.  The finding
is not an accusation: a chore that shows up here every month is usually an area
of accountability that was never named, and naming it is the fix."
  (let* ((scan (or scan
                   (and (fboundp 'org-foresight-clock-scan)
                        (org-foresight-clock-scan org-convect-clock-window))))
         (declared (mapcar (lambda (e) (plist-get e :name))
                           (org-convect-entries entries 'area))))
    (seq-remove (lambda (row) (member (car row) declared))
                (plist-get scan :rows))))

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

;;;; The file itself

(defcustom org-convect-skeleton
  (concat "#+title: Horizons\n"
          "#+COLUMNS: %40ITEM(Item) %CONVECT_HORIZON(Horizon)"
          " %CONVECT_SERVES(Serves)\n\n"
          (mapconcat (lambda (h) (format "* %s\n" (cdr h)))
                     org-convect-horizons ""))
  "Written into the first of `org-convect-files' when it does not exist.

Sections are scaffolding only -- entries are found by `CONVECT_HORIZON', so
moving one out of its section changes nothing.  They are ordered lowest first
because that is the order the rungs get written.

What this creates is the empty frame, not a suggestion of what to put in it:
GTD's advice for filling in areas is to copy them off the job description, the
org chart and the household's division of labour, and a template guessing at
them would get in the way of the inventory."
  :type 'string
  :group 'org-convect)

(defun org-convect--frame ()
  "Write `org-convect-skeleton' into this buffer when it is empty."
  (when (= (point-min) (point-max))
    (insert org-convect-skeleton)))

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

(provide 'org-convect-core)

;;; org-convect-core.el ends here
