;;; org-convect-core-test.el --- Tests for org-convect  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run from the package root:
;;
;;   emacs --batch -Q -L . -l test/org-convect-core-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Loading this file alone is the ladder without the overlay, which is a
;; supported way to run the package and therefore has to be a way to run its
;; tests:
;;
;;   emacs --batch -Q -L . -l test/org-convect-core-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Every test here was checked against a deliberately broken implementation
;; before being kept.  A test that passes either way is not a test.

;;; Code:

(require 'ert)
(require 'seq)
(require 'org-convect-core)

(defconst org-convect-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "This file's directory, so the seam test can find the source it reads.")

;;;; Fixture

(defconst org-convect-test--ladder "\
#+title: Test

* Areas of Focus and Accountability
** engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: A team that runs itself
:ACT_DOMAIN: work
:END:
- Note taken on [2026-08-01 Sat] \\\\
  still mine
- Note taken on [2026-08-20 Thu] \\\\
  still mine, and quieter

*** a task that happens to live here
This carries no CONVECT_HORIZON, so it is not a rung -- being filed under one
is not the same as being one.

** procurement
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
:LOGBOOK:
- Note taken on [2026-08-18 Tue] \\\\
  the vendor list is current
:END:

** admin
:PROPERTIES:
:CONVECT_HORIZON: area
:END:

* Goals and Objectives
** A team that runs itself
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_SERVES: Honesty; A vision nobody wrote
:END:

** A goal nothing serves
:PROPERTIES:
:CONVECT_HORIZON: goal
:END:

* Purpose and Principles
** Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
*** Wanted to keep quiet about the estimate
:PROPERTIES:
:CREATED: [2026-08-10 Mon]
:ACT_MOVE: away
:ACT_STRUGGLE: 7
:END:
*** Rounded the number down
:PROPERTIES:
:CREATED: [2026-08-25 Tue]
:ACT_MOVE: away
:END:
*** Let the estimate stand uncorrected
:PROPERTIES:
:CREATED: [2026-08-20 Thu]
:ACT_MOVE: away
:END:

** Craft
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
*** Shipped it without the test
:PROPERTIES:
:CREATED: [2026-08-11 Tue]
:ACT_MOVE: away
:END:
*** Wrote the test first
:PROPERTIES:
:CREATED: [2026-08-24 Mon]
:ACT_MOVE: towards
:END:
*** Went back and covered it
:PROPERTIES:
:CREATED: [2026-08-22 Sat]
:ACT_MOVE: away
:END:
"
  "A ladder with every shape the code has to survive.

It is deliberately not tidy: a non-rung heading filed under an area, a review
written as a plain list item and another written into a LOGBOOK, an area with
no review at all, a name pointed at that does not exist, a goal nothing
serves, and choice points whose document order is not their date order.")

(defconst org-convect-test--ladder-written "\
* Areas of Focus and Accountability
** engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: A team that runs itself
:END:
reviews come back the same day; no branch older than a week

* Goals and Objectives
** A team that runs itself
:PROPERTIES:
:CONVECT_HORIZON: goal
:END:
I would know because the on-call rota has someone else on it

* Purpose and Principles
** Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
I do not let a number stand that I know is wrong
"
  "A ladder whose lower rungs are finished rather than merely named.

The difference matters to `org-convect-next-rung', which will not send anyone
up off a level whose headings have nothing written under them.  Vision is the
gap here, and vision is what it should ask for.")

(defmacro org-convect-test--with-ladder (text &rest body)
  "Run BODY with `org-convect-files' holding one temporary file of TEXT."
  (declare (indent 1) (debug t))
  `(let* ((file (make-temp-file "org-convect-test" nil ".org" ,text))
          (org-convect-files (list file))
          (buffer nil))
     (unwind-protect
         (progn (setq buffer (find-file-noselect file)) ,@body)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (ignore-errors (delete-file file)))))

(defun org-convect-test--names (entries)
  "The names in ENTRIES, sorted, so a failure reads as a set difference."
  (sort (mapcar (lambda (e) (plist-get e :name)) entries) #'string<))

(defun org-convect-test--kinds (findings name)
  "Sorted finding kinds reported against NAME in FINDINGS."
  (sort (mapcar (lambda (f) (plist-get f :kind))
                (seq-filter (lambda (f) (equal (plist-get f :name) name))
                            findings))
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(defun org-convect-test--entry (entries name)
  "The entry of ENTRIES called NAME."
  (seq-find (lambda (e) (equal (plist-get e :name) name)) entries))

(defun org-convect-test--day (year month day)
  "A time value for YEAR-MONTH-DAY at midnight."
  (encode-time 0 0 0 day month year))

;;;; The ladder is declared, not located

(ert-deftest org-convect-test-scan-reads-only-declared ()
  "A heading filed under a rung is not a rung."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((entries (org-convect-scan)))
      (should (equal (org-convect-test--names entries)
                     '("A goal nothing serves" "A team that runs itself"
                       "Craft" "Honesty" "admin" "engineering" "procurement")))
      (should-not (org-convect-test--entry
                   entries "a task that happens to live here")))))

(ert-deftest org-convect-test-scan-ignores-inheritance ()
  "A child does not become a rung because its parent declared one."
  (let ((org-use-property-inheritance t))
    (org-convect-test--with-ladder org-convect-test--ladder
      (let ((entries (org-convect-scan)))
        (should (= (length entries) 7))
        (should-not (org-convect-test--entry
                     entries "a task that happens to live here"))
        (should-not (org-convect-test--entry
                     entries "Rounded the number down"))))))

(ert-deftest org-convect-test-scan-finds-nothing-in-a-plain-file ()
  "A file with headings and no declarations yields no ladder at all."
  (org-convect-test--with-ladder "* one\n** two\n* three\n"
    (should-not (org-convect-scan))))

;;;; Several parents

(ert-deftest org-convect-test-serves-holds-several ()
  "`CONVECT_SERVES' carries more than one name, which is why a rung can
have two parents at all."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((goal (org-convect-test--entry (org-convect-scan)
                                         "A team that runs itself")))
      (should (equal (plist-get goal :serves)
                     '("Honesty" "A vision nobody wrote"))))))

(ert-deftest org-convect-test-serves-absent-is-empty ()
  "No `CONVECT_SERVES' is an empty list, never a list holding nothing."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((area (org-convect-test--entry (org-convect-scan) "admin")))
      (should-not (plist-get area :serves)))))

;;;; Reviews

(ert-deftest org-convect-test-reviewed-is-the-newest ()
  "The last review is the newest note, not the first one written."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((area (org-convect-test--entry (org-convect-scan) "engineering")))
      (should (equal (format-time-string "%Y-%m-%d" (plist-get area :reviewed))
                     "2026-08-20")))))

(ert-deftest org-convect-test-reviewed-reads-a-logbook-too ()
  "A note in a LOGBOOK drawer counts the same as one written in the open."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((area (org-convect-test--entry (org-convect-scan) "procurement")))
      (should (equal (format-time-string "%Y-%m-%d" (plist-get area :reviewed))
                     "2026-08-18")))))

(ert-deftest org-convect-test-reviewed-ignores-children ()
  "Recording a choice point under a principle is not reviewing the principle."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((purpose (org-convect-test--entry (org-convect-scan) "Honesty")))
      (should-not (plist-get purpose :reviewed)))))

(ert-deftest org-convect-test-reviewed-ignores-the-property-drawer ()
  "A stamp in the drawer is metadata about the entry, not a look at it."
  (org-convect-test--with-ladder "\
* thing
:PROPERTIES:
:CONVECT_HORIZON: area
:CREATED: [2026-08-15 Sat]
:END:
"
    (should-not (plist-get (car (org-convect-scan)) :reviewed))))

;;;; Cadence, and the two rungs that have none

(ert-deftest org-convect-test-overdue-follows-the-cadence ()
  "An area is due monthly and a goal quarterly, per GTD's own frequencies."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let* ((entries (org-convect-scan))
           (area (org-convect-test--entry entries "engineering")))
      (should-not (org-convect-overdue-p
                   area (org-convect-test--day 2026 9 10)))
      (should (org-convect-overdue-p
               area (org-convect-test--day 2026 10 10))))))

(ert-deftest org-convect-test-never-reviewed-is-due ()
  "A rung nobody has looked at is due, so the first check-in includes it."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((area (org-convect-test--entry (org-convect-scan) "admin")))
      (should (org-convect-overdue-p area (org-convect-test--day 2026 8 25))))))

(ert-deftest org-convect-test-the-upper-rungs-have-no-calendar ()
  "Vision and purpose are never overdue: GTD gives them no frequency, and
inventing one would be this package making up a rule and calling it GTD."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((purpose (org-convect-test--entry (org-convect-scan) "Honesty")))
      (should-not (plist-get purpose :reviewed))
      (should-not (org-convect-overdue-p
                   purpose (org-convect-test--day 2030 1 1)))
      (should-not (org-convect-cadence-days 'purpose))
      (should-not (org-convect-cadence-days 'vision)))))

;;;; Findings

(ert-deftest org-convect-test-unresolved-serves-is-reported ()
  "A name pointing at nothing is surfaced, never silently dropped."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((findings (org-convect-findings (org-convect-scan)
                                          (org-convect-test--day 2026 8 25))))
      (should (seq-find (lambda (f)
                          (and (eq (plist-get f :kind) 'unresolved-serves)
                               (equal (plist-get f :detail) "A vision nobody wrote")))
                        findings))
      (should-not (seq-find (lambda (f)
                              (and (eq (plist-get f :kind) 'unresolved-serves)
                                   (equal (plist-get f :detail) "Honesty")))
                            findings)))))

(ert-deftest org-convect-test-duplicate-names-are-reported ()
  "Two rungs sharing a name make pointing at it meaningless, so it is a break."
  (org-convect-test--with-ladder "\
* one
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
* one
:PROPERTIES:
:CONVECT_HORIZON: goal
:END:
"
    (let ((findings (org-convect-findings (org-convect-scan))))
      (should (= 2 (length (seq-filter
                            (lambda (f) (eq (plist-get f :kind) 'duplicate-name))
                            findings)))))))

(ert-deftest org-convect-test-unknown-horizon-is-reported ()
  "A typo in `CONVECT_HORIZON' is visible rather than invisible."
  (org-convect-test--with-ladder "\
* one
:PROPERTIES:
:CONVECT_HORIZON: aera
:END:
"
    (let ((findings (org-convect-findings (org-convect-scan))))
      (should (seq-find (lambda (f) (eq (plist-get f :kind) 'unknown-horizon))
                        findings)))))

(ert-deftest org-convect-test-unserved-rung-is-reported ()
  "The ladder is read upward: a goal nothing serves is the question worth
asking, and a goal that is served is not."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((findings (org-convect-findings (org-convect-scan)
                                          (org-convect-test--day 2026 8 25))))
      (should (memq 'unserved-rung
                    (org-convect-test--kinds findings "A goal nothing serves")))
      (should-not (memq 'unserved-rung
                        (org-convect-test--kinds
                         findings "A team that runs itself"))))))

(ert-deftest org-convect-test-chores-are-not-blamed ()
  "An area serving nothing is not a defect.  Keeping the engines running is
what an area is for, and the company's chores are a real accountability."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((findings (org-convect-findings (org-convect-scan)
                                          (org-convect-test--day 2026 8 25))))
      (should (equal (org-convect-test--kinds findings "admin")
                     '(bare-rung overdue-review)))
      (should-not (memq 'unserved-rung
                        (org-convect-test--kinds findings "admin")))
      (should (equal (org-convect-test--kinds findings "procurement")
                     '(bare-rung))))))

;;;; Which rung to ask for next

(ert-deftest org-convect-test-next-rung-is-the-lowest-empty ()
  "GTD's order made mechanical: with areas and goals finished and no vision,
what gets asked for is the vision -- not the purpose above it."
  (org-convect-test--with-ladder org-convect-test--ladder-written
    (should (eq (org-convect-next-rung (org-convect-scan)) 'vision))))

(ert-deftest org-convect-test-next-rung-starts-at-the-bottom ()
  "With nothing written, the question is areas.  Nobody buried in the day can
answer about purpose, and asking anyway is how the exercise gets abandoned."
  (should (eq (org-convect-next-rung nil) 'area)))

(ert-deftest org-convect-test-next-rung-is-nil-when-filled ()
  "Every level occupied *and* written is a finished ladder."
  (org-convect-test--with-ladder "\
* a
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
reviews come back the same day
* b
:PROPERTIES:
:CONVECT_HORIZON: goal
:END:
I would know because the on-call rota has someone else on it
* c
:PROPERTIES:
:CONVECT_HORIZON: vision
:END:
the team ships without me in the room
* d
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
I do not let a number stand that I know is wrong
"
    (should-not (org-convect-next-rung (org-convect-scan)))))

;;;; A rung that is only a name

(ert-deftest org-convect-test-bare-is-read-below-the-drawers ()
  "`org-end-of-meta-data' walks past the blank space after the drawers too, so
on an entry with nothing written it lands on the *next* heading.  A body read
from there is the next entry's property drawer, and every bare rung in the file
comes back full."
  (org-convect-test--with-ladder "\
* one
:PROPERTIES:
:CONVECT_HORIZON: area
:CREATED: [2026-08-01 Sat]
:END:
* two
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
reviews come back the same day
"
    (let ((entries (org-convect-scan)))
      (should (plist-get (org-convect-test--entry entries "one") :bare))
      (should-not (plist-get (org-convect-test--entry entries "two") :bare)))))

(ert-deftest org-convect-test-a-bare-rung-is-a-finding ()
  "Named but empty is unfinished work, and the file cannot say so itself."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "work.dev")
    (should (memq 'bare-rung
                  (org-convect-test--kinds
                   (org-convect-findings (org-convect-scan)) "work.dev")))))

(ert-deftest org-convect-test-where-to-climb-and-what-is-behind-differ ()
  "Two questions, and conflating them makes the tool an obstacle.  An area
named and left bare does not stop the ladder being climbed -- leaving one bare
is sometimes the right call, and a standard invented to fill a blank is
fiction -- but it is still worth being told about."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "work.dev")
    (let ((entries (org-convect-scan)))
      (should (eq 'goal (org-convect-next-rung entries)))
      (should (equal '(area) (org-convect-unfinished entries))))
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-max))
      (insert "reviews come back the same day\n")
      (save-buffer))
    (let ((entries (org-convect-scan)))
      (should (eq 'goal (org-convect-next-rung entries)))
      (should-not (org-convect-unfinished entries)))))

(ert-deftest org-convect-test-nothing-gates-the-climb ()
  "A ladder is partial at every moment it is any use at all, so no command
refuses to work because a level below is half-done."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "work.dev")
    (org-convect-add 'goal "a team that runs itself")
    (should (= 2 (length (org-convect-scan))))))

(ert-deftest org-convect-test-goals-are-not-one-per-area ()
  "The commonest way this level goes wrong is going down the list of areas
looking for a goal each.  There are fewer goals than areas and they cut across
them, and the guidance has to say so where the mistake is made."
  (let ((guidance (org-convect-guide 'goal :find)))
    (should (string-match-p "fewer of these than there are areas" guidance))
    (should (string-match-p "cut across" guidance))
    (should (string-match-p "no goal at all" guidance))))

(ert-deftest org-convect-test-an-area-with-no-goal-is-never-a-finding ()
  "An area is maintained, not achieved.  Pointing upward is optional and its
absence is not a defect -- only the other direction, a goal nothing serves, is
ever asked about."
  (org-convect-test--with-ladder "\
* father
:PROPERTIES:
:CONVECT_HORIZON: area
:CREATED: [2026-08-30 Sun]
:END:
there is time with them alone each week; I have not shouted
"
    (should-not (org-convect-findings (org-convect-scan)
                                      (org-convect-test--day 2026 9 5)))))

(ert-deftest org-convect-test-the-standards-test-has-a-clock-in-it ()
  "Abstract wording is what produced purpose statements where standards go, so
the question is asked with a month in it and answered with an example."
  (let ((guidance (org-convect-guide 'area :write)))
    (should (string-match-p "slipped this month" guidance))
    (should (string-match-p "Move it up to Purpose or Vision" guidance))))

;;;; Where the time went

(ert-deftest org-convect-test-the-scan-is-asked-for-areas ()
  "The binding that tells the clock scan to group by area has to be dynamic,
and org-foresight is a soft dependency that may not be loaded when this file
is compiled -- in which case `let' on a symbol never declared special produces
a lexical binding, the scan keeps its own default, and this package quietly
answers a different question with no error anywhere.

The stub reads the variable the way the scan would, so it sees what the scan
would have seen."
  (let (seen)
    (cl-letf (((symbol-function 'org-foresight-clock-scan)
               (lambda (&rest _)
                 (setq seen (and (boundp 'org-foresight-clock-property)
                                 org-foresight-clock-property))
                 (list :rows nil))))
      (org-convect-clock-rows)
      (should (equal seen "CONVECT_AREA")))))

(ert-deftest org-convect-test-no-width-is-imposed-by-default ()
  "The package used to put area names in the agenda's narrow category column
and held them to its width.  It does not any more, so it has no business
saying a name is too long unless you tell it where you display them."
  (should-not org-convect-area-width)
  (org-convect-test--with-ladder "\
* a name far longer than any narrow column would ever have shown
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
kept up means the tests pass
"
    (should-not (memq 'long-heading
                      (org-convect-test--kinds
                       (org-convect-findings (org-convect-scan)
                                             (org-convect-test--day 2026 9 5))
                       "a name far longer than any narrow column would ever have shown")))))

(ert-deftest org-convect-test-unattributed-time-is-its-own-answer ()
  "Two different things go wrong here and they want different fixes: time
nothing claims to be answerable for, and time booked to a name that is not an
area at all."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let* ((scan (list :rows '(("engineering" . 600)
                               ("?" . 480)
                               ("enginering" . 90)
                               ("admin" . 120))))
           (found (org-convect-unclaimed-time (org-convect-scan) scan)))
      (should (equal (car found) '(("?" . 480))))
      (should (equal (cdr found) '(("enginering" . 90)))))))
(ert-deftest org-convect-test-unattributed-time-without-a-clock ()
  "No scan and no org-foresight is an empty answer, not an error: the ladder
reads without a clock."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((found (org-convect-unclaimed-time (org-convect-scan) (list :rows nil))))
      (should-not (car found))
      (should-not (cdr found)))))
(ert-deftest org-convect-test-skeleton-is-highest-first ()
  "The file is ordered the way the ladder is read: purpose down to areas.
`org-convect-horizons' stays the other way round because that is the order the
rungs get written, and `org-convect-next-rung' walks it."
  (let ((frame (org-convect--build-skeleton))
        (case-fold-search nil))
    (should (< (string-match "Purpose and Principles" frame)
               (string-match "Vision" frame)))
    (should (< (string-match "Vision" frame)
               (string-match "Goals and Objectives" frame)))
    (should (< (string-match "Goals and Objectives" frame)
               (string-match "Areas of Focus" frame)))
    (should (equal (mapcar #'car org-convect-horizons)
                   '(area goal vision purpose)))))

(ert-deftest org-convect-test-a-fresh-frame-holds-no-rungs ()
  "Sections and their guidance are scaffolding.  A file that has only been
opened has an empty ladder, not four rungs called after the sections."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (should-not (org-convect-scan))
    (should (eq (org-convect-next-rung (org-convect-scan)) 'area))))

(ert-deftest org-convect-test-every-horizon-is-guided ()
  "No rung can be offered without something to say about it, or the command
would ask a question it gives no help with.

`:write' and `:do' are in the list because the gap they close was a real one:
the guide used to say what a rung is and where to find yours, and stop --
leaving the file full of bare headings and no word about what goes under them."
  (dolist (horizon (mapcar #'car org-convect-horizons))
    (dolist (field '(:what :test :find :examples :write :shape :when :do
                     :prompt :hint))
      (should (org-convect-guide horizon field)))))

(ert-deftest org-convect-test-the-guide-teaches-in-order ()
  "Reading comes first and doing comes last, and the test comes after there is
something to apply it to: you are shown one written out before being asked
whether yours is one."
  (dolist (horizon (mapcar #'car org-convect-horizons))
    (let* ((drawer (org-convect--guide-drawer horizon))
           (at (lambda (field)
                 (string-match
                  (regexp-quote (org-convect--guide-field horizon field))
                  drawer))))
      (let ((order '(:what :find :write :shape :test :examples :when :do)))
        (dolist (field order)
          (should (funcall at field)))
        (while (cdr order)
          (should (< (funcall at (car order)) (funcall at (cadr order))))
          (setq order (cdr order)))))))

(ert-deftest org-convect-test-every-part-of-the-guide-says-what-it-is ()
  "Nine fields answering nine different questions arrive as nine paragraphs
that look exactly alike, and sorting them is work the reader should not be
given.  The label is what says which question is being answered."
  (dolist (horizon (mapcar #'car org-convect-horizons))
    (dolist (entry org-convect-guide-fields)
      (when (org-convect-guide horizon (car entry))
        (should (string-prefix-p
                 (concat "*" (nth 1 entry) "*.")
                 (org-convect--guide-field horizon (car entry))))))))

(ert-deftest org-convect-test-the-label-is-set-off-from-the-answer ()
  "The full stop goes outside the emphasis.  Inside it, `.*' is not a sentence
end to `fill-region', which closes the two spaces after the label to one and
makes it read as the first words of the answer rather than as a label."
  (should (string-match-p "\\*What this is\\*\\.  Why"
                          (org-convect--guide-drawer 'purpose))))

(ert-deftest org-convect-test-the-guide-is-not-indented-under-a-bullet ()
  "`adaptive-fill-regexp' counts a leading `*' as a bullet, so a paragraph
opening with a bold label would have every line after the first indented one
column under a list that does not exist -- while the same field's second
paragraph, which carries no label, stayed flush."
  (dolist (horizon (mapcar #'car org-convect-horizons))
    (should-not (string-match-p "\n [^ \n]" (org-convect--guide-drawer horizon)))))

(ert-deftest org-convect-test-the-guide-shows-one-written-out ()
  "Describing a rung and showing one are different jobs, and only the second
settles whether the standard goes in the heading or underneath it.

The example needs a body: a heading on its own shows nothing that was not
already obvious, and what goes underneath is the part people get wrong.  Its
heading is indented, because at column zero it would be a heading in the file
rather than a picture of one."
  (dolist (horizon (mapcar #'car org-convect-horizons))
    (let ((lines (split-string (org-convect-guide horizon :shape) "\n")))
      (should (string-match-p "\\`  \\*\\* " (car lines)))
      (should (seq-some #'org-string-nw-p (cdr lines))))))

(ert-deftest org-convect-test-the-example-is-not-refilled ()
  "An example reflowed to 72 columns is no longer an example of anything: the
heading and the body run into one line, which is the one thing it is showing."
  (should (string-match-p "^  \\*\\* engineering$"
                          (org-convect--guide-drawer 'area))))

(ert-deftest org-convect-test-the-guide-drawer-holds-all-of-it ()
  "Nothing in the guidance may close the drawer early.

Org ends a drawer at the first line reading `:END:', so one buried in an
example would spill the rest of the guidance into the section's body, where it
would look like something the user had written.  Checked by searching the built
file the way Org does, because the line that does it need not look dangerous --
an indented property drawer in an example is enough."
  (with-temp-buffer
    (org-mode)
    (insert (org-convect--build-skeleton))
    (should (= (length org-convect-horizons)
               (length (org-element-map (org-element-parse-buffer) 'headline
                         #'identity))))
    (dolist (horizon (mapcar #'car org-convect-horizons))
      (goto-char (point-min))
      (should (re-search-forward
               (format "^\\* %s$" (regexp-quote (org-convect-horizon-name horizon)))
               nil t))
      (let* ((bound (save-excursion (org-end-of-subtree t t)))
             ;; the regexps Org itself ends a drawer by: leading whitespace
             ;; is allowed, so an indented `:END:' closes it just as well.
             (from (progn (re-search-forward "^[ \t]*:GUIDE:[ \t]*$" bound t)
                          (point)))
             (to (progn (re-search-forward "^[ \t]*:END:[ \t]*$" bound t)
                        (match-beginning 0))))
        ;; the last field is on the far side of everything before it
        (should (string-match-p
                 (regexp-quote (org-convect--guide-field horizon :do))
                 (buffer-substring-no-properties from to)))))))

(ert-deftest org-convect-test-the-guide-names-its-own-commands ()
  "Every rung's guidance ends in something you can actually type."
  (dolist (horizon (mapcar #'car org-convect-horizons))
    (should (string-match-p "org-convect-\\(add\\|declare\\)"
                            (org-convect-guide horizon :do)))))

(ert-deftest org-convect-test-the-guide-keeps-its-paragraphs ()
  "A blank line in the source is a paragraph, and filling must not eat it --
the areas guidance is two thoughts (what a rung is, and role-not-object) and
they do not read as one."
  (should (string-match-p "\n\n" (org-convect--fill "one.\n\nother.")))
  (should (string-match-p "quietly stop working\\.\n\nName the role"
                          (org-convect--guide-drawer 'area))))

(ert-deftest org-convect-test-an-indented-question-stays-indented ()
  "The areas guidance turns on one question, set on a line of its own so that
it is asked rather than read past.  Filling trims what it is given, so without
this the question arrives as one more paragraph of prose."
  (should (string-match-p "\n\n  second" (org-convect--fill "first.\n\n  second one")))
  (should (string-match-p "\n  If this had slipped this month"
                          (org-convect--guide-drawer 'area))))

(ert-deftest org-convect-test-mechanism-stays-in-the-field-labelled-for-it ()
  "A reader who cannot tell advice from a rule cannot use either.

Everything up to `:when' is about the thinking; `:note' is about the file and
`:do' is about the keys.  A property name or a command in any of the others is
a change of register with nothing to announce it, and the paragraph reads as
though the package were describing a wish rather than a mechanism."
  (dolist (horizon (mapcar #'car org-convect-horizons))
    (dolist (entry org-convect-guide-fields)
      (let ((field (car entry))
            (text (org-convect-guide horizon (car entry))))
        (when (and text (not (memq field '(:note :do))))
          (should-not (string-match-p "CONVECT_" text))
          (should-not (string-match-p "M-x" text)))))))

(ert-deftest org-convect-test-a-rung-points-at-another-without-defining-it ()
  "The goal guidance names the areas, because looking at the areas is how goals
are found.  What it must not do is say what an area *is*: that belongs to the
area's own rung, and a definition kept in two places is a definition that will
eventually disagree with itself.

The rule is pointing versus defining, so what is banned is the sentence shape a
definition takes -- \"an area is\", \"an area with\" -- rather than the word."
  (dolist (horizon (mapcar #'car org-convect-horizons))
    (dolist (entry org-convect-guide-fields)
      (let ((text (org-convect-guide horizon (car entry))))
        (dolist (other (mapcar #'car org-convect-horizons))
          (unless (eq other horizon)
            (should-not
             (and text
                  (string-match-p
                   ;; sentence-initial and capitalised, which is where a
                   ;; definition sits.  Mid-sentence the same words are
                   ;; ordinary prose -- \"producing a purpose is the most
                   ;; common thing\" defines nothing.
                   (format "\\(\\`\\|[.!?]  \\|\n\\)\\(An?\\|Each\\|Every\\) %s \\(is\\|with\\|has\\)"
                           (symbol-name other))
                   text)))))))))

(ert-deftest org-convect-test-the-areas-guide-rules-out-objects ()
  "The distinction that decides what an area may be called."
  (let ((guidance (org-convect-guide 'area :what)))
    (should (string-match-p "role or the function" guidance))
    (should (string-match-p "Children" guidance))
    (should (string-match-p "Parent" guidance))))

(ert-deftest org-convect-test-add-round-trips ()
  "What `org-convect-add' writes is what `org-convect-scan' reads."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'goal "A team that runs itself")
    (org-convect-add 'area "engineering" '("A team that runs itself"))
    (let* ((entries (org-convect-scan))
           (area (org-convect-test--entry entries "engineering")))
      (should (= (length entries) 2))
      (should (eq (plist-get area :horizon) 'area))
      (should (equal (plist-get area :serves) '("A team that runs itself")))
      ;; Everything reported is a blank the entry is *for* leaving: `add'
      ;; writes the heading, and the standard, the date and the links are
      ;; typed afterwards.  Nothing is structurally wrong.
      (should (equal (sort (delete-dups
                            (mapcar (lambda (f) (plist-get f :kind))
                                    (org-convect-findings
                                     entries (org-convect-test--day 2026 8 30))))
                           (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                     '(bare-rung undated-goal))))))

(ert-deftest org-convect-test-add-files-into-its-section ()
  "An entry gathers under the section carrying its `CONVECT_SECTION', so it
lands beside the guidance for it rather than at the end of the file."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "engineering")
    (let* ((file (car org-convect-files))
           (text (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (should (< (string-match "Areas of Focus" text)
                 (string-match "^\\*\\* engineering" text)))
      (should (string-match "^\\*\\* engineering" text)))))

(ert-deftest org-convect-test-add-without-a-section-still-reads ()
  "The ladder is declared, not located: with no section to gather under, the
entry goes to the end of the file and is a rung all the same."
  (org-convect-test--with-ladder "#+title: bare\n"
    (org-convect-add 'area "engineering")
    (let ((entries (org-convect-scan)))
      (should (= (length entries) 1))
      (should (equal (plist-get (car entries) :name) "engineering")))))

(ert-deftest org-convect-test-nothing-sits-above-purpose ()
  "Which is why adding a purpose never asks what it is in service of.  Being
asked to point at an empty level would make the bottom-up order feel like a
mistake, and it is the prescribed path."
  (should-not (org-convect-above 'purpose))
  (should (equal (org-convect-above 'area) '(goal vision purpose)))
  (should (equal (org-convect-above 'goal) '(vision purpose))))

(ert-deftest org-convect-test-a-new-rung-is-not-instantly-overdue ()
  "Writing one down is not the same as neglecting it.  The cadence runs from
the `CREATED' stamp until there is a review to run it from instead."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "engineering")
    (let ((area (car (org-convect-scan))))
      (should (plist-get area :created))
      (should-not (org-convect-overdue-p area))
      (should (org-convect-overdue-p
               area (time-add (current-time) (days-to-time 45)))))))

(ert-deftest org-convect-test-an-old-rung-with-no-review-is-overdue ()
  "The stamp buys a cadence, not silence."
  (org-convect-test--with-ladder "\
* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CREATED: [2026-01-05 Mon]
:END:
"
    (should (org-convect-overdue-p (car (org-convect-scan))
                                   (org-convect-test--day 2026 8 30)))))

(ert-deftest org-convect-test-an-undated-rung-is-overdue ()
  "Neither a review nor a stamp is an entry of unknown age, and due is the
honest answer for that."
  (org-convect-test--with-ladder "\
* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
"
    (should (org-convect-overdue-p (car (org-convect-scan))
                                   (org-convect-test--day 2026 8 30)))))

(ert-deftest org-convect-test-add-writes-given-properties ()
  "Whatever a layer above contributes is written alongside the ladder's own."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "engineering" nil '(("ACT_DOMAIN" . "work")))
    (let ((entry (car (org-convect-scan))))
      (should (equal (org-entry-get (plist-get entry :marker) "ACT_DOMAIN")
                     "work")))))

(ert-deftest org-convect-test-add-skips-empty-properties ()
  "An unanswered optional question leaves no drawer line behind."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "engineering" nil '(("ACT_DOMAIN" . "")))
    (let ((entry (car (org-convect-scan))))
      (should-not (org-entry-get (plist-get entry :marker) "ACT_DOMAIN")))))

(ert-deftest org-convect-test-add-from-lisp-asks-nothing ()
  "The registry is consulted by the interactive spec only.  Called from Lisp,
`org-convect-add' writes exactly what it was handed -- so a script cannot be
made to hang on a prompt by something a layer above registered."
  (let ((org-convect-add-property-functions
         (list (lambda (_horizon) (error "asked a question")))))
    (org-convect-test--with-ladder (org-convect--build-skeleton)
      (org-convect-add 'area "engineering")
      (should (= 1 (length (org-convect-scan)))))))

(defun org-convect-test--paste (section text)
  "Write TEXT as headings at the end of the SECTION section."
  (goto-char (point-min))
  (re-search-forward (format "^\\* %s" (regexp-quote section)))
  (org-end-of-subtree t t)
  (insert "\n" text))

(defmacro org-convect-test--over-the-file (&rest body)
  "Run BODY in the ladder file with the whole buffer marked."
  `(with-current-buffer (find-file-noselect (car org-convect-files))
     (goto-char (point-min))
     (push-mark (point-max) t t)
     (goto-char (point-min))
     (let ((transient-mark-mode t)) ,@body)))

(ert-deftest org-convect-test-declare-marks-a-whole-paste ()
  "The bulk half, and the one that matches the advice: areas are copied off a
document, and copying is a paste rather than thirty answers to a prompt."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (org-convect-test--paste "Areas of Focus and Accountability"
                               "** work.dev\n** work.org\n** family.father\n"))
    (org-convect-test--over-the-file (org-convect-declare))
    (should (equal (org-convect-test--names (org-convect-scan))
                   '("family.father" "work.dev" "work.org")))))

(ert-deftest org-convect-test-declare-reads-the-horizon-off-the-section ()
  "A region spanning two sections comes out right rather than uniformly wrong."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (org-convect-test--paste "Areas of Focus and Accountability" "** work.dev\n")
      (org-convect-test--paste "Goals and Objectives" "** a team that runs itself\n"))
    (org-convect-test--over-the-file (org-convect-declare))
    (let ((entries (org-convect-scan)))
      (should (eq 'area (plist-get (org-convect-test--entry entries "work.dev")
                                   :horizon)))
      (should (eq 'goal (plist-get (org-convect-test--entry
                                    entries "a team that runs itself")
                                   :horizon))))))

(ert-deftest org-convect-test-declare-leaves-sections-alone ()
  "A section is scaffolding.  Marking one would give the ladder a rung called
\"Areas of Focus and Accountability\", which is not a thing anyone is
accountable for."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-test--over-the-file (org-convect-declare))
    (should-not (org-convect-scan))))

(ert-deftest org-convect-test-declare-never-overwrites ()
  "A heading that already says what it is keeps saying it, even when it sits
under a section that says otherwise -- because the ladder is declared, not
located, and a rung filed in the wrong drawer is still that rung.  This is
also what makes a second sweep after adding one more heading safe."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (org-convect-test--paste "Areas of Focus and Accountability"
                               "** a team that runs itself\n:PROPERTIES:\n:CONVECT_HORIZON: goal\n:END:\n** work.dev\n"))
    (org-convect-test--over-the-file (org-convect-declare))
    (org-convect-test--over-the-file (org-convect-declare))
    (let ((entries (org-convect-scan)))
      (should (= 2 (length entries)))
      (should (eq 'goal (plist-get (org-convect-test--entry
                                    entries "a team that runs itself")
                                   :horizon)))
      (should (eq 'area (plist-get (org-convect-test--entry entries "work.dev")
                                   :horizon))))))

(ert-deftest org-convect-test-declare-stamps-what-it-marks ()
  "Marked rungs carry a creation stamp like written ones, so the cadence has
something to count from and they are not due the moment they exist."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (org-convect-test--paste "Areas of Focus and Accountability" "** work.dev\n"))
    (org-convect-test--over-the-file (org-convect-declare))
    (let ((entry (car (org-convect-scan))))
      (should (plist-get entry :created))
      (should-not (org-convect-overdue-p entry)))))

(ert-deftest org-convect-test-declare-takes-the-heading-at-point ()
  "With no region it marks the one you are standing on."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (org-convect-test--paste "Areas of Focus and Accountability"
                               "** work.dev\n** work.org\n")
      (goto-char (point-min))
      (re-search-forward "^\\*\\* work.dev")
      (org-convect-declare))
    (should (equal (org-convect-test--names (org-convect-scan)) '("work.dev")))))

(ert-deftest org-convect-test-refresh-leaves-the-rungs-alone ()
  "Guidance is replaceable; what you wrote is not.  The refresh may not touch
a rung, its properties or its stamp -- the file is your ladder, and the drawers
are only this package talking."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (org-convect-test--paste "Areas of Focus and Accountability"
                               "** work.dev\n** work.org\n"))
    (org-convect-test--over-the-file (org-convect-declare))
    (let ((before (org-convect-scan)))
      (org-convect-refresh-guides)
      (let ((after (org-convect-scan)))
        (should (equal (org-convect-test--names before)
                       (org-convect-test--names after)))
        (should (equal (mapcar (lambda (e) (plist-get e :horizon)) before)
                       (mapcar (lambda (e) (plist-get e :horizon)) after)))
        (should (equal (mapcar (lambda (e) (plist-get e :created)) before)
                       (mapcar (lambda (e) (plist-get e :created)) after)))))))

(ert-deftest org-convect-test-refresh-does-not-write-into-a-rung ()
  "What you wrote under a rung is the standard the review is against, and it
is the one thing in the file this package must never touch.  Guidance belongs
to sections; a rung gets nothing put inside it."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (org-convect-test--paste "Areas of Focus and Accountability" "** work.dev\n"))
    (org-convect-test--over-the-file (org-convect-declare))
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-max))
      (insert "Reviews turned around in a day.  No branch older than a week.\n")
      (save-buffer))
    (org-convect-refresh-guides)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* work.dev")
      (let ((body (buffer-substring-no-properties (point) (point-max))))
        (should (string-match-p "Reviews turned around in a day" body))
        (should-not (string-match-p ":GUIDE:" body))))))

(ert-deftest org-convect-test-refresh-replaces-rather-than-piles-up ()
  "Run twice and there is still one drawer per section."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-refresh-guides)
    (org-convect-refresh-guides)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (should (= 4 (count-matches "^:GUIDE:$"))))))

(ert-deftest org-convect-test-refresh-brings-back-a-deleted-drawer ()
  "Deleting one is supposed to be free, which means changing your mind is too."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^:GUIDE:$")
      (let ((start (match-beginning 0)))
        (re-search-forward "^:END:$")
        (delete-region start (match-end 0)))
      (goto-char (point-min))
      (should (= 3 (count-matches "^:GUIDE:$"))))
    (org-convect-refresh-guides)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (should (= 4 (count-matches "^:GUIDE:$"))))))

(ert-deftest org-convect-test-refresh-updates-the-guidance-itself ()
  "The point of it: a file written before the wording changed catches up."
  (org-convect-test--with-ladder
      (let ((org-convect-horizon-guide
             '((area :prompt "a" :hint "h" :what "Stale." :find "Stale."
                     :write "Stale." :when "Stale." :do "Stale."))))
        (org-convect--build-skeleton))
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (should (string-match-p "Stale\\." (buffer-string))))
    (org-convect-refresh-guides)
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (should-not (string-match-p "Stale\\." (buffer-string)))
      (should (string-match-p "org-convect-declare" (buffer-string))))))

;;;; A goal's date, and which way links run

(ert-deftest org-convect-test-a-goal-carries-its-date-as-a-property ()
  "Read rather than noticed: the date is machine-readable, so a goal that has
outlived it can be surfaced instead of waiting to be spotted."
  (org-convect-test--with-ladder "\
* こうたを守れる身体になっている
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2026-12-31]
:END:
Vo2max 55
"
    (let ((entry (car (org-convect-scan))))
      (should (plist-get entry :by))
      (should (equal (format-time-string "%Y-%m-%d" (plist-get entry :by))
                     "2026-12-31")))))

(ert-deftest org-convect-test-a-goal-past-its-date-is-a-finding ()
  "Reached, abandoned, or never a goal -- all three want the entry changed."
  (org-convect-test--with-ladder "\
* one
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2026-03-31]
:END:
I would know because the rota has someone else on it
"
    (should (memq 'past-its-date
                  (org-convect-test--kinds
                   (org-convect-findings (org-convect-scan)
                                         (org-convect-test--day 2026 8 30))
                   "one")))
    (should-not (memq 'past-its-date
                      (org-convect-test--kinds
                       (org-convect-findings (org-convect-scan)
                                             (org-convect-test--day 2026 1 5))
                       "one")))))

(ert-deftest org-convect-test-only-goals-take-a-date ()
  "The rungs above have no calendar and an area is never reached, so a date on
either would be a rule this package made up."
  (should (org-convect-guide 'goal :dated))
  (dolist (horizon '(area vision purpose))
    (should-not (org-convect-guide horizon :dated))))

(ert-deftest org-convect-test-linking-downward-writes-upward ()
  "Standing on the goal is where you know which areas it changes, but the link
still lives on the areas -- the data only ever points up."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "self.health")
    (org-convect-add 'goal "こうたを守れる身体になっている")
    (let* ((entries (org-convect-scan))
           (goal (org-convect-test--entry entries "こうたを守れる身体になっている"))
           (area (org-convect-test--entry entries "self.health")))
      (org-convect--add-serves (plist-get area :marker) (plist-get goal :name))
      (let* ((after (org-convect-scan))
             (area (org-convect-test--entry after "self.health"))
             (goal (org-convect-test--entry after "こうたを守れる身体になっている")))
        (should (equal (plist-get area :serves)
                       '("こうたを守れる身体になっている")))
        (should-not (plist-get goal :serves))))))

(ert-deftest org-convect-test-a-link-is-not-written-twice ()
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "self.health")
    (org-convect-add 'goal "a body that can carry him")
    (let ((area (org-convect-test--entry (org-convect-scan) "self.health")))
      (org-convect--add-serves (plist-get area :marker) "a body that can carry him")
      (org-convect--add-serves (plist-get area :marker) "a body that can carry him"))
    (should (equal (plist-get (org-convect-test--entry (org-convect-scan) "self.health")
                              :serves)
                   '("a body that can carry him")))))

(ert-deftest org-convect-test-unserved-applies-above-the-areas ()
  "A goal frames areas, a vision creates goals, a purpose drives the vision.
A rung up there shaping nothing is either not in use yet or not really held,
and that is worth asking about at every one of those levels -- not only at the
goals, which is as far as this once reached."
  (org-convect-test--with-ladder "\
* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: a team that runs itself
:END:
reviews come back the same day
* a team that runs itself
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2030-12-31]
:END:
the rota has someone else on it
* the shop runs without me in the room
:PROPERTIES:
:CONVECT_HORIZON: vision
:END:
a day goes by and nobody needed me
* Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
I do not let a number stand that I know is wrong
"
    (let ((kinds (lambda (name)
                   (org-convect-test--kinds
                    (org-convect-findings (org-convect-scan)
                                          (org-convect-test--day 2026 8 30))
                    name))))
      ;; the goal is pointed at by the area, so it is fine
      (should-not (memq 'unserved-rung (funcall kinds "a team that runs itself")))
      ;; nothing points at the vision or the principle
      (should (memq 'unserved-rung (funcall kinds "the shop runs without me in the room")))
      (should (memq 'unserved-rung (funcall kinds "Honesty")))
      ;; and an area serving nothing is still never asked about
      (should-not (memq 'unserved-rung (funcall kinds "engineering"))))))

(ert-deftest org-convect-test-an-area-is-exempt-from-being-unserved ()
  "Most areas serve nothing.  Keeping the engines running is what they are
for, and the company's chores are a real accountability."
  (org-convect-test--with-ladder "\
* admin
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
the expenses are filed by the tenth
"
    (should-not (memq 'unserved-rung
                      (org-convect-test--kinds
                       (org-convect-findings (org-convect-scan)
                                             (org-convect-test--day 2026 9 5))
                       "admin")))))

(ert-deftest org-convect-test-every-rung-says-what-it-is-not ()
  "Telling the rungs apart is the thing people get wrong, so each one carries
the question that decides it and an example of each answer."
  (dolist (horizon (mapcar #'car org-convect-horizons))
    (let ((examples (org-convect-guide horizon :examples)))
      (should (string-match-p "\\`Yes:" examples))
      (should (string-match-p "\nNo:" examples)))))

(ert-deftest org-convect-test-a-rung-in-use-is-not-called-unserved ()
  "The ladder holds commitments, and a principle's output is not a commitment
lower down -- it is conduct, which the ladder does not contain.  So something
outside it has to be able to say the rung is in use."
  (org-convect-test--with-ladder "\
* Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
I do not let a number stand that I know is wrong
* Craft
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
I do not ship what I would not review
"
    (let ((kinds (lambda (name)
                   (org-convect-test--kinds
                    (org-convect-findings (org-convect-scan)) name))))
      ;; nothing points at either, so both are reported
      (should (memq 'unserved-rung (funcall kinds "Honesty")))
      (should (memq 'unserved-rung (funcall kinds "Craft")))
      ;; until something outside the ladder vouches for one of them
      (let ((org-convect-in-use-functions
             (list (lambda (e) (equal (plist-get e :name) "Honesty")))))
        (should-not (memq 'unserved-rung (funcall kinds "Honesty")))
        (should (memq 'unserved-rung (funcall kinds "Craft")))))))

;;;; The one binding that runs downward

(ert-deftest org-convect-test-area-names-are-the-categories-on-offer ()
  "A task can only be filed against a responsibility somebody has claimed."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "dev.work")
    (org-convect-add 'area "health.self")
    (org-convect-add 'goal "a team that runs itself")
    (should (equal (sort (org-convect-area-names) #'string<)
                   '("dev.work" "health.self")))))

(ert-deftest org-convect-test-read-area-is-empty-without-any ()
  "Nothing to offer means nothing asked, and a template renders that as no
category at all -- better than a prompt with an empty list."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "asked with nothing to offer"))))
      (should (equal "" (org-convect-read-area))))))

(ert-deftest org-convect-test-set-area-writes-the-property ()
  "The binding is a CATEGORY, not a pointer: inherited, so marking a project
marks everything under it, and the task itself carries nothing extra."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "dev.work")
    (let ((task (make-temp-file "task" nil ".org" "* NEXT compare the quotes\n")))
      (unwind-protect
          (with-current-buffer (find-file-noselect task)
            (goto-char (point-min))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "dev.work")))
              (org-convect-set-area))
            (should (equal "dev.work" (org-entry-get nil "CONVECT_AREA")))
            ;; and CATEGORY is left alone -- it is Org's, and other packages
            ;; read it for their own purposes
            (should-not (string-match-p ":CATEGORY:" (buffer-string))))
        (ignore-errors (delete-file task))))))

(ert-deftest org-convect-test-set-area-clears-on-an-empty-answer ()
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "dev.work")
    (let ((task (make-temp-file "task" nil ".org"
                                "* NEXT x\n:PROPERTIES:\n:CONVECT_AREA: dev.work\n:END:\n")))
      (unwind-protect
          (with-current-buffer (find-file-noselect task)
            (goto-char (point-min))
            (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
              (org-convect-set-area))
            (should-not (org-entry-get nil "CONVECT_AREA" t)))
        (ignore-errors (delete-file task))))))

;;;; The report

(ert-deftest org-convect-test-an-undated-goal-is-a-finding ()
  "\"Roughly when\" is half of what makes an outcome a goal rather than a wish."
  (org-convect-test--with-ladder "\
* one
:PROPERTIES:
:CONVECT_HORIZON: goal
:END:
I would know because the rota has someone else on it
* two
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2030-12-31]
:END:
I would know because the books balance
"
    (let ((findings (org-convect-findings (org-convect-scan)
                                          (org-convect-test--day 2026 8 30))))
      (should (memq 'undated-goal (org-convect-test--kinds findings "one")))
      (should-not (memq 'undated-goal (org-convect-test--kinds findings "two"))))))

(ert-deftest org-convect-test-only-a-dated-horizon-can-be-undated ()
  "An area with no date is not missing one."
  (org-convect-test--with-ladder "\
* work.dev
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
reviews come back the same day
"
    (should-not (memq 'undated-goal
                      (org-convect-test--kinds
                       (org-convect-findings (org-convect-scan)) "work.dev")))))

(ert-deftest org-convect-test-doctor-counts-what-is-missing ()
  "The report exists because a heading with an empty body looks exactly like a
heading, so the file cannot show its own gaps."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "work.dev")
    (org-convect-add 'area "family.father")
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-max))
      (insert "there is time with him alone each week\n")
      (save-buffer))
    (save-window-excursion (org-convect-doctor))
    (with-current-buffer org-convect-doctor-buffer
      (let ((report (buffer-string)))
        (should (string-match-p "2 rungs   1 with nothing written" report))
        (should (string-match-p "Nothing written under it" report))
        (should (string-match-p "work.dev" report))
        (should-not (string-match-p "family.father$" report))))))

(ert-deftest org-convect-test-doctor-rows-are-live ()
  "A finding names a heading, and the heading is where you decide whether the
finding matters.  Without the marker the report is a list of names you have to
go and look up, which is how a finding comes to be read as scenery."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion (org-convect-doctor))
    (with-current-buffer org-convect-doctor-buffer
      (should (derived-mode-p 'org-agenda-mode))
      (goto-char (point-min))
      (should (re-search-forward "^Nothing below points at it$" nil t))
      (forward-line 1)
      (let ((marker (org-get-at-bol 'org-hd-marker)))
        (should (markerp marker))
        (should (marker-buffer marker))
        ;; and the rung reads back through it, which is what lets every
        ;; command that fixes a finding be run from the row
        (should (org-convect--rung-at-point))))))

(ert-deftest org-convect-test-the-doctor-follows-through-the-agenda ()
  "Following is the agenda's own, turned on with `v f' and off to begin with.

Writing another one was the wrong instinct: the agenda already has follow mode,
its keys are the ones already in the fingers, and the reason it looked broken
was elsewhere.  What it needs from a row is `org-marker' -- `org-hd-marker'
alone is not what `org-agenda-do-context-action' reads."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion
      (org-convect-doctor)
      (with-current-buffer org-convect-doctor-buffer
        (should-not org-agenda-follow-mode)
        (should (eq (key-binding (kbd "v")) 'org-agenda-view-mode-dispatch))
        (goto-char (point-min))
        (should (re-search-forward "^Nothing below points at it$" nil t))
        (forward-line 1)
        (should (markerp (org-get-at-bol 'org-marker)))
        (org-agenda-follow-mode)
        (should org-agenda-follow-mode)
        (goto-char (point-min))
        (re-search-forward "^Nothing below points at it$")
        (org-agenda-next-line)
        (should (get-buffer-window
                 (marker-buffer (org-get-at-bol 'org-hd-marker))))))))

(ert-deftest org-convect-test-the-doctor-redraws-on-r ()
  "A report you have to re-run by name after every fix is a report you stop
consulting -- and the whole point of the remedies is that you fix things while
looking at it."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion
      (org-convect-doctor)
      (with-current-buffer org-convect-doctor-buffer
        (should (eq (key-binding (kbd "r")) 'org-convect-doctor))
        ;; `g' is left to whatever the buffer already had.  It is a motion
        ;; prefix -- `gg', `gj', `gs' -- and a command on it would take the
        ;; prefix and everything under it away in this buffer alone.
        (let ((own (copy-keymap org-convect-doctor-mode-map)))
          (set-keymap-parent own nil)
          (should-not (lookup-key own (kbd "g"))))
        (goto-char (point-min))
        (re-search-forward "^Nothing below points at it$")
        (forward-line 1)
        (let ((was (org-get-at-bol 'org-convect-rung)))
          (should was)
          (call-interactively (key-binding (kbd "r")))
          (with-current-buffer org-convect-doctor-buffer
            ;; the row, not the line: a fix removes lines above the one you
            ;; were on, and a line number would land somewhere else each time
            (should (equal was (org-get-at-bol 'org-convect-rung)))))))))

(ert-deftest org-convect-test-a-redraw-shows-what-changed ()
  "Otherwise it is a picture of the file as it was when you opened it."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion
      (org-convect-doctor)
      (with-current-buffer org-convect-doctor-buffer
        (should (string-match-p "Honesty" (buffer-string))))
      (with-current-buffer (find-file-noselect (car org-convect-files))
        (goto-char (point-min))
        (re-search-forward "^\\*+ Honesty")
        (org-entry-put nil "CONVECT_HORIZON" "area")
        (save-buffer))
      (with-current-buffer org-convect-doctor-buffer
        (call-interactively (key-binding (kbd "r")))
        (with-current-buffer org-convect-doctor-buffer
          ;; an area nothing points at is never a finding
          (should-not (string-match-p "Nothing below points at it"
                                      (buffer-string))))))))

(ert-deftest org-convect-test-a-link-may-skip-as-many-rungs-as-it-likes ()
  "GTD puts no ceiling on how far up a rung may point.  The horizons are
altitudes to look from rather than a tree -- clarity higher up reveals what is
missing lower down, and nothing requires a parent one level above.

An area answerable to a principle with no goal or vision in between is one of
the commonest honest shapes there is."
  (org-convect-test--with-ladder "\
* Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
Rules out staying quiet.

* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: Honesty
:END:
Reviews come back the same day.
"
    (let ((kinds (org-convect-test--kinds
                  (org-convect-findings (org-convect-scan)) "engineering")))
      (should-not (memq 'misaimed-serves kinds))
      (should-not (memq 'unresolved-serves kinds)))
    ;; and the command offers the whole way up, not the next rung
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\* engineering")
      (let (offered)
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (_prompt table &rest _)
                     (setq offered (mapcar #'car table))
                     nil)))
          (org-convect-link))
        (should (member "Honesty  [purpose]" offered))))))

(ert-deftest org-convect-test-a-link-that-does-not-run-up-is-a-finding ()
  "The one shape the model has no reading for.  A rung above exists to shape
what is under it, so one in service of something at or below its own altitude
is a sentence with its subject and object swapped."
  (org-convect-test--with-ladder "\
* Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:CONVECT_SERVES: engineering
:END:
Rules out staying quiet.

* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
Reviews come back the same day.
"
    (let ((findings (org-convect-findings (org-convect-scan))))
      (should (memq 'misaimed-serves
                    (org-convect-test--kinds findings "Honesty")))
      ;; and it says which altitude it landed on
      (should (string-match-p
               "engineering \\[area\\]"
               (plist-get (seq-find (lambda (f)
                                      (eq (plist-get f :kind) 'misaimed-serves))
                                    findings)
                          :detail))))))

(ert-deftest org-convect-test-a-downward-link-does-not-count-as-being-served ()
  "\"Nothing below points at it\" has to mean below.  A link running the other
way is broken rather than an answer, and letting it count would silence the
gap it is sitting on top of."
  (org-convect-test--with-ladder "\
* A team that runs itself
:PROPERTIES:
:CONVECT_HORIZON: vision
:END:
Releases go out without me.

* Two reviewers, unprompted
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2030-06-30]
:END:
Reached when two in a row go out unasked.
"
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\* A team that runs itself")
      (org-entry-put nil "CONVECT_SERVES" "Two reviewers, unprompted")
      (save-buffer))
    (let ((kinds (org-convect-test--kinds
                  (org-convect-findings (org-convect-scan))
                  "Two reviewers, unprompted")))
      (should (memq 'unserved-rung kinds)))))

(ert-deftest org-convect-test-a-candidate-carries-its-altitude ()
  "Two rungs can be worded alike, and which one you meant is the difference
between a principle and the goal that serves it."
  (should (equal "Honesty  [purpose]"
                 (org-convect--candidate '(:name "Honesty" :horizon purpose))))
  ;; and what comes back is the name, not the label it was shown under
  (let ((table '(("Honesty  [purpose]" . "Honesty"))))
    (should (equal "Honesty" (org-convect--chosen "Honesty  [purpose]" table)))
    ;; typed freely, it comes back unchanged: a link may be written before the
    ;; rung it points at exists
    (should (equal "Not yet written" (org-convect--chosen "Not yet written" table)))))

(ert-deftest org-convect-test-relink-offers-altitudes-too ()
  "The same ambiguity, in the command that repairs the link rather than the one
that writes it."
  (org-convect-test--with-ladder "\
* Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
Rules out staying quiet.

* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: Honestly
:END:
Reviews come back the same day.
"
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\* engineering")
      (let (offered)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt table &rest _)
                     (if (string-match-p "Repoint which" prompt)
                         "Honestly  (not in the file)"
                       (setq offered (mapcar #'car table))
                       "Honesty  [purpose]"))))
          (org-convect-relink))
        (should (member "Honesty  [purpose]" offered))
        ;; and the property holds the name, not the label
        (should (equal "Honesty" (org-entry-get nil "CONVECT_SERVES")))))))

(ert-deftest org-convect-test-linking-down-ignores-a-name-with-no-rung ()
  "The downward half writes onto another rung, and a name typed freely has no
rung to write onto.  It used to look one up by name and hand on the marker of
whatever came back, which for nothing is nil -- and `org-with-point-at' takes
nil as \"here\", so the property went silently into whatever entry point
happened to be on.  Nothing is the only correct thing to write."
  (org-convect-test--with-ladder "\
* Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
Rules out staying quiet.

* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
Reviews come back the same day.
"
    (let ((before (with-temp-buffer
                    (insert-file-contents (car org-convect-files))
                    (buffer-string))))
      (with-current-buffer (find-file-noselect (car org-convect-files))
        (goto-char (point-min))
        (re-search-forward "^\\* Honesty")
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) (list "no such rung"))))
          (org-convect-link t)))
      ;; the whole file, not only the rung that was offered: the failure this
      ;; guards writes somewhere else entirely
      (should (equal before (with-temp-buffer
                              (insert-file-contents (car org-convect-files))
                              (buffer-string)))))))

(ert-deftest org-convect-test-relink-repoints-a-broken-link ()
  "Neither of the other two can do this.  `org-convect-link' adds a name beside
the broken one and `org-convect-reword' renames a rung rather than the link
pointing at it, so without this the one finding that is always a defect had no
command behind it."
  (org-convect-test--with-ladder "\
* Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
Rules out staying quiet.

* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: Honestly
:END:
Reviews come back the same day.
"
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\* engineering")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-match-p "Repoint which" prompt)
                       "Honestly  (not in the file)"
                     "Honesty"))))
        (org-convect-relink))
      (should (equal "Honesty" (org-entry-get nil "CONVECT_SERVES"))))
    (should-not (memq 'unresolved-serves
                      (org-convect-test--kinds
                       (org-convect-findings (org-convect-scan)) "engineering")))))

(ert-deftest org-convect-test-relink-drops-a-link-on-an-empty-answer ()
  "When the thing it named is gone, dropping it is the honest repair -- and
leaving the property behind with nothing in it would be a second finding."
  (org-convect-test--with-ladder "\
* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: A goal nobody kept
:END:
Reviews come back the same day.
"
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\* engineering")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-match-p "Repoint which" prompt)
                       "A goal nobody kept  (not in the file)"
                     ""))))
        (org-convect-relink))
      (should-not (org-entry-get nil "CONVECT_SERVES")))))

(ert-deftest org-convect-test-relink-refuses-a-rung-with-no-links ()
  "There is nothing to repoint, and a prompt offering an empty list teaches
people to dismiss prompts."
  (org-convect-test--with-ladder org-convect-test--ready
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\*+ Honesty")
      (should-error (org-convect-relink) :type 'user-error))))

(ert-deftest org-convect-test-every-finding-says-what-answers-it ()
  "Naming a condition without naming what resolves it leaves the reader to go
and find the command, and a report you have to research is a report you stop
reading."
  (dolist (entry org-convect--finding-labels)
    (should (org-string-nw-p (org-convect--finding-label (car entry))))
    (should (org-string-nw-p (org-convect--finding-remedy (car entry)))))
  ;; the ones whose answer is a command name it
  (should (string-match-p "org-convect-link"
                          (org-convect--finding-remedy 'unserved-rung)))
  ;; not reword, which renames a rung, and not link, which only ever adds
  (should (string-match-p "org-convect-relink"
                          (org-convect--finding-remedy 'unresolved-serves)))
  (should (string-match-p "org-convect-reword"
                          (org-convect--finding-remedy 'duplicate-name)))
  (should (string-match-p "org-convect-set-date"
                          (org-convect--finding-remedy 'undated-goal))))

(ert-deftest org-convect-test-the-remedy-is-shown-with-the-finding ()
  "In the report, not only in the source."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion (org-convect-doctor))
    (with-current-buffer org-convect-doctor-buffer
      (should (string-match-p "C-u M-x org-convect-link" (buffer-string))))))

(ert-deftest org-convect-test-the-two-link-findings-are-worded-as-a-pair ()
  "They are read together and they are about the same one property, so they are
worded alike -- same verb, and the direction said out loud.

They are not opposites, though, and the wording must not claim they are.  One
is an absence, nothing below points here; the other is a break, something does
point, at a name that is not in the file.  The true opposite of the first is
deliberately not a finding: most areas serve nothing."
  (let ((down (org-convect--finding-label 'unserved-rung))
        (up (org-convect--finding-label 'unresolved-serves)))
    (should (string-match-p "points" down))
    (should (string-match-p "points" up))
    (should (string-match-p "below" down))
    (should (string-match-p "above" up))))

(ert-deftest org-convect-test-a-date-set-from-a-board-lands-on-the-rung ()
  "`org-convect-set-date' used to write at point, which is the board when the
rung was read from a row -- so the property went into the report and the goal
kept its silence."
  (org-convect-test--with-ladder org-convect-test--ladder-written
    (save-window-excursion (org-convect-doctor))
    (with-current-buffer org-convect-doctor-buffer
      (goto-char (point-min))
      (should (re-search-forward "goal +A team that runs itself" nil t))
      (beginning-of-line)
      (org-convect-set-date "2030-12-31")
      (should-not (string-match-p "CONVECT_BY" (buffer-string))))
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (should (re-search-forward "^\\*+ A team that runs itself" nil t))
      (should (equal "[2030-12-31]" (org-entry-get nil "CONVECT_BY"))))))

(defconst org-convect-test--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Where this file lives, taken while it loads.

`load-file-name' is bound during the load and nil by the time a test runs, so
a test that wants a file next to this one has to have been told at load time.")

(ert-deftest org-convect-test-the-readme-lists-every-finding ()
  "The table drifted once already -- it carried a kind called `unserved-goal'
that the code has never reported -- and a table of findings that is missing one
is worse than no table, because it reads as complete."
  (let ((readme (expand-file-name "../README.org" org-convect-test--directory)))
    (skip-unless (file-readable-p readme))
    (let ((text (with-temp-buffer (insert-file-contents readme) (buffer-string))))
      (dolist (entry org-convect--finding-labels)
        (should (string-match-p (format "~%s~" (car entry)) text))))))

(ert-deftest org-convect-test-doctor-counts-highest-first ()
  "The report reads down the way the file does.  Counting up would put the
areas first, which is the order they get *written* in and not the order
anything is read in."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion (org-convect-doctor))
    (with-current-buffer org-convect-doctor-buffer
      (let ((text (buffer-string)))
        (should (< (string-match "Purpose and Principles" text)
                   (string-match "Goals and Objectives" text)))
        (should (< (string-match "Goals and Objectives" text)
                   (string-match "Areas of Focus" text)))))))

(ert-deftest org-convect-test-doctor-writes-nothing ()
  "It is a report.  Running it must not change the ladder it is reporting on."
  (org-convect-test--with-ladder (org-convect--build-skeleton)
    (org-convect-add 'area "work.dev")
    (let ((before (with-temp-buffer
                    (insert-file-contents (car org-convect-files))
                    (buffer-string))))
      (save-window-excursion (org-convect-doctor))
      (should (equal before (with-temp-buffer
                              (insert-file-contents (car org-convect-files))
                              (buffer-string)))))))

(ert-deftest org-convect-test-doctor-is-clean-on-a-finished-ladder ()
  (org-convect-test--with-ladder org-convect-test--ladder-written
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* A team that runs itself")
      (org-entry-put nil "CONVECT_BY" "[2030-12-31]")
      (save-buffer))
    (save-window-excursion (org-convect-doctor))
    (with-current-buffer org-convect-doctor-buffer
      (should-not (string-match-p "Nothing written under it" (buffer-string)))
      (should-not (string-match-p "No date" (buffer-string))))))

(ert-deftest org-convect-test-a-long-name-is-reported-with-its-width ()
  "Whether a long heading is a name or a stray sentence cannot be decided from
the text.  The finding reports the width and leaves the reading to a person."
  (let ((org-convect-heading-width 20) (org-convect-area-width nil))
    (org-convect-test--with-ladder "\
* short
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2030-01-01]
:END:
I would know because the books balance
* a heading long enough to have been a sentence all along
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2030-01-01]
:END:
I would know because it says so
"
      (let ((findings (org-convect-findings (org-convect-scan)
                                            (org-convect-test--day 2026 8 30))))
        (should-not (memq 'long-heading
                          (org-convect-test--kinds findings "short")))
        (should (memq 'long-heading
                      (org-convect-test--kinds
                       findings
                       "a heading long enough to have been a sentence all along")))))))

(ert-deftest org-convect-test-width-is-columns-not-characters ()
  "A name in a wide script takes two columns per character, so counting
characters would let a heading twice the intended width through unremarked --
which is the only kind of heading this check exists for on this ladder."
  (let ((org-convect-heading-width 40))
    (org-convect-test--with-ladder "\
* 働き方の物差しが、居た時間からログと集中に変わっている
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2030-01-01]
:END:
週次で自分のログを見返している
"
      (let ((name "働き方の物差しが、居た時間からログと集中に変わっている"))
        ;; 26 characters, 52 columns: under the limit by one count, over by the
        ;; other, and the agenda draws columns
        (should (< (length name) 40))
        (should (> (string-width name) 40))
        (should (memq 'long-heading
                      (org-convect-test--kinds
                       (org-convect-findings (org-convect-scan)
                                             (org-convect-test--day 2026 8 30))
                       name)))))))

(ert-deftest org-convect-test-an-area-is-measured-against-the-agenda ()
  "An area's name is the CATEGORY the agenda prints in a fixed column, so it
is held to that width rather than to the one the rungs above it get."
  (let ((org-convect-area-width 8)
        (org-convect-heading-width 40))
    (org-convect-test--with-ladder "\
* family.father
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
there is time with him alone each week
"
      (should (memq 'long-heading
                    (org-convect-test--kinds
                     (org-convect-findings (org-convect-scan)
                                           (org-convect-test--day 2026 8 30))
                     "family.father"))))))

(ert-deftest org-convect-test-a-long-bare-heading-says-so ()
  "The strong case: content takes the shape of a long heading over an empty
body when there was nowhere else to put it."
  (let ((org-convect-heading-width 10))
    (org-convect-test--with-ladder "\
* a principle stated at length
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
* another stated at length
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
what it rules out
"
      (let ((findings (org-convect-findings (org-convect-scan))))
        (should (string-match-p
                 "nothing written underneath"
                 (plist-get (seq-find (lambda (f)
                                        (and (eq (plist-get f :kind) 'long-heading)
                                             (equal (plist-get f :name)
                                                    "a principle stated at length")))
                                      findings)
                            :detail)))
        (should-not (string-match-p
                     "nothing written underneath"
                     (plist-get (seq-find (lambda (f)
                                            (and (eq (plist-get f :kind) 'long-heading)
                                                 (equal (plist-get f :name)
                                                        "another stated at length")))
                                          findings)
                                :detail)))))))

;;;; Planning a project

(defmacro org-convect-test--in-org (text &rest body)
  "Run BODY in an Org buffer holding TEXT, point at its start."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (let ((org-todo-keywords '((sequence "NEXT" "ONGO" "|" "DONE" "CANCEL"))))
       (insert ,text)
       (goto-char (point-min))
       ,@body)))

(ert-deftest org-convect-test-plan-lays-down-only-what-persists ()
  "Five steps and two of them leave anything behind.  A form with five blanks
would be asking for three things that have nowhere to sit."
  (org-convect-test--in-org "* NEXT move the service\n"
    (re-search-forward "move")
    (org-convect-plan)
    (let ((text (buffer-string)))
      (should (string-match-p "^- Purpose ::" text))
      (should (string-match-p "^- Outcome ::" text))
      (should-not (string-match-p "Brainstorm" text))
      (should-not (string-match-p "Organi" text)))))

(ert-deftest org-convect-test-plan-opens-a-space-with-no-keyword ()
  "The child carries no TODO keyword, and that is the point: org-foresight
gives a heading without one no record at all, so a brainstorm can be as long
and as wrong as it needs to be without a thought of it counting as work."
  (org-convect-test--in-org "* NEXT move the service\n"
    (re-search-forward "move")
    (org-convect-plan)
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* *$" nil t))
    (should-not (string-match-p "\\*\\* NEXT" (buffer-string)))))

(ert-deftest org-convect-test-plan-takes-its-depth-from-the-project ()
  (org-convect-test--in-org "* work\n** NEXT move the service\n"
    (re-search-forward "move")
    (org-convect-plan)
    (should (string-match-p "^\\*\\*\\* *$" (buffer-string)))))

(ert-deftest org-convect-test-plan-does-not-repeat-itself ()
  (org-convect-test--in-org "\
* NEXT move the service
- Purpose :: the old one is falling over
- Outcome :: everything runs on the new one
"
    (re-search-forward "move")
    (org-convect-plan)
    (goto-char (point-min))
    (should (= 1 (count-matches "^- Purpose ::")))
    (should (= 1 (count-matches "^- Outcome ::")))))

(ert-deftest org-convect-test-plan-leaves-an-existing-brainstorm-alone ()
  "Called again on a project already being thought about, it must not add an
empty heading to the pile."
  (org-convect-test--in-org "\
* NEXT move the service
- Purpose :: the old one is falling over
- Outcome :: everything runs on the new one
** take stock of what depends on it
"
    (re-search-forward "move the service")
    (org-convect-plan)
    (should (= 1 (with-current-buffer (current-buffer)
                   (goto-char (point-min))
                   (count-matches "^\\*\\* "))))))

(ert-deftest org-convect-test-plan-stays-in-its-own-entry ()
  "Inserting the fields leaves point at the start of the line after them,
which on an entry with no body is the *next heading*.  Going back to a heading
from there arrives at the wrong one, and the space to think opens under
somebody else's work."
  (org-convect-test--in-org "\
* NEXT move the service
:PROPERTIES:
:Effort: 0:10
:END:
* NEXT something else
"
    (re-search-forward "move the service")
    (org-convect-plan)
    (goto-char (point-min))
    (re-search-forward "^\\* NEXT move the service")
    (let ((mine (buffer-substring-no-properties
                 (point) (save-excursion (org-end-of-subtree t t) (point)))))
      (should (string-match-p "^- Purpose ::" mine))
      (should (string-match-p "^\\*\\* *$" mine)))
    ;; and the entry after it is untouched
    (goto-char (point-min))
    (should (re-search-forward "^\\* NEXT something else$" nil t))
    (goto-char (point-max))
    (should-not (string-match-p "\\*\\* *\\'" (buffer-substring-no-properties
                                               (line-beginning-position) (point))))))

(ert-deftest org-convect-test-plan-does-not-split-the-drawers ()
  "Text written after the property drawer but before the LOGBOOK breaks a run
Org expects to be unbroken -- and it is the easy mistake, because
`org-end-of-meta-data' lands before the drawers and PROPERTIES is the one
everybody remembers."
  (org-convect-test--in-org "\
* NEXT move the service
:PROPERTIES:
:Effort: 0:10
:END:
:LOGBOOK:
CLOCK: [2026-06-16 Tue 20:33]--[2026-06-16 Tue 20:35] =>  0:02
:END:
"
    (re-search-forward "move the service")
    (org-convect-plan)
    (let ((text (buffer-string)))
      (should (string-match-p ":END:\n:LOGBOOK:" text))
      (should (< (string-match ":LOGBOOK:" text)
                 (string-match "- Purpose ::" text))))))

(ert-deftest org-convect-test-plan-does-not-eat-the-next-heading ()
  "`org-end-of-subtree' with TO-HEADING lands on the *start* of whatever
follows, so a heading inserted there without a line of its own runs into that
one and swallows it."
  (org-convect-test--in-org "\
* NEXT move the service
- Purpose :: the old one is falling over
- Outcome :: everything runs on the new one
* NEXT something else
"
    (re-search-forward "move the service")
    (org-convect-plan)
    (goto-char (point-min))
    (should (re-search-forward "^\\* NEXT something else$" nil t))))

;;;; What the thing under the cursor is asking for

(defun org-convect-test--eldoc ()
  "What Eldoc would say about the line at point, or nil.

Read from the return value, and the callback is handed one that fails, because
answering through both is how the same line gets printed twice."
  (org-convect-eldoc-function
   (lambda (&rest _) (error "answered twice: callback as well as return"))))

(ert-deftest org-convect-test-eldoc-answers-where-it-can ()
  "Guidance in the file goes stale and guidance in a command is only there
when you thought to run it.  This is there while you are filling the thing in."
  (org-convect-test--in-org "\
* NEXT move the service
- Purpose ::
- Outcome ::
* work.dev
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
"
    (re-search-forward "- Purpose")
    (should (string-match-p "\\`Purpose" (org-convect-test--eldoc)))
    (goto-char (point-min)) (re-search-forward "- Outcome")
    (should (string-match-p "\\`Outcome" (org-convect-test--eldoc)))
    ;; and on a rung, what goes underneath it
    (goto-char (point-min)) (re-search-forward "^\\* work.dev")
    (should (string-match-p "Areas of Focus" (org-convect-test--eldoc)))))

(ert-deftest org-convect-test-eldoc-is-silent-everywhere-else ()
  "It runs in every Org buffer, so anywhere it does not recognise it must
return nothing at all rather than something vague."
  (org-convect-test--in-org "\
* an ordinary heading
  an ordinary line
- a list item that is not a field
"
    (dolist (probe '("^\\* an ordinary" "an ordinary line" "a list item"))
      (goto-char (point-min))
      (re-search-forward probe)
      (should-not (org-convect-test--eldoc)))))

(ert-deftest org-convect-test-eldoc-can-be-turned-off ()
  (org-convect-test--in-org "* NEXT x\n- Purpose ::\n"
    (re-search-forward "- Purpose")
    (let ((org-convect-eldoc nil))
      (should-not (org-convect-test--eldoc)))))

;;;; The review

(defconst org-convect-test--ready "\
* Areas of Focus and Accountability
:PROPERTIES:
:CONVECT_SECTION: area
:END:
** engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: a team that runs itself
:CREATED: [2026-01-05 Mon]
:END:
reviews come back the same day
** admin
:PROPERTIES:
:CONVECT_HORIZON: area
:CREATED: [2026-08-30 Sun]
:END:
the expenses are filed by the tenth
- Note taken on [2026-08-30 Sun 10:00] \\\\
  still mine
* Goals and Objectives
** a team that runs itself
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2030-12-31]
:CREATED: [2026-08-30 Sun]
:END:
the rota has someone else on it
* Purpose and Principles
** Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:CREATED: [2020-01-01 Wed]
:END:
I do not let a number stand that I know is wrong
"
  "A ladder with one rung long overdue and the rest recently seen.")

(ert-deftest org-convect-test-only-the-overdue-are-due ()
  "The board is a list of what wants looking at, so a rung looked at last week
has no business on it."
  (org-convect-test--with-ladder org-convect-test--ready
    (let* ((entries (org-convect-scan))
           (due (org-convect-due entries (org-convect-test--day 2026 9 5))))
      (should (equal (org-convect-test--names due) '("engineering"))))))

(ert-deftest org-convect-test-the-upper-rungs-never-come-due ()
  "Honesty has been there since 2020 and is not overdue, because GTD puts no
calendar on it.  If it ever appears on the board it will be because something
noticed a condition, which is a different section."
  (org-convect-test--with-ladder org-convect-test--ready
    (let ((due (org-convect-due (org-convect-scan)
                                (org-convect-test--day 2030 1 1))))
      (should-not (member "Honesty" (org-convect-test--names due))))))

(ert-deftest org-convect-test-a-signal-can-call-a-rung-with-no-calendar ()
  "The only way the top of the ladder is ever reached."
  (org-convect-test--with-ladder org-convect-test--ready
    (let* ((entries (org-convect-scan))
           (org-convect-signal-functions
            (list (lambda (e) (and (equal (plist-get e :name) "Honesty")
                                   "3 away in a row")))))
      (should (equal (mapcar (lambda (c) (plist-get (car c) :name))
                             (org-convect-called entries))
                     '("Honesty")))
      (should (equal (cdr (car (org-convect-called entries))) "3 away in a row")))))

(ert-deftest org-convect-test-nothing-is-called-without-a-signal ()
  (org-convect-test--with-ladder org-convect-test--ready
    (let ((org-convect-signal-functions nil))
      (should-not (org-convect-called (org-convect-scan))))))

(ert-deftest org-convect-test-the-neighbourhood-runs-both-ways ()
  "What you are looking at while reviewing one rung: what it was raised for,
and what answers to it."
  (org-convect-test--with-ladder org-convect-test--ready
    (let* ((entries (org-convect-scan))
           (area (org-convect-test--entry entries "engineering"))
           (goal (org-convect-test--entry entries "a team that runs itself")))
      (should (equal (org-convect-test--names
                      (car (org-convect-neighbourhood entries area)))
                     '("a team that runs itself")))
      (should-not (cdr (org-convect-neighbourhood entries area)))
      (should (equal (org-convect-test--names
                      (cdr (org-convect-neighbourhood entries goal)))
                     '("engineering")))
      (should-not (car (org-convect-neighbourhood entries goal))))))

(ert-deftest org-convect-test-board-rows-are-live ()
  "The marker is what makes a row a row rather than a picture of one.  Without
it the board looks identical and every agenda key on it does nothing, which is
the failure worth guarding against."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion
      (org-convect-review nil (org-convect-test--day 2026 9 5)))
    (with-current-buffer org-convect-review-buffer
      (goto-char (point-min))
      (should (re-search-forward "engineering" nil t))
      (should (markerp (get-text-property (match-beginning 0) 'org-hd-marker)))
      (should (derived-mode-p 'org-agenda-mode)))))

(ert-deftest org-convect-test-board-shows-the-whole-ladder ()
  "Reviewing is not something a date gives permission for, and the
relationships are worth seeing on any day -- which is otherwise only possible
by reading the file and rebuilding them in your head.  So the board shows
everything and marks what wants attention."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion
      (org-convect-review nil (org-convect-test--day 2026 9 5)))
    (with-current-buffer org-convect-review-buffer
      (let ((text (buffer-string)))
        ;; the overdue one and the recently-seen one are both here
        (should (string-match-p "engineering" text))
        (should (string-match-p "admin" text))
        ;; and so is everything with no calendar at all
        (should (string-match-p "Honesty" text))
        ;; highest first, the way the file reads
        (should (< (string-match "Purpose and Principles" text)
                   (string-match "Areas of Focus" text)))))))

(ert-deftest org-convect-test-board-does-not-cut-a-name ()
  "A principle read in half is a different principle, so the name column is as
wide as the widest name there is rather than a number picked in advance."
  (org-convect-test--with-ladder "\
* a principle stated at a length nobody would have guessed in advance
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
I do not let a number stand that I know is wrong
"
    (save-window-excursion
      (org-convect-review nil (org-convect-test--day 2026 9 5)))
    (with-current-buffer org-convect-review-buffer
      (should (string-match-p
               "a principle stated at a length nobody would have guessed in advance"
               (buffer-string))))))

(ert-deftest org-convect-test-board-marks-rather-than-filters ()
  "What is due is a word on the row.  The two ways a rung asks for attention
stay apart in the marking: a cadence gives a date, and the rungs above have
none, so they say so until something notices a condition."
  (org-convect-test--with-ladder org-convect-test--ready
    (let ((org-convect-signal-functions
           (list (lambda (e) (and (equal (plist-get e :name) "Honesty")
                                  "3 away in a row")))))
      (save-window-excursion
        (org-convect-review nil (org-convect-test--day 2026 9 5)))
      (with-current-buffer org-convect-review-buffer
        (goto-char (point-min))
        (should (re-search-forward "^  engineering +due$" nil t))
        (goto-char (point-min))
        (should (re-search-forward "^  admin +due 2026-09-29$" nil t))
        (goto-char (point-min))
        (should (re-search-forward "^  Honesty +called -- 3 away in a row$" nil t))
        (goto-char (point-min))
        (should (re-search-forward "1 due, 1 called" nil t))))))

(ert-deftest org-convect-test-board-can-be-narrowed-to-what-wants-attention ()
  "The shape of a monthly sitting rather than a look."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion
      (org-convect-review t (org-convect-test--day 2026 9 5)))
    (with-current-buffer org-convect-review-buffer
      (let ((text (buffer-string)))
        (should (string-match-p "engineering" text))
        (should-not (string-match-p "admin" text))
        (should-not (string-match-p "Honesty" text))))))

(ert-deftest org-convect-test-board-says-when-nothing-is-watching ()
  "With no signal registered the upper rungs are never called for at all, and
saying \"nothing\" would read as \"they are fine\"."
  (org-convect-test--with-ladder org-convect-test--ready
    (let ((org-convect-signal-functions nil))
      (save-window-excursion
        (org-convect-review nil (org-convect-test--day 2026 9 5)))
      (with-current-buffer org-convect-review-buffer
        (should (string-match-p "nothing is watching" (buffer-string)))))))

(ert-deftest org-convect-test-board-shows-evidence-a-layer-above-supplies ()
  "The ladder shows what was declared; a layer above adds what happened."
  (org-convect-test--with-ladder org-convect-test--ready
    (let ((org-convect-review-evidence-functions
           (list (lambda (_e) (list "12 choice points, 5 away")))))
      (save-window-excursion
        (org-convect-review nil (org-convect-test--day 2026 9 5)))
      (with-current-buffer org-convect-review-buffer
        (should (string-match-p "12 choice points, 5 away" (buffer-string)))))))

;;;; The thread under the cursor

(defun org-convect-test--lit (target)
  "Names of the rows lit when the cursor is on TARGET's row."
  (goto-char (point-min))
  (re-search-forward (concat "^  " (regexp-quote target)))
  (beginning-of-line)
  (org-convect--light-lineage)
  (sort (mapcar (lambda (o)
                  (get-text-property (overlay-start o) 'org-convect-rung))
                org-convect--lit)
        #'string<))

(ert-deftest org-convect-test-the-highlight-names-no-colour ()
  "A package has no business knowing what colour anybody's theme is.  The face
says what the rows *mean* -- marked, but not the thing you are acting on --
and inherits from the standard face that already means that, which a theme has
usually already thought about."
  (should (eq 'secondary-selection
              (face-attribute 'org-convect-lineage-face :inherit nil)))
  (should (eq 'unspecified
              (face-attribute 'org-convect-lineage-face :background nil))))

(ert-deftest org-convect-test-the-cursor-lights-its-thread ()
  "The mesh is the part of the ladder a file cannot show, and this is the
cheapest way to see it: what moves together when you move."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion (org-convect-review nil (org-convect-test--day 2026 9 5)))
    (with-current-buffer org-convect-review-buffer
      ;; the area lights the goal it serves as well as itself
      (should (equal (org-convect-test--lit "engineering")
                     '("a team that runs itself" "engineering")))
      ;; and standing on the goal lights the same thread from the other end
      (should (equal (org-convect-test--lit "a team that runs itself")
                     '("a team that runs itself" "engineering"))))))

(ert-deftest org-convect-test-an-unlinked-rung-lights-alone ()
  "Which is worth seeing: it says the rung connects to nothing."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion (org-convect-review nil (org-convect-test--day 2026 9 5)))
    (with-current-buffer org-convect-review-buffer
      (should (equal (org-convect-test--lit "admin") '("admin")))
      (should (equal (org-convect-test--lit "Honesty") '("Honesty"))))))

(ert-deftest org-convect-test-the-highlight-can-be-turned-off ()
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion (org-convect-review nil (org-convect-test--day 2026 9 5)))
    (with-current-buffer org-convect-review-buffer
      (let ((org-convect-highlight-lineage nil))
        (should-not (org-convect-test--lit "engineering"))))))

(ert-deftest org-convect-test-a-line-that-is-no-rung-lights-nothing ()
  "Section headings and the legend at the foot are not rungs, and leaving the
last row should put its thread out rather than leave it behind."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion (org-convect-review nil (org-convect-test--day 2026 9 5)))
    (with-current-buffer org-convect-review-buffer
      (org-convect-test--lit "engineering")
      (should org-convect--lit)
      (goto-char (point-min))
      (org-convect--light-lineage)
      (should-not org-convect--lit))))

(ert-deftest org-convect-test-an-unchanged-row-costs-nothing ()
  "It runs on every keystroke, so moving within a row must not rebuild the
overlays -- which is also what stops them flickering."
  (org-convect-test--with-ladder org-convect-test--ready
    (save-window-excursion (org-convect-review nil (org-convect-test--day 2026 9 5)))
    (with-current-buffer org-convect-review-buffer
      (org-convect-test--lit "engineering")
      (let ((before org-convect--lit))
        (forward-char 3)
        (org-convect--light-lineage)
        (should (eq before org-convect--lit))))))

;;;; One thread of the ladder

(ert-deftest org-convect-test-a-thread-runs-both-ways ()
  "The view the file cannot give: links point up only, so reading the file
tells you what a rung serves and never what serves it."
  (org-convect-test--with-ladder org-convect-test--ready
    (let* ((entries (org-convect-scan))
           (goal (org-convect-test--entry entries "a team that runs itself"))
           (thread (org-convect-lineage entries goal)))
      (should (equal (org-convect-test--names thread)
                     '("a team that runs itself" "engineering"))))))

(ert-deftest org-convect-test-a-thread-is-walked-by-name ()
  "A rung handed in from somewhere else is the same rung.  Walking by object
puts the one you started from into the thread twice, which is what happens
when the entry was built fresh at point rather than taken from the scan."
  (org-convect-test--with-ladder org-convect-test--ready
    (let* ((entries (org-convect-scan))
           ;; a copy, as `org-convect--rung-at-point' would produce
           (fresh (copy-sequence
                   (org-convect-test--entry entries "engineering")))
           (thread (org-convect-lineage entries fresh)))
      (should (= 2 (length thread)))
      (should (equal (org-convect-test--names thread)
                     '("a team that runs itself" "engineering"))))))

(ert-deftest org-convect-test-a-thread-that-stops-says-so ()
  "What a piece of work is finally for, and where the answer runs out.  A
purpose is allowed to serve nothing; anything lower that does is a thread that
stops."
  (org-convect-test--with-ladder org-convect-test--ready
    (let* ((entries (org-convect-scan))
           (goal (org-convect-test--entry entries "a team that runs itself"))
           (area (org-convect-test--entry entries "engineering"))
           (honesty (org-convect-test--entry entries "Honesty")))
      (should (org-convect-thread-ends-at-p goal))
      (should-not (org-convect-thread-ends-at-p area))
      (should-not (org-convect-thread-ends-at-p honesty)))))

(ert-deftest org-convect-test-narrowing-shows-more-not-less ()
  "Focusing on a handful of rungs and then saying less about each of them
would be narrowing for nothing.  The board carries every rung and can afford a
glimpse; the thread has the room for the whole standard."
  (org-convect-test--with-ladder "\
* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: a team that runs itself
:CREATED: [2026-01-05 Mon]
:END:
reviews come back the same day
no branch is older than a week
the build is green when I leave
* a team that runs itself
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2030-12-31]
:END:
the rota has someone else on it
"
    (let* ((entries (org-convect-scan))
           (area (org-convect-test--entry entries "engineering")))
      (save-window-excursion
        (org-convect-review nil (org-convect-test--day 2026 9 5)))
      (with-current-buffer org-convect-review-buffer
        ;; the board shows the first line only
        (should (string-match-p "reviews come back the same day" (buffer-string)))
        (should-not (string-match-p "the build is green" (buffer-string))))
      (cl-letf (((symbol-function 'org-convect--pick) (lambda (&rest _) area)))
        (save-window-excursion (org-convect-lineage-show)))
      (with-current-buffer "*Horizons Thread*"
        (let ((text (buffer-string)))
          ;; the thread shows all of it, and the state the board showed
          (should (string-match-p "reviews come back the same day" text))
          (should (string-match-p "no branch is older than a week" text))
          (should (string-match-p "the build is green" text))
          (should (string-match-p "serves a team that runs itself" text))
          (should (string-match-p "due" text)))))))

(ert-deftest org-convect-test-history-says-what-it-says-now ()
  "A history read without the present tense of the thing is a list of edits."
  (org-convect-test--with-ladder org-convect-test--ready
    (let ((entry (org-convect-test--entry (org-convect-scan) "Honesty")))
      (cl-letf (((symbol-function 'org-convect--pick) (lambda (&rest _) entry)))
        (save-window-excursion (org-convect-history-show)))
      (with-current-buffer "*Horizons History*"
        (should (string-match-p "I do not let a number stand" (buffer-string)))))))

;;;; What has happened to a rung

(ert-deftest org-convect-test-history-is-one-stream ()
  "Three things you might look back over turn out to be one thing recorded
three ways in the same place, so they are read as one stream with the kind as
a column."
  (org-convect-test--with-ladder org-convect-test--ready
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Honesty")
      (org-convect-reword "Saying the number" "too broad to act on"))
    (let* ((entry (org-convect-test--entry (org-convect-scan) "Saying the number"))
           (org-convect-history-functions
            (list (lambda (_e) (list (list (org-convect-test--day 2020 6 1)
                                           'choice "away · kept quiet")))))
           (history (org-convect-history entry)))
      (should (equal (mapcar #'cadr history) '(reworded choice)))
      (should (string-match-p "Reworded from \"Honesty\"" (nth 2 (car history)))))))

(ert-deftest org-convect-test-history-tells-a-reword-from-a-conclusion ()
  (org-convect-test--with-ladder org-convect-test--ready
    (let ((entry (org-convect-test--entry (org-convect-scan) "admin")))
      ;; the fixture's admin carries an ordinary note
      (should (equal (mapcar #'cadr (org-convect-history entry)) '(reviewed))))))

(ert-deftest org-convect-test-history-is-newest-first ()
  (org-convect-test--with-ladder "\
* Honesty
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
I do not let a number stand that I know is wrong
- Note taken on [2026-03-01 Sun 09:00] \\\\
  the older one
- Note taken on [2026-07-01 Wed 09:00] \\\\
  the newer one
"
    (let ((history (org-convect-history
                    (car (org-convect-scan)))))
      (should (string-match-p "newer" (nth 2 (car history))))
      (should (string-match-p "older" (nth 2 (cadr history)))))))

;;;; Rewording a rung

(ert-deftest org-convect-test-reword-follows-the-references ()
  "Rungs are pointed at by name, so a reword breaks every link that named the
old one -- silently, because a link resolving to nothing shows up only in a
report nobody has run yet."
  (org-convect-test--with-ladder org-convect-test--ready
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* a team that runs itself")
      (org-convect-reword "a team that carries itself" "carrying is the word"))
    (let* ((entries (org-convect-scan))
           (area (org-convect-test--entry entries "engineering")))
      (should (org-convect-test--entry entries "a team that carries itself"))
      (should-not (org-convect-test--entry entries "a team that runs itself"))
      (should (equal (plist-get area :serves) '("a team that carries itself")))
      (should-not (seq-find (lambda (f) (eq (plist-get f :kind) 'unresolved-serves))
                            (org-convect-findings entries))))))

(ert-deftest org-convect-test-reword-keeps-what-it-used-to-say ()
  "A ladder that changes is not a ladder going wrong; what is worth keeping is
that it changed and why, next to the rung where the next review will read it."
  (org-convect-test--with-ladder org-convect-test--ready
    (with-current-buffer (find-file-noselect (car org-convect-files))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Honesty")
      (org-convect-reword "Saying the number" "honesty was too broad to act on")
      (let ((body (buffer-string)))
        (should (string-match-p "Reworded from \"Honesty\"" body))
        (should (string-match-p "honesty was too broad to act on" body))))
    ;; and the note counts as having looked at it
    (should (plist-get (org-convect-test--entry (org-convect-scan) "Saying the number")
                       :reviewed))))

(ert-deftest org-convect-test-reword-reaches-into-other-files ()
  "The ladder may be spread over several files, and a reference in one of them
is exactly as broken as a reference beside the rung."
  (let* ((other (make-temp-file "org-convect-other" nil ".org" "\
* engineering
:PROPERTIES:
:CONVECT_HORIZON: area
:CONVECT_SERVES: a team that runs itself
:END:
reviews come back the same day
")))
    (unwind-protect
        (org-convect-test--with-ladder "\
* a team that runs itself
:PROPERTIES:
:CONVECT_HORIZON: goal
:CONVECT_BY: [2030-12-31]
:END:
the rota has someone else on it
"
          (let ((org-convect-files (list (car org-convect-files) other)))
            (with-current-buffer (find-file-noselect (car org-convect-files))
              (goto-char (point-min))
              (re-search-forward "^\\* a team that runs itself")
              (org-convect-reword "a team that carries itself" "clearer"))
            (should (equal (plist-get (org-convect-test--entry (org-convect-scan)
                                                              "engineering")
                                      :serves)
                           '("a team that carries itself")))))
      (ignore-errors (delete-file other)))))

;;;; Not quoting the source

(defconst org-convect-test--not-ours
  '("monthly personal check-in"
    "quarterly reviews and recalibrations"
    "whenever additional clarity"
    "What are the critical behaviors"
    "Priorities are determined from the top down"
    "ultimate intention for something"
    "keep the engines running"
    "purpose and core values"
    "not paraphrased")
  "Phrasing belonging to the David Allen Company, or claims that we copied it.

The horizon *names* are kept exactly, because they are how anything written
about the model can be read against this package.  Everything said about them
is the author's own, and this list is what keeps that decision from quietly
eroding one convenient sentence at a time.")

(ert-deftest org-convect-test-source-quotes-nothing ()
  "No verbatim source text in the code or the README."
  (dolist (file '("../org-convect-core.el" "../org-convect-act.el"
                  "../org-convect.el" "../README.org"))
    (let ((path (expand-file-name file org-convect-test--dir)))
      (when (file-readable-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (dolist (phrase org-convect-test--not-ours)
            (goto-char (point-min))
            (should (equal (list file phrase (search-forward phrase nil t))
                           (list file phrase nil)))))))))

;;;; The seam

(ert-deftest org-convect-test-core-does-not-reach-into-the-overlay ()
  "What makes the overlay removable: the ladder cannot have grown a quiet
dependency on it.

Two ways it could.  It could read an `ACT_' property, which is the obvious
one.  Or it could *call* the overlay -- which the property check misses
entirely, because a function name is not a property string, and which is the
easier mistake to make when the overlay already has the answer you want.

Mentioning it in a docstring is fine and often useful, so what is banned is a
call: an open paren in front of the name."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "../org-convect-core.el" org-convect-test--dir))
    (goto-char (point-min))
    (should-not (re-search-forward "\"ACT_" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "(org-convect-act-" nil t))))

(ert-deftest org-convect-test-ladder-stands-without-the-overlay ()
  "Loaded on its own, this file exercises a ladder with no ACT anywhere in
the image.  That is the removability condition, run rather than asserted."
  (skip-unless (not (featurep 'org-convect-act)))
  (org-convect-test--with-ladder org-convect-test--ladder-written
    (let ((entries (org-convect-scan)))
      (should (= (length entries) 3))
      (should (org-convect-findings entries))
      (should-not (org-convect-unfinished entries))
      (should (eq (org-convect-next-rung entries) 'vision)))))

(provide 'org-convect-core-test)

;;; org-convect-core-test.el ends here
