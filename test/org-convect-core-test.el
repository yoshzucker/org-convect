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

(ert-deftest org-convect-test-unserved-goal-is-reported ()
  "The ladder is read upward: a goal nothing serves is the question worth
asking, and a goal that is served is not."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((findings (org-convect-findings (org-convect-scan)
                                          (org-convect-test--day 2026 8 25))))
      (should (memq 'unserved-goal
                    (org-convect-test--kinds findings "A goal nothing serves")))
      (should-not (memq 'unserved-goal
                        (org-convect-test--kinds
                         findings "A team that runs itself"))))))

(ert-deftest org-convect-test-chores-are-not-blamed ()
  "An area serving nothing is not a defect.  Keeping the engines running is
what an area is for, and the company's chores are a real accountability."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((findings (org-convect-findings (org-convect-scan)
                                          (org-convect-test--day 2026 8 25))))
      (should (equal (org-convect-test--kinds findings "admin")
                     '(overdue-review)))
      (should-not (org-convect-test--kinds findings "procurement")))))

;;;; Which rung to ask for next

(ert-deftest org-convect-test-next-rung-is-the-lowest-empty ()
  "GTD's order made mechanical: with areas and goals written and no vision,
what gets asked for is the vision -- not the purpose above it."
  (org-convect-test--with-ladder org-convect-test--ladder
    (should (eq (org-convect-next-rung (org-convect-scan)) 'vision))))

(ert-deftest org-convect-test-next-rung-starts-at-the-bottom ()
  "With nothing written, the question is areas.  Nobody buried in the day can
answer about purpose, and asking anyway is how the exercise gets abandoned."
  (should (eq (org-convect-next-rung nil) 'area)))

(ert-deftest org-convect-test-next-rung-is-nil-when-filled ()
  (org-convect-test--with-ladder "\
* a
:PROPERTIES:
:CONVECT_HORIZON: area
:END:
* b
:PROPERTIES:
:CONVECT_HORIZON: goal
:END:
* c
:PROPERTIES:
:CONVECT_HORIZON: vision
:END:
* d
:PROPERTIES:
:CONVECT_HORIZON: purpose
:END:
"
    (should-not (org-convect-next-rung (org-convect-scan)))))

;;;; Where the time went

(ert-deftest org-convect-test-unclaimed-categories ()
  "Hours booked to a category no area claims are the finding; hours booked to
a declared one are not."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let* ((scan (list :rows '(("engineering" . 600)
                               ("meetings" . 480)
                               ("admin" . 120))))
           (unclaimed (org-convect-unclaimed-categories (org-convect-scan) scan)))
      (should (equal unclaimed '(("meetings" . 480)))))))

(ert-deftest org-convect-test-unclaimed-categories-without-a-clock ()
  "No scan and no org-foresight is an empty answer, not an error: the ladder
reads without a clock."
  (org-convect-test--with-ladder org-convect-test--ladder
    (should-not (org-convect-unclaimed-categories (org-convect-scan)
                                                  (list :rows nil)))))

;;;; The seam

(ert-deftest org-convect-test-core-names-no-act-property ()
  "The core never reads an `ACT_' property.  This is what makes the overlay
removable: the ladder cannot have grown a quiet dependency on it."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "../org-convect-core.el" org-convect-test--dir))
    (goto-char (point-min))
    (should-not (re-search-forward "\"ACT_" nil t))))

(ert-deftest org-convect-test-ladder-stands-without-the-overlay ()
  "Loaded on its own, this file exercises a ladder with no ACT anywhere in
the image.  That is the removability condition, run rather than asserted."
  (skip-unless (not (featurep 'org-convect-act)))
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((entries (org-convect-scan)))
      (should (= (length entries) 7))
      (should (org-convect-findings entries))
      (should (eq (org-convect-next-rung entries) 'vision)))))

(provide 'org-convect-core-test)

;;; org-convect-core-test.el ends here
