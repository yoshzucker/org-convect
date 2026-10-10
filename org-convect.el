;;; org-convect.el --- The Horizons of Focus above the project  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-convect
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Org holds projects and next actions well.  Everything above them -- the
;; areas being maintained, the goals they serve, the vision behind those, the
;; purpose behind that -- has nowhere to live, and so tends not to exist.
;;
;; Convection stops when there is no difference.  What this package works on is
;; the difference between what was declared to matter and where the hours
;; actually went, and it reads in both directions:
;;
;;   upward    the clock says which areas were paid for, and which of them
;;             nobody ever claimed
;;   downward  the ladder says which goals nothing serves, and which rungs are
;;             past the review GTD asks for
;;
;; Heat rises and what rises then governs what is below: GTD's own two
;; directions.  What matters most is settled from the top -- purpose shapes the
;; vision, the vision shapes the goals, the goals frame the areas -- and yet
;; the ladder gets written from the bottom, because nobody buried in the day
;; can answer honestly about purpose.  Both are true and neither is the other.
;; The file is ordered for the first; `org-convect-next-rung' asks for the
;; second.
;;
;; Layout:
;;
;;   org-convect-core.el    the ladder -- reading, resolving, what is due
;;   org-convect-act.el     the ACT overlay, and the seam it can be removed at
;;   org-convect-export.el  the review written out as a page to read away
;;                          from the machine
;;
;; Requiring `org-convect' gets both.  Requiring only `org-convect-core' gets
;; a ladder with no overlay, which is a supported way to run it.
;;
;; Getting Things Done and GTD are registered trademarks of the David Allen
;; Company.  This package is an independent implementation and is not
;; affiliated with, authorised by or endorsed by them.  The horizon names are
;; used to identify the model; every description of them here is the author's
;; own words.

;;; Code:

(require 'org-convect-core)
(require 'org-convect-act)
(require 'org-convect-export)

;; Every command loads this file first, whichever library defines it, so that
;; a configuration waiting on `org-convect' is in place before the first one
;; runs.  Requiring `org-convect-core' alone still gives the ladder without
;; ACT or the export, as before.
;;;###autoload (autoload 'org-convect-read-area "org-convect" "Read one declared area by name and return it." nil)
;;;###autoload (autoload 'org-convect-set-area "org-convect" "Put a declared area's name in the `CONVECT_AREA\\=' of the entry at point." t)
;;;###autoload (autoload 'org-convect-open "org-convect" "Open the horizons file, writing its frame the first time." t)
;;;###autoload (autoload 'org-convect-refresh-guides "org-convect" "Rewrite the guidance in the ladder file and leave everything else alone." t)
;;;###autoload (autoload 'org-convect-set-date "org-convect" "Set `CONVECT_BY\\=' on the rung at point, or clear it with an empty answer." t)
;;;###autoload (autoload 'org-convect-link "org-convect" "Record what the rung at point is in service of." t)
;;;###autoload (autoload 'org-convect-relink "org-convect" "Repoint or drop one of the links written on the rung at point." t)
;;;###autoload (autoload 'org-convect-add "org-convect" "Write a rung called NAME at HORIZON, in service of SERVES." t)
;;;###autoload (autoload 'org-convect-declare "org-convect" "Make the heading at point, or every heading in the region, a rung." t)
;;;###autoload (autoload 'org-convect-reviewed "org-convect" "Write CONCLUSION onto the rung at point and call it reviewed." t)
;;;###autoload (autoload 'org-convect-note "org-convect" "Write TEXT as an ordinary note on a rung read by name." t)
;;;###autoload (autoload 'org-convect-reword "org-convect" "Rename the rung at point to NEW, saying REASON, and follow the references." t)
;;;###autoload (autoload 'org-convect-doctor "org-convect" "Show what the ladder has and what it is missing.  Writes nothing." t)
;;;###autoload (autoload 'org-convect-plan "org-convect" "Open a planning space under the project at point." t)
;;;###autoload (autoload 'org-convect-review "org-convect" "Show the ladder, with what wants looking at marked." t)
;;;###autoload (autoload 'org-convect-lineage-show "org-convect" "Show one thread of the ladder: the rung at point and everything linked to it." t)
;;;###autoload (autoload 'org-convect-history-show "org-convect" "Show what has happened to the rung at point, newest first." t)
;;;###autoload (autoload 'org-convect-act-target "org-convect" "Capture target: the horizon entry a choice point is about." nil)
;;;###autoload (autoload 'org-convect-review-export "org-convect" "Write the review as a page to read away from Emacs." t)

(provide 'org-convect)

;;; org-convect.el ends here
