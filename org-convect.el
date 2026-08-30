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
;;   org-convect-core.el  the ladder -- reading, resolving, what is due
;;   org-convect-act.el   the ACT overlay, and the seam it can be removed at
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

(provide 'org-convect)

;;; org-convect.el ends here
