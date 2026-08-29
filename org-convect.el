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
;; directions.  Priorities are determined from the top down, and the ladder is
;; nonetheless written from the bottom up, because nobody buried in the day can
;; answer honestly about purpose.  Both are true and neither is the other.
;;
;; Layout:
;;
;;   org-convect-core.el  the ladder -- reading, resolving, what is due
;;   org-convect-act.el   the ACT overlay, and the seam it can be removed at
;;
;; Requiring `org-convect' gets both.  Requiring only `org-convect-core' gets
;; a ladder with no overlay, which is a supported way to run it.

;;; Code:

(require 'org-convect-core)
(require 'org-convect-act)

(provide 'org-convect)

;;; org-convect.el ends here
