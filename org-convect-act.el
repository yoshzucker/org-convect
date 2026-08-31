;;; org-convect-act.el --- The ACT overlay on the ladder  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-convect
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; GTD says *when* to revisit the upper horizons and never says what to ask
;; there.  The weekly review has a procedure -- get clear, get current, get
;; creative -- and nothing above projects has an equivalent.  Worse, the two
;; highest rungs have no calendar at all: GTD reads them when direction or
;; motivation has gone, which is a condition rather than a date, and a
;; condition nobody notices from inside it.  It is a trigger that never fires
;; on its own.
;;
;; ACT -- Acceptance and Commitment Therapy -- has the questions GTD is
;; missing, and one of them turns that trigger into something observable:
;;
;;   towards / partly / away  did this move me toward what I said matters
;;   struggle                 how hard I fought the feeling that pulled
;;   workability              is this working, in the service of what matters
;;
;; Workability is the one that carries.  "Is this still current" stops meaning
;; anything as the ladder rises -- a purpose does not go out of date -- but
;; "is this working" can be asked at every altitude.  And a principle whose
;; last few choice points all read `away' is a principle that alignment has
;; quietly left, which is precisely the condition GTD names and cannot detect.
;; And GTD already treats the top rung as being partly about conduct -- what
;; standards you hold, not only what you are for -- so keeping a record of
;; behaviour against it is not a foreign idea grafted on.
;;
;; The overlay adds nothing to the ladder's shape.  A value is not a separate
;; kind of thing living in a separate file -- it is a rung, usually a
;; `purpose' one, and a choice point is written as its child.  What the choice
;; point is about is said by its parent, so there is no property naming it.
;;
;; This file is the seam.  Everything ACT contributes is prefixed `ACT_' in
;; the file and lives in this one feature; `grep ACT_' is the whole of it.
;; Delete it and the ladder still reads, still resolves, still knows what is
;; overdue -- only the upper two rungs lose the signal that calls them.

;;; Code:

(require 'org)
(require 'seq)
(require 'org-convect-core)

(defcustom org-convect-act-domains nil
  "Life domains checked for coverage, e.g. work, family, health.

Not a rung and not a hierarchy.  In ACT these exist so that a set of values
does not turn out to be entirely about work, and here they do the same job for
areas: a domain with no area of accountability in it is a domain nobody is
maintaining.  Few is better than many -- a domain nothing is ever written
about becomes a blank that gets skipped, and skipping is a habit.

Left empty by default: which domains matter is a personal answer and belongs in
your configuration, not in the package."
  :type '(repeat string)
  :group 'org-convect)

(defcustom org-convect-act-drift-run 3
  "How many consecutive `away' choice points count as lost alignment.

The signal that calls a rung with no calendar.  Small on purpose: three in a
row is a pattern, and waiting for more is waiting for the year to end."
  :type 'integer
  :group 'org-convect)

(defconst org-convect-act-moves '("towards" "partly" "away")
  "The answers ACT accepts to \"did this move me toward it?\".

Workability, and the only test ACT applies to an action.  Note what is absent:
how strong the feeling was.  Intensity is deliberately not recorded, because
tracking it invites wanting it lower, and wanting it lower is the trap the
whole practice is about.")

;; The overlay's own columns, appended rather than replacing the frame's: with
;; them, `\\[org-columns]' over a value is the table of its choice points, so
;; the property drawers pay for themselves and nothing is stored twice.  They
;; leave with this feature.
(dolist (column '("%ACT_STRUGGLE(Struggle)" "%ACT_MOVE(Moved)"))
  (add-to-list 'org-convect-columns column t))

;;;; Reading choice points

(defun org-convect-act-domain (entry)
  "The `ACT_DOMAIN' of ENTRY, or nil.

Read through the entry's marker rather than during the ladder scan, so the core
never has to know this property exists."
  (org-entry-get (plist-get entry :marker) "ACT_DOMAIN"))

(defun org-convect-act-choice-points (entry)
  "Choice points recorded under ENTRY, newest first.

Direct children only.  A choice point is written as a child of the rung it is
about -- that is what says which value it concerns -- so reading them apart
from their parent would lose the one thing that identifies them."
  (let ((marker (plist-get entry :marker))
        points)
    (when (marker-buffer marker)
      (with-current-buffer (marker-buffer marker)
        (org-with-wide-buffer
         (goto-char marker)
         (org-back-to-heading t)
         (let ((end (save-excursion (org-end-of-subtree t t)))
               (level (1+ (org-current-level))))
           (while (and (outline-next-heading) (< (point) end))
             (when (and (= (org-current-level) level)
                        (org-entry-get nil "ACT_MOVE"))
               (push (list :name     (org-get-heading t t t t)
                           :move     (org-entry-get nil "ACT_MOVE")
                           :struggle (org-entry-get nil "ACT_STRUGGLE")
                           :created  (org-entry-get nil "CREATED")
                           :marker   (point-marker))
                     points)))))))
    (org-convect-act--by-recency points)))

(defun org-convect-act--by-recency (points)
  "Sort POINTS newest first by their `CREATED' stamp.

Document order would do it only as long as choice points are appended, and
whether they are is a `:prepend' away from being the other way round.  The
stamp is what the record actually claims, so it is what decides."
  (sort points
        (lambda (a b)
          (let ((ta (org-convect-act--created a))
                (tb (org-convect-act--created b)))
            (cond ((and ta tb) (time-less-p tb ta))
                  (ta t)
                  (t nil))))))

(defun org-convect-act--created (point)
  "The time in POINT's `CREATED' stamp, or nil when it has none."
  (let ((stamp (plist-get point :created)))
    (and stamp (ignore-errors (org-time-string-to-time stamp)))))

;;;; What the record says

(defun org-convect-act-drift (entries &optional run)
  "Rungs of ENTRIES whose last RUN choice points all read `away'.

RUN defaults to `org-convect-act-drift-run'.  Returns a list of
\(ENTRY . POINTS), POINTS being the run that triggered it, newest first.

This is the whole reason the overlay exists.  GTD asks for purpose and vision
to be revisited whenever alignment is needed and gives no way to notice that it
is; a run of moves away from a principle is that noticing, and it arrives on
its own rather than waiting to be felt."
  (let ((run (or run org-convect-act-drift-run))
        found)
    (dolist (entry entries (nreverse found))
      (let ((points (org-convect-act-choice-points entry)))
        (when (and (>= (length points) run)
                   (seq-every-p (lambda (p) (equal (plist-get p :move) "away"))
                                (seq-take points run)))
          (push (cons entry (seq-take points run)) found))))))

(defun org-convect-act-domain-gaps (entries)
  "Domains in `org-convect-act-domains' that no area of ENTRIES claims.

A domain with nothing maintaining it is the blind spot the domain list exists
to expose: health with no area of accountability means health is not anybody's
job, which is usually news."
  (let ((claimed (delq nil (mapcar #'org-convect-act-domain
                                   (org-convect-entries entries 'area)))))
    (seq-remove (lambda (domain) (member domain claimed))
                org-convect-act-domains)))

(defun org-convect-act-domain-property (horizon)
  "Ask which life domain an area belongs to, as an alist for `org-convect-add'.

Registered on `org-convect-add-property-functions', which is the whole of the
overlay's reach into writing a rung.  Only areas are asked -- a domain is a
check on what is being maintained, and nothing above the areas maintains
anything directly -- and nothing is asked at all when no domains are
configured, because a question with no answers is a question that teaches
people to skip questions."
  (when (and (eq horizon 'area) org-convect-act-domains)
    (let ((domain (completing-read
                   "Life domain (optional): " org-convect-act-domains nil nil)))
      (and (org-string-nw-p domain) (list (cons "ACT_DOMAIN" domain))))))

(add-hook 'org-convect-add-property-functions #'org-convect-act-domain-property)

(defun org-convect-act-in-use-p (entry)
  "Non-nil when ENTRY has choice points recorded under it.

Registered on `org-convect-in-use-functions'.  A principle with a month of
choice points under it is the most-used thing in the file, and the ladder
cannot see that: nothing was derived from it, because what it produces is
conduct rather than a rung.  This is where the evidence actually is."
  (and (org-convect-act-choice-points entry) t))

(add-hook 'org-convect-in-use-functions #'org-convect-act-in-use-p)

(defun org-convect-act-review-evidence (entry)
  "Lines about ENTRY's choice points, for the review board.

What the ladder can show about a rung is what was declared: the standard, the
links, the date it was last looked at.  What actually happened is somewhere
else, and this is the half that carries it -- how often the moment came, and
which way it went."
  (let ((points (org-convect-act-choice-points entry)))
    (when points
      (let ((away (seq-count (lambda (p) (equal (plist-get p :move) "away"))
                             points)))
        (list (format "%d choice point%s, %d away"
                      (length points) (if (= 1 (length points)) "" "s") away))))))

(defun org-convect-act-signal (entry)
  "Say why ENTRY is being called for, or nil.

The rungs above the goals have no cadence, so without something like this they
are never called for at all.  A run of moves away from a principle is the
condition GTD describes and cannot detect: alignment has gone, and it went
without anybody deciding to let it."
  (let ((run (assq entry (org-convect-act-drift (list entry)))))
    (and run
         (format "%d away in a row -- alignment has gone" (length (cdr run))))))

(defun org-convect-act-history (entry)
  "ENTRY's choice points as history rows, (TIME choice TEXT).

The half of a rung's past the ladder cannot see.  Its notes hold what was
concluded about it; these hold what actually happened while it was in force."
  (delq nil
        (mapcar (lambda (point)
                  (let ((time (org-convect-act--created point)))
                    (and time
                         (list time 'choice
                               (format "%s · %s"
                                       (or (plist-get point :move) "?")
                                       (plist-get point :name))))))
                (org-convect-act-choice-points entry))))

(add-hook 'org-convect-history-functions #'org-convect-act-history)
(add-hook 'org-convect-review-evidence-functions #'org-convect-act-review-evidence)
(add-hook 'org-convect-signal-functions #'org-convect-act-signal)

;;;; Recording one

(defcustom org-convect-act-target-horizons nil
  "Rungs a choice point may be recorded against; nil means any.

Usually left alone.  Most choice points are about a principle, but an area can
be the honest answer -- the pull was away from a responsibility, not from a
value -- and refusing to file it anywhere is how a practice stops being kept."
  :type '(repeat symbol)
  :group 'org-convect)

;;;###autoload
(defun org-convect-act-target ()
  "Capture target: the horizon entry a choice point is about.

Leaves point on the chosen heading, so an `entry' template files the choice
point as its child.  Meant for `org-capture-templates':

  (\"v\" \"choice point\" entry (function org-convect-act-target) TEMPLATE)

The template itself stays in your configuration: its prompts are wording in
your language, which is a personal answer like the domain list, not mechanism."
  (let* ((entry (org-convect-read-entry
                 "Choice point about: " nil org-convect-act-target-horizons))
         (marker (plist-get entry :marker)))
    (set-buffer (marker-buffer marker))
    (widen)
    (goto-char marker)
    (org-back-to-heading t)))

(provide 'org-convect-act)

;;; org-convect-act.el ends here
