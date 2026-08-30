;;; org-convect-act-test.el --- Tests for the ACT overlay  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run after the core tests, from the package root:
;;
;;   emacs --batch -Q -L . -l test/org-convect-core-test.el \
;;         -l test/org-convect-act-test.el -f ert-run-tests-batch-and-exit
;;
;; The fixture is the core suite's, deliberately: the overlay reads the same
;; ladder everything else does, and a fixture of its own would let the two
;; drift apart.

;;; Code:

(require 'ert)
(require 'seq)
(require 'cl-lib)
(require 'org-convect-core-test)
(require 'org-convect-act)

;;;; The ACT overlay

(ert-deftest org-convect-test-choice-points-belong-to-their-parent ()
  "A choice point says which value it is about by being its child, so the
count has to come out per parent and not pooled."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let* ((entries (org-convect-scan))
           (honesty (org-convect-test--entry entries "Honesty"))
           (craft (org-convect-test--entry entries "Craft"))
           (area (org-convect-test--entry entries "engineering")))
      (should (= 3 (length (org-convect-act-choice-points honesty))))
      (should (= 3 (length (org-convect-act-choice-points craft))))
      (should-not (org-convect-act-choice-points area)))))

(ert-deftest org-convect-test-choice-points-are-newest-first ()
  "Ordered by the stamp they carry, not by where they happen to sit."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let* ((honesty (org-convect-test--entry (org-convect-scan) "Honesty"))
           (points (org-convect-act-choice-points honesty)))
      (should (equal (mapcar (lambda (p) (plist-get p :name)) points)
                     '("Rounded the number down"
                       "Let the estimate stand uncorrected"
                       "Wanted to keep quiet about the estimate"))))))

(ert-deftest org-convect-test-drift-needs-a-run ()
  "Three moves away in a row is lost alignment.  Craft has three aways among
its points but the newest is `towards', and that is not drift."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let* ((entries (org-convect-scan))
           (drift (org-convect-act-drift entries)))
      (should (equal (mapcar (lambda (d) (plist-get (car d) :name)) drift)
                     '("Honesty"))))))

(ert-deftest org-convect-test-drift-run-is-configurable ()
  "A run of two catches Craft's older pair only if the newest is not towards;
it is, so raising sensitivity still does not report it."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((drift (org-convect-act-drift (org-convect-scan) 2)))
      (should (equal (mapcar (lambda (d) (plist-get (car d) :name)) drift)
                     '("Honesty"))))))

(ert-deftest org-convect-test-domain-gaps ()
  "A domain no area claims is a domain nobody is maintaining."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((org-convect-act-domains '("work" "health")))
      (should (equal (org-convect-act-domain-gaps (org-convect-scan))
                     '("health"))))))

(ert-deftest org-convect-test-domain-is-read-off-the-entry ()
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((entries (org-convect-scan)))
      (should (equal (org-convect-act-domain
                      (org-convect-test--entry entries "engineering"))
                     "work"))
      (should-not (org-convect-act-domain
                   (org-convect-test--entry entries "admin"))))))

;;;; Writing a domain onto an area

(ert-deftest org-convect-test-domain-is-asked-for-areas-only ()
  "A domain checks what is being maintained, and only areas maintain."
  (let ((org-convect-act-domains '("work" "health")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "work")))
      (should (equal (org-convect-act-domain-property 'area)
                     '(("ACT_DOMAIN" . "work"))))
      (dolist (horizon '(goal vision purpose))
        (should-not (org-convect-act-domain-property horizon))))))

(ert-deftest org-convect-test-domain-is-not-asked-without-domains ()
  "A question with no answers teaches people to skip questions."
  (let ((org-convect-act-domains nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "asked with nothing to offer"))))
      (should-not (org-convect-act-domain-property 'area)))))

(ert-deftest org-convect-test-domain-may-be-declined ()
  (let ((org-convect-act-domains '("work")))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
      (should-not (org-convect-act-domain-property 'area)))))

(ert-deftest org-convect-test-domain-question-is-registered ()
  "Loading the overlay is what puts the question in front of anyone adding an
area; nothing in the core mentions it."
  (should (memq #'org-convect-act-domain-property
                org-convect-add-property-functions)))

(ert-deftest org-convect-test-a-written-domain-closes-the-gap ()
  "The whole point: an area added through the command has the domain the gap
check reads, which it never did before."
  (let ((org-convect-act-domains '("work" "health")))
    (org-convect-test--with-ladder (org-convect--build-skeleton)
      (org-convect-add 'area "engineering" nil '(("ACT_DOMAIN" . "work")))
      (should (equal (org-convect-act-domain-gaps (org-convect-scan))
                     '("health"))))))

(ert-deftest org-convect-test-choice-points-vouch-for-a-rung ()
  "The overlay is what supplies the evidence: a principle with choice points
under it is the most-used thing in the file, and the links cannot show it."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let* ((entries (org-convect-scan))
           (honesty (org-convect-test--entry entries "Honesty"))
           (area (org-convect-test--entry entries "admin")))
      (should (org-convect-act-in-use-p honesty))
      (should-not (org-convect-act-in-use-p area))
      (should (memq #'org-convect-act-in-use-p org-convect-in-use-functions))
      ;; and so the principle is not reported as shaping nothing
      (should-not (memq 'unserved-rung
                        (org-convect-test--kinds
                         (org-convect-findings entries) "Honesty"))))))

(provide 'org-convect-act-test)

;;; org-convect-act-test.el ends here
