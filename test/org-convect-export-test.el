;;; org-convect-export-test.el --- Tests for the exported review  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run from the package root:
;;
;;   emacs --batch -Q -L . -L test -l test/org-convect-export-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Every test here was checked against a deliberately broken implementation
;; before being kept.  A test that passes either way is not a test.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-convect-core-test)
(require 'org-convect-export)

(defun org-convect-export-test--body (text)
  "TEXT with the stylesheet taken out, which is most of it."
  (replace-regexp-in-string "^#\\+html_head:.*\n" "" text))

(defun org-convect-export-test--headlines (text)
  "The headlines of Org TEXT, as (LEVEL . TITLE)."
  (seq-keep (lambda (line)
              (when (string-match "\\`\\(\\*+\\) \\(.*\\)\\'" line)
                (cons (length (match-string 1 line)) (match-string 2 line))))
            (split-string text "\n")))

;;;; The document

(ert-deftest org-convect-export-test-a-rung-is-a-heading ()
  "The page is read, so the rungs are headings and not a column of text."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((heads (org-convect-export-test--headlines
                  (org-convect--export-document nil nil (current-time)))))
      (should (member '(1 . "Purpose and Principles") heads))
      (should (member '(2 . "Honesty") heads))
      (should (member '(2 . "engineering") heads)))))

(ert-deftest org-convect-export-test-the-guidance-is-quoted ()
  "Said about the rungs below it rather than being one of them."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((text (org-convect-export-test--body
                 (org-convect--export-document nil nil (current-time)))))
      (should (string-match-p "#\\+begin_quote\n.*\n?.*Am I still behaving"
                              text)))))

(ert-deftest org-convect-export-test-the-descent-nests ()
  "`t' on the board puts a rung under what it serves.  So does the page: a
descent flattened into one level is not the descent."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((heads (org-convect-export-test--headlines
                  (org-convect--export-document t nil (current-time)))))
      (should (member '(1 . "Honesty") heads))
      (should (member '(2 . "A team that runs itself") heads))
      (should (member '(3 . "engineering") heads)))))

(ert-deftest org-convect-export-test-the-descent-names-its-altitudes ()
  "A descent has no section headings, so the questions have to say which
altitude they are asking about."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let ((text (org-convect-export-test--body
                 (org-convect--export-document t nil (current-time)))))
      (should (string-match-p "\\*Purpose and Principles\\*" text))
      (should (string-match-p "\\*Areas of Focus and Accountability\\*" text)))))

(ert-deftest org-convect-export-test-only-wanting-narrows ()
  "The same argument the board takes, meaning the same thing.

`admin' has never been reviewed, so it is due whatever day this runs on;
`Craft' has no calendar and nothing is watching it here, so it is not."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let* ((org-convect-signal-functions nil)
           (heads (org-convect-export-test--headlines
                   (org-convect--export-document nil t (current-time)))))
      (should (member '(2 . "admin") heads))
      (should-not (member '(2 . "Craft") heads))
      (should-not (member '(1 . "Purpose and Principles") heads)))))

(ert-deftest org-convect-export-test-nothing-put-in-can-open-a-heading ()
  "The document\='s structure is the exporter\='s and not the text\='s.

A star at column zero is a heading wherever it lands -- inside a quotation as
readily as between two rungs -- and everything after one would be read as
belonging to something nobody wrote.  One column in, and none of it can."
  (let ((out (org-convect--export-indent "first\n* not a heading\n\nlast")))
    (should (equal out " first\n * not a heading\n\n last"))
    (should-not (string-match-p "^\\*" out))))

;;;; The file

(ert-deftest org-convect-export-test-writes-a-page-that-carries-its-style ()
  "One file: no stylesheet to fetch, so it survives being mailed."
  (org-convect-test--with-ladder org-convect-test--ladder
    (let* ((dir (file-name-as-directory (make-temp-file "convect-export" t)))
           (org-convect-export-directory dir)
           (org-convect-export-open nil)
           (inhibit-message t)
           target)
      (unwind-protect
          (progn
            (setq target (org-convect-review-export))
            (should (file-exists-p target))
            (should (string-suffix-p ".html" target))
            (let ((html (with-temp-buffer
                          (insert-file-contents target)
                          (buffer-string))))
              (should (string-match-p "<style>" html))
              (should (string-match-p "font-size" html))
              (should (string-match-p "Honesty" html))
              (should (string-match-p "Horizons Review" html))
              (should-not (string-match-p "<link rel=\"stylesheet\"" html))))
        (delete-directory dir t)))))

(provide 'org-convect-export-test)

;;; org-convect-export-test.el ends here
