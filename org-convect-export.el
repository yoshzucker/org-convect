;;; org-convect-export.el --- The review, off the screen  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-convect

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A monthly sitting is not done at the keyboard.  It is done in a chair,
;; often away from the machine that holds the file, and what is being asked --
;; is this still mine, is the standard being met -- is answered better by
;; someone not looking at their inbox.
;;
;; So the board can be written out as a page: one file, no styling to fetch,
;; large enough to read on a phone and to print without shrinking.  HTML
;; rather than PDF, because a browser turns the page into a PDF on any machine
;; and a LaTeX toolchain is a dependency the reader may not have.
;;
;; Both orders come out: the four altitudes, and the descent that `t' draws on
;; the board, where a rung sits under what it serves.  What is being read is
;; the same in either, which is the point of the toggle in the first place.
;;
;; Requiring `org-convect-core' alone leaves this out, and the board says so
;; rather than offering a key that is not there.

;;; Code:

(require 'org)
(require 'ox-html)
(require 'org-convect-core)
(require 'seq)

(defgroup org-convect-export nil
  "Writing the review out to read away from Emacs."
  :group 'org-convect
  :prefix "org-convect-export-")

(defcustom org-convect-export-directory nil
  "Where an exported review is written.

Nil is `temporary-file-directory', which is right for a page that is read
once and then thrown away.  Name a directory to keep them."
  :type '(choice (const :tag "Temporary directory" nil) directory)
  :group 'org-convect-export)

(defcustom org-convect-export-open t
  "Whether an exported review is opened after it is written.

The browser is where it gets printed or sent on, so opening it is usually
the next thing anyway."
  :type 'boolean
  :group 'org-convect-export)

(defcustom org-convect-export-style
  '(":root { color-scheme: light; }"
    "body { font-family: -apple-system, BlinkMacSystemFont, \"Hiragino Sans\","
    "       \"Noto Sans JP\", \"Yu Gothic\", sans-serif;"
    "       font-size: 18px; line-height: 1.75; color: #1b1b1b;"
    "       background: #fff; max-width: 40em; margin: 2.5rem auto;"
    "       padding: 0 1.2rem; }"
    ".title { font-size: 1.5rem; margin-bottom: 0.2rem; }"
    ".subtitle { color: #555; font-size: 1rem; margin-top: 0; }"
    "h2 { font-size: 1.2rem; margin: 2.4rem 0 0.6rem;"
    "     border-bottom: 1px solid #ddd; padding-bottom: 0.2rem; }"
    "h3, h4, h5 { font-size: 1.05rem; margin: 1.6rem 0 0.2rem; }"
    "p { margin: 0.45rem 0; }"
    "blockquote { margin: 0.4rem 0 1.2rem; padding-left: 0.9rem;"
    "             border-left: 3px solid #ddd; color: #666;"
    "             font-style: italic; font-size: 0.94em; }"
    "blockquote p { margin: 0.25rem 0; }"
    "#postamble, #org-div-home-and-up { display: none; }"
    "@media (max-width: 480px) { body { font-size: 17px; margin: 1.2rem auto; } }"
    "@media print {"
    "  body { font-size: 11.5pt; max-width: none; margin: 0; }"
    "  h2 { page-break-after: avoid; }"
    "  h3, h4, h5 { page-break-after: avoid; }"
    "  blockquote { color: #444; }"
    "}")
  "The page's own stylesheet, one CSS line per string.

Carried here rather than fetched, so the file is one thing that can be
mailed or dropped on a phone.  The size is deliberate: a review read at
arm's length, and printed without anybody reaching for a magnifier."
  :type '(repeat string)
  :group 'org-convect-export)

(defun org-convect--export-indent (text)
  "Return TEXT with every line moved in one column.

The document's structure is this file's and not the text's.  A star at column
zero is a heading wherever it lands -- inside a quotation as readily as
between two rungs -- and everything after one would be read as belonging to
something nobody wrote.  A column costs nothing to read and settles it."
  (mapconcat (lambda (line) (if (string-empty-p line) line (concat " " line)))
             (split-string text "\n")
             "\n"))

(defun org-convect--export-rung (entry level now called entries scan threaded
                                       &optional also)
  "ENTRY as an Org subtree at LEVEL.

The same four things the board puts on and under a row: the name, how it
stands, what is written on it, and the links.  ALSO is what a descent could
not draw, exactly as on the board."
  (let* ((name (plist-get entry :name))
         (status (org-convect--review-status entry now called))
         (body (org-with-point-at (plist-get entry :marker) (org-convect--body)))
         (tally (org-convect--evidence-tally entry entries scan threaded)))
    (concat (make-string level ?*) " " name "\n"
            (if (and status (not (string-empty-p status)))
                (format " *%s*\n" status)
              "")
            (if (string-empty-p body)
                ""
              (concat "\n" (org-convect--export-indent body) "\n"))
            (if also
                (format "\n /%s also serves %s/\n"
                        org-convect-review-mark (string-join also ", "))
              "")
            (if tally (format "\n /%s/\n" tally) ""))))

(defun org-convect--export-guide (horizon &optional named)
  "HORIZON's review question and cadence, as a quotation.

A quotation because that is what it is on the page: something said about the
rungs below it rather than one of them.  NAMED writes the altitude into the
quotation, which a descent needs -- there are no section headings there to
say which altitude is being asked about."
  (if-let ((asks (org-convect-guide horizon :review)))
      (format "#+begin_quote\n%s%s\n#+end_quote\n\n"
              (if named
                  (format " *%s*\n\n" (org-convect-horizon-name horizon))
                "")
              (org-convect--export-indent asks))
    ""))

(defun org-convect--export-document (threaded only-wanting now)
  "The review as Org text, in the order THREADED asks for.

ONLY-WANTING leaves out what is not asking to be looked at, which is what
the board does with the same argument."
  (let* ((entries (org-convect-scan))
         (called (org-convect-called entries))
         (due (org-convect-due entries now))
         (scan (org-convect-clock-rows))
         (wanted (lambda (e) (or (memq e due) (assq e called)))))
    (concat
     "#+title: Horizons Review\n"
     (format "#+subtitle: %s -- %d due, %d called\n"
             (format-time-string "%Y-%m-%d" now) (length due) (length called))
     "#+options: toc:nil num:nil author:nil timestamp:nil ^:nil\n"
     (mapconcat (lambda (line) (concat "#+html_head: " line))
                (cons "<style>" (append org-convect-export-style '("</style>")))
                "\n")
     "\n\n"
     (if threaded
         (concat
          (mapconcat (lambda (horizon)
                       (org-convect--export-guide horizon t))
                     (reverse (mapcar #'car org-convect-horizons)) "")
          (mapconcat
           (pcase-lambda (`(,entry ,depth ,_prefix ,also))
             (org-convect--export-rung entry (1+ depth) now called entries scan
                                       t also))
           (org-convect--review-rows entries only-wanting wanted)
           "\n"))
       (mapconcat
        (lambda (horizon)
          (let ((at (seq-filter (lambda (e)
                                  (or (not only-wanting) (funcall wanted e)))
                                (org-convect-entries entries horizon))))
            (when (or at (not only-wanting))
              (concat "* " (org-convect-horizon-name horizon) "\n"
                      (org-convect--export-guide horizon)
                      (if at
                          (mapconcat
                           (lambda (entry)
                             (org-convect--export-rung entry 2 now called
                                                       entries scan nil))
                           at "\n")
                        " /nothing/\n")
                      "\n"))))
        (reverse (mapcar #'car org-convect-horizons))
        "")))))

(defun org-convect--export-target (now)
  "Where today's review is written."
  (expand-file-name (format-time-string "horizons-review-%Y-%m-%d.html" now)
                    (or org-convect-export-directory temporary-file-directory)))

;;;###autoload
(defun org-convect-review-export (&optional file)
  "Write the review as a page to read away from Emacs.

One HTML file, styling and all, so it can be printed, mailed, or read on a
phone.  A browser makes the PDF; asking LaTeX to would make the command
depend on a toolchain that is not on every machine this runs on.

Called on the board, it writes what the board is showing -- the altitudes,
or the descent `\\<org-convect-review-mode-map>\\[org-convect-review-thread]'
draws, and the same narrowing.  Called anywhere else it writes the whole
ladder in its usual order.  With a prefix argument, FILE is asked for."
  (interactive
   (list (when current-prefix-arg
           (read-file-name "Write the review to: "
                           (or org-convect-export-directory
                               temporary-file-directory)
                           nil nil
                           (format-time-string "horizons-review-%Y-%m-%d.html")))))
  (let* ((board (equal (buffer-name) org-convect-review-buffer))
         (threaded (if board org-convect--review-threaded
                     org-convect-review-threaded))
         (only-wanting (and board (car org-convect--review-args)))
         (now (or (and board (cadr org-convect--review-args)) (current-time)))
         (target (or file (org-convect--export-target now)))
         (doc (org-convect--export-document threaded only-wanting now)))
    (make-directory (file-name-directory target) t)
    (with-temp-buffer
      (insert doc)
      (let ((org-export-use-babel nil)
            (default-directory (file-name-directory target)))
        (org-mode)
        (org-export-to-file 'html target)))
    (when org-convect-export-open
      (browse-url-of-file target))
    (message "Review written to %s" target)
    target))

(define-key org-convect-review-mode-map (kbd "e") #'org-convect-review-export)

(provide 'org-convect-export)

;;; org-convect-export.el ends here
