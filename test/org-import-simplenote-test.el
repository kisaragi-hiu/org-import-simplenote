;;; org-import-simplenote-test.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests.
;;
;; To get tests involving timestamps with offsets to work reliably,
;; pass the expected timestamp to
;; `org-import-simplenote--normalize-timestamp'.
;; `test--normalized-timestamp' is also available as a macro that
;; replaces all %s fields in a format string with the normalized
;; timestamp. This ensures tests work regardless of the system
;; timezone.
;;
;; The timezone normalization does not work on Emacs < 26, however. In
;; Emacs < 26, tests will only work if the system timezone matches the
;; offsets specified in the tests; as we run tests for older Emacs in
;; CI only (on GitHub Actions), which use UTC+0, please just make sure
;; timestamps in specified tests are formatted to UTC+0.

;;; Code:

(require 'ert)
(require 'org-import-simplenote)

(defun test--remove-leading-spaces (str)
  "Remove leading spaces from STR."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (while (re-search-backward (rx bol (+? " ") ":") nil t)
        (replace-match ":"))
      (buffer-string))))

(defmacro test--normalized-timestamp (timestamp format)
  "Normalize TIMESTAMP and format it with FORMAT.

Every single instance of %s in FORMAT will be replaced by the
normalized version of TIMESTAMP."
  (let ((count
         ;; This is basically a copy of `s-count-matches'.
         ;;
         ;; We need this because "%1$s" was only added in Emacs 26,
         ;; and I'm trying to maintain support back to Emacs 24.
         (save-match-data
           (with-temp-buffer
             (insert format)
             (goto-char (point-min))
             (count-matches "%s" (point-min) (point-max)))))
        (normalized (org-import-simplenote--normalize-timestamp timestamp)))
    `(format ,format ,@(make-list count normalized))))

(ert-deftest org-import-simplenote--insert-note--both ()
  (with-temp-buffer
    (org-mode)
    (let ((org-import-simplenote-title-format 'both))
      (org-import-simplenote--insert-note
       '((id . "3332bd3d9ecb42598c99cacc55773fc8")
         (content . "test\C-m
\C-m
content\C-m")
         (creationDate . "2021-10-25T05:30:51.000Z")
         (lastModified . "2021-10-30T11:18:14.594Z"))))
    (should (equal (test--remove-leading-spaces
                    (buffer-substring-no-properties (point-min) (point-max)))
                   (test--normalized-timestamp
                    "2021-10-25T05:30:51+0000"
                    "
* %s test
:PROPERTIES:
:created:  %s
:END:

content")))))

(ert-deftest org-import-simplenote--insert-note--first-line ()
  (with-temp-buffer
    (org-mode)
    (let ((org-import-simplenote-title-format 'first-line))
      (org-import-simplenote--insert-note
       '((id . "3332bd3d9ecb42598c99cacc55773fc8")
         (content . "test\C-m
\C-m
content\C-m")
         (creationDate . "2021-10-25T05:30:51.000Z")
         (lastModified . "2021-10-30T11:18:14.594Z"))))
    (should (equal (test--remove-leading-spaces
                    (buffer-substring-no-properties (point-min) (point-max)))
                   (test--normalized-timestamp
                    "2021-10-25T05:30:51+0000"
                    "
* test
:PROPERTIES:
:created:  %s
:END:

content")))))

(ert-deftest org-import-simplenote--insert-note--timestamp ()
  (with-temp-buffer
    (org-mode)
    (let ((org-import-simplenote-title-format 'timestamp))
      (org-import-simplenote--insert-note
       '((id . "3332bd3d9ecb42598c99cacc55773fc8")
         (content . "test")
         (creationDate . "2021-10-25T05:30:51.000Z")
         (lastModified . "2021-10-30T11:18:14.594Z"))))
    (should (equal (test--remove-leading-spaces
                    (buffer-substring-no-properties (point-min) (point-max)))
                   (test--normalized-timestamp
                    "2021-10-25T05:30:51+0000"
                    "
* %s
:PROPERTIES:
:created:  %s
:END:

test")))))

(ert-deftest org-import-simplenote--format-title ()
  ;; The quasiquoting is to make the value NOT a constant list.
  ;; Since the Elisp linter *is* the byte compiler, as long as the byte compiler
  ;; doesn't complain this should actually resolve the problem.
  (let ((note `((id . ,"3332bd3d9ecb42598c99cacc55773fc8")
                (content . "test")
                (creationDate . "2021-10-25T05:30:51.000Z")
                (lastModified . "2021-10-30T11:18:14.594Z"))))
    (setcdr (assq 'creationDate note)
            (org-import-simplenote--normalize-timestamp
             (cdr (assq 'creationDate note))))
    (let ((org-import-simplenote-title-format 'timestamp))
      (should (equal (org-import-simplenote--normalize-timestamp
                      "2021-10-25T05:30:51+0000")
                     (org-import-simplenote--format-title note))))
    (let ((org-import-simplenote-title-format 'first-line))
      (should (equal "test"
                     (org-import-simplenote--format-title note))))
    (let ((org-import-simplenote-title-format 'both))
      (should (equal (format "%s %s"
                             (org-import-simplenote--normalize-timestamp
                              "2021-10-25T05:30:51+0000")
                             "test")
                     (org-import-simplenote--format-title note))))
    (let ((org-import-simplenote-title-format
           (lambda (note)
             (cdr (assq 'id note)))))
      (should (equal "3332bd3d9ecb42598c99cacc55773fc8"
                     (org-import-simplenote--format-title note))))))

(provide 'org-import-simplenote-test)

;;; org-import-simplenote-test.el ends here
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
