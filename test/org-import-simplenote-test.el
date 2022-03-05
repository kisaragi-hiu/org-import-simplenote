(require 'ert)
(require 'org-import-simplenote)

(defun test--remove-leading-spaces (str)
  "Remove leading spaces from STR."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (while (re-search-backward (rx bol (+? " ") ":") nil t)
        (replace-match ""))
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

(ert-deftest org-import-simplenote--insert-note ()
  (with-temp-buffer
    (org-mode)
    (org-import-simplenote--insert-note
     '((id . "3332bd3d9ecb42598c99cacc55773fc8")
       (content . "test")
       (creationDate . "2021-10-25T05:30:51.000Z")
       (lastModified . "2021-10-30T11:18:14.594Z")))
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
  (let ((note '((id . "3332bd3d9ecb42598c99cacc55773fc8")
                (content . "test")
                (creationDate . "2021-10-25T05:30:51.000Z")
                (lastModified . "2021-10-30T11:18:14.594Z"))))
    (setf (alist-get 'creationDate note)
          (org-import-simplenote--normalize-timestamp
           (alist-get 'creationDate note)))
    (let ((org-import-simplenote-title-format 'timestamp))
      (should (equal (org-import-simplenote--normalize-timestamp
                      "2021-10-25T14:30:51+0900")
                     (org-import-simplenote--format-title note))))
    (let ((org-import-simplenote-title-format 'first-line))
      (should (equal "test"
                     (org-import-simplenote--format-title note))))
    (let ((org-import-simplenote-title-format 'both))
      (should (equal (format "%s %s"
                             (org-import-simplenote--normalize-timestamp
                              "2021-10-25T14:30:51+0900")
                             "test")
                     (org-import-simplenote--format-title note))))
    (let ((org-import-simplenote-title-format
           (lambda (note)
             (alist-get 'id note))))
      (should (equal "3332bd3d9ecb42598c99cacc55773fc8"
                     (org-import-simplenote--format-title note))))))

(provide 'org-import-simplenote-test)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
