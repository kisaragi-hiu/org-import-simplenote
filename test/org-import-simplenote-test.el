(require 'ert)
(require 'org-import-simplenote)

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
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   (test--normalized-timestamp
                    "2021-10-25T05:30:51+0000"
                    "
* %s
  :PROPERTIES:
  :created:  %s
  :END:

test")))))

(provide 'org-import-simplenote-test)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
