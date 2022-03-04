(require 'ert)
(require 'org-import-simplenote)

(defun test--normalized-timestamp (timestamp format)
  "Normalize TIMESTAMP and format it with FORMAT."
  (format format (org-import-simplenote--normalize-timestamp timestamp)))

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
                    "2021-10-25T14:30:51+0900"
                    "
* %1$s
:PROPERTIES:
:created:  %1$s
:END:

test")))))

(provide 'org-import-simplenote-test)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
