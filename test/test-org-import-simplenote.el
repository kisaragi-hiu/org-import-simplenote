(require 'ert)
(require 'org-import-simplenote)
(ert-deftest org-import-simplenote--insert-note ()
  (with-temp-buffer
    (org-mode)
    (org-import-simplenote--insert-note
     '((id . "3332bd3d9ecb42598c99cacc55773fc8")
       (content . "test")
       (creationDate . "2021-10-25T05:30:51.000Z")
       (lastModified . "2021-10-30T11:18:14.594Z")))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "
* 2021-10-25T14:30:51+0900
:PROPERTIES:
:created:  2021-10-25T14:30:51+0900
:END:

test"))))

(provide 'test-org-import-simplenote)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
