;;; org-import-simplenote.el --- Import Simplenote notes into Org -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://github.com/kisaragi-hiu/org-import-simplenote

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; commentary

;;; Code:

(require 'parse-time)

(require 'org)

(defun org-import-simplenote--normalize-timestamp (timestamp)
  "Normalize TIMESTAMP to the current timezone and remove subsecond precision."
  (format-time-string "%FT%T%z"
                      (parse-iso8601-time-string timestamp)))

;; Returning a string to be inserted in the main function would be
;; cleaner. However, we have to rely on `org-set-tags' and
;; `org-set-property', and it is unnecessarily expensive to create a
;; new temporary buffer, switch to org mode, insert our text, then
;; return the string *for every single note*.
(defun org-import-simplenote--insert-note (note)
  "Convert NOTE into Org text then insert it.

This assumes we're in `org-mode'."
  (let-alist note
    (let ((date-in-current-timezone
           ;; This loses the subsecond portion, but I don't
           ;; really care.
           (org-import-simplenote--normalize-timestamp .creationDate)))
      (insert "\n* " date-in-current-timezone)
      ;; just run this as we're still on the heading
      (when (> (length .tags) 0)
        (org-set-tags (cl-coerce .tags 'list)))
      (org-set-property "created" date-in-current-timezone)
      ;; Make sure we don't insert before the property drawer
      (goto-char (point-max))
      (insert "\n"
              ;; Simplenote entries seem to be stored with CRLF.
              ;; Delete the CR characters.
              (replace-regexp-in-string (regexp-quote "") ""
                                        .content)))))

;;;###autoload
(defun org-import-simplenote (archive)
  "Convert Simplenote ARCHIVE to Org headings.

Insert those headings into the end of the current buffer.

ARCHIVE is either the exported zip file, or the JSON
representation located in source/notes.json in the zip file. When
given a zip file, it will first be unzipped with 7z.

Simplenote exports contain notes as individual files, but the
JSON file contains more information such as creation timestamp.

All active notes (not trashed) are inserted as Org headings at
the end the current buffer."
  (interactive "fSimplenote notes.zip: ")
  (save-excursion
    (let ((parsed-json (if (string= (file-name-extension archive) "zip")
                           (let* ((tmp-dir (file-name-as-directory
                                            (expand-file-name
                                             (format "emacs%s"
                                                     (random 10000))
                                             temporary-file-directory))))
                             (unwind-protect
                                 (progn
                                   (make-directory tmp-dir :parents)
                                   (let ((default-directory tmp-dir))
                                     (call-process "7z" nil nil nil "x" archive)
                                     (json-read-file
                                      (expand-file-name "notes.json" "source"))))
                               (when (file-exists-p tmp-dir)
                                 (delete-directory tmp-dir t))))
                         (json-read-file archive))))
      (let-alist parsed-json
        (goto-char (point-max))
        (cl-loop for note being the elements of .activeNotes
                 do (org-import-simplenote--insert-note note))
        (insert "\n")))))

(provide 'org-import-simplenote)

;;; org-import-simplenote.el ends here
