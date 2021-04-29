;;; enet --- generate enet CSV
;;; Commentary:
;;; Code:

(require 'org)

(defun enet-export (name)
  "Search for table named `NAME` and export.
ARG changes behaviour."
  (interactive "MTable Name: ")
  (outline-show-all)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
        (progn
          (forward-line)
          (let* ((beg (org-table-begin))
                 (end (org-table-end))
                 (txt (buffer-substring-no-properties beg end))
                 (file (org-entry-get (point) "TABLE_EXPORT_FILE"))
                 (format (org-entry-get (point) "TABLE_EXPORT_FORMAT")))
            (with-temp-buffer
              (insert txt)
              (goto-char (point-min))
              (org-table-next-field)
              (org-table-delete-column)
              (goto-char (org-table-end))
              (backward-char)
              (org-table-kill-row)
              (org-table-kill-row)
              (org-entry-put (point) "TABLE_EXPORT_FILE" file)
              (org-entry-put (point) "TABLE_EXPORT_FORMAT" format)
              (make-directory (file-name-directory file) t)
              (org-table-export))))
      (message "Not Found"))))

(provide 'enet)
;;; enet.el ends here
