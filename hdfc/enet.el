;;; enet --- generate enet CSV
;;; Commentary:
;;; Code:

(require 'org)
(require 'subr-x)

(defun enet-table-names ()
  "Return names of tables in the current buffer."
  (let (names)
    (org-table-map-tables
     (lambda ()
       (when-let ((name (org-element-property
                         :name (org-element-at-point))))
         (push name names)))
     'quiet)
    (nreverse names)))

(defun enet-table-at-point ()
  "Return the name of table at point."
  (and (org-at-table-p)
       (save-excursion
         (goto-char (org-table-begin))
         (org-element-property :name (org-element-at-point)))))

(defun enet-export (name)
  "Search for table named `NAME` and export.
ARG changes behaviour."
  (interactive (list (completing-read
                      "Table Name: "
                      (enet-table-names)
                      nil
                      t
                      (enet-table-at-point))))
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
