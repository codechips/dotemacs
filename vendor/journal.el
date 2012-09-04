(defvar org-journal-file "~/Dropbox/org/journal.org"
  "Path to OrgMode journal file.")

(defvar org-journal-date-format "%Y-%m-%d"
  "Date format string for journal headings.")

(defun journal ()
  "Create a new diary entry for today."
  (interactive)
  (switch-to-buffer (find-file org-journal-file))
  (widen)
  (let ((today (format-time-string org-journal-date-format)))
    (end-of-buffer)
    (unless (org-goto-local-search-headings today nil t)
      ((lambda ()
         (insert "\n")
         (org-insert-heading)
         (insert today)
         (insert "\n\n"))))
    (org-show-entry)
    (org-narrow-to-subtree)
    (end-of-buffer)
    (insert "  ")
    (set-fill-column 70)))


