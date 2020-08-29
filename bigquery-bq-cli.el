(defvar bigquery/bq-command "bq"
  "Path to the bq command line utility.")

(defvar bigquery/bq-default-project nil
  "Default project for the bq command line utility to use with queries.")


(defun bigquery/bq-run-query (&optional ask-for-project)
  (interactive "P")
  (if ask-for-project
      (let ((project (read-string "Bigquery Project: "
				  bigquery/bq-default-project)))
	(bigquery/bq-run-query-internal project))
    (bigquery/bq-run-query-internal bigquery/bq-default-project)))


(defun bigquery/bq-run-query-internal (project)  
  (when (not bigquery/bq-default-project)
    (setq bigquery/bq-default-project project))
  (let* ((qry (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max))))
	 (qry (bigquery/bq-clean-query-text qry)))
    (with-current-buffer-window
     "*bq query results*" nil nil 
     (call-process bigquery/bq-command nil "*bq query results*" t
		   ;; Args to bq
		   "query"
		   (format "--project_id=%s" project)
		   "--nouse_legacy_sql"
		   "-q"
		   (format "%s" qry)))))


(defun bigquery/bq-clean-query-text (qry)
  (with-temp-buffer 
    (sql-mode)
    (insert qry)
    (goto-char (point-min))
    (let (kill-ring)
      (comment-kill (count-lines (point-min) (point-max))))
    (goto-char (point-min))
    (flush-lines "^[[:space:]]*$")
    (buffer-substring-no-properties (point-min) (point-max))))



(provide 'bigquery-bq-cli)


;;; bigquery-bq-cli.el ends here
