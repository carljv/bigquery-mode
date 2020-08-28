
(defun bigquery/bq-shell-args (project)
  (list (format "--project_id=%s" project)
	"shell"))


(defvar bigquery/bq-shell-regexp "^[A-Za-z0-9_\\-]+>"
  "Prompt for `run-bq-shell'.")


(defun bigquery/bq-shell-initialize ()
  "Helper function to initialize Cassandra"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))


(defun bigquery/bq-shell-run (project)
  "Run an inferior instance of `bq shell' inside Emacs."
  (interactive "sProject: ")
  (let* ((bq-shell-program bigquery/bq-command)
         (buffer (when (comint-check-proc "*bq shell*") "*bq shell*")))
    ;; pop to the "*bq shell*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer (get-buffer-create "*bq shell*"))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "bq shell" buffer
             bigquery/bq-command nil (bigquery/bq-shell-args project))
      (bq-shell-mode))))


(defun bigquery/bq-shell-send-buffer-or-region ()
  (interactive)
  (when (and (string= major-mode "bigquery-mode")
	     (comint-check-proc "*bq shell*"))
    (let* ((qry (if (region-active-p)
		    (buffer-substring-no-properties (region-beginning) (region-end))
		  (buffer-substring-no-properties (point-min) (point-max))))
	   (qry (bigquery/bq-clean-query-text qry))
	   (qry (format "query --use_legacy_sql=false \"%s\"\n"
		        (replace-regexp-in-string "\n" " " qry))))
      (comint-send-string "*bq shell*" qry))))
	       


