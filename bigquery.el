;;; bigquery.el -- A mode for writing and running BiqQuery SQL queries.

;; 2020 Carl Vogel

;; Author: Carl Vogel <carljv@gmail.com>
;; Version 0.1
;; Keywords: SQL, BigQuery
;; URL: https://github.com/carljv/bigquery-el

;;; Comentary:


;;; Code:


(require 'bigquery-syntax)
(require 'bigquery-auth)
(require 'bigquery-api)
(require 'bigquery-schema)
(require 'bigquery-bq-cli)
(require 'bigquery-comint)
(require 'bigquery-R)


;; bigquery-mode
(progn 
  (setq bigquery-mode-map (make-sparse-keymap))
  (define-key bigquery-mode-map (kbd "C-c C-q") 'bigquery/bq-run-query)
  (define-key bigquery-mode-map (kbd "C-c C-z") 'bigquery/bq-shell-run)
  (define-key bigquery-mode-map (kbd "C-c C-c") 'bigquery/bq-shell-send-buffer-or-region)
  (define-key bigquery-mode-map (kbd "C-c C-r") 'bigquery/R-run-query)
  (define-key bigquery-mode-map (kbd "C-c C-s") 'bigquery/ivy-bigquery-search-schemas)
  (define-key bigquery-mode-map (kbd "C-c C-u") 'bigquery/toggle-keyword-case))

(setq bigquery-mode-syntax-table bigquery/syntax-table)

(define-derived-mode bigquery-mode prog-mode "BigQuery"
  (setq font-lock-defaults '(bigquery/syntax-font-lock-defaults nil t)))


;; bq-shell-mode
(define-derived-mode bq-shell-mode comint-mode "bq shell"
  "bq shell major mode"
  (setq comint-prompt-regexp bigquery/bq-shell-regexp)
  (setq comint-prompt-read-only t))


(provide 'bigquery)

;;; bigquery.el ends here

