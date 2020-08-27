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
(require 'bigquery-r)


(progn 
  (setq bigquery-mode-map (make-sparse-keymap))
  (define-key bigquery-mode-map (kbd "C-c C-c") 'bigquery/bq-run-query)
  (define-key bigquery-mode-map (kbd "C-c C-r") 'bigquery/R-run-query)
  (define-key bigquery-mode-map (kbd "C-c C-s") 'bigquery/ivy-bigquery-search-schemas))


(define-derived-mode bigquery-mode prog-mode "BigQuery"
  (setq font-lock-defaults '(bigquery/syntax-font-lock-defaults nil t))
  (setq bigquery-mode-syntax-table bigquery/syntax-table))


;;; bigquery.el ends here

