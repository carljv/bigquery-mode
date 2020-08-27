;;; bigquery.el -- A mode for writing and running BiqQuery SQL queries.

;; 2020 Carl Vogel

;; Author: Carl Vogel <carljv@gmail.com>
;; Version 0.1
;; Keywords: SQL, BigQuery
;; URL: https://github.com/carljv/bigquery-el

;;; Comentary:

;; Major-mode conventions
;; - Derive from other mode with `define-derived-mode`
;;   - derive from `prog-mode` (instead of text or special or fundamental)
;;     - (defin-derived-mode bigquery-mode prog-mode "BigQuery" "Docstring" (...))
;;   - provide:
;;     - `bigquery-mode-map` at global/top-level
;;     - :syntax-table, :abbrev-table, :group, :after-hook
;;     
;; - name: bigquery-mode
;;
;; - function (bigquery-mode) 
;;   - idempotent,
;;   - docstring
;;     - \[COMMAND], \{KEYMAP}, \<KEYMAP>
;;   - starts with `kill-all-local-variables`
;;   - set `major-mode` to "bigquery-mode"
;;   - set `mode-name` to "BigQuery"
;;   - call `use-local-map` for keybindings
;;   - set buffer local vars for syntax, font-lock, eldoc, imenu, etc.
;;   - last thing: run hooks 
;;   - idempotent
;;
;; -  All global names prefixed w/ bigquery
;;
;; - auto-indentation
;;
;; - local keymap:
;;    - in bigquery-mode-map
;;    - begin with C-c C-[?]
;;    - can rebind M-n M-p M-s for "movement"
;;
;; - syntax table:
;;   - define in bigquery-mode-syntax-table
;;   - set variables for comment syntax
;;
;; - abbrev-table: bigquery-mode-abbrev-table 
;;
;; - Font-lock:
;;   - set buffer-local `font-lock-defaults`
;;   - faces should inherit from existing faces
;;
;; - Imenu:
;;   - identify how to find definitions & sections
;;   - buffer-locals:
;;      - `imenu-generic-expression`,
;;      - `imenu-prev-index-position-function` and
;;      - `imenu-extract-index-name-function` or
;;      - `imenu-create-index-function`
;;
;; - Eldoc:
;;   - Buffer-local `eldoc-documentation-function`
;;
;; - Completion:
;;   - `completion-at-point-functions`
;;
;; - Buffer-local customization vars
;;   - `make-local-variable` NOT `make-variable-buffer-local`
;;
;; - Hooks:
;;   - bigquery-mode-hook
;;
;; - If derived, call parent major-mode command
;;
;; - Auto-major mode: add element to `auto-mode-alist`
;;
;; - Top-level forms idempotent
;;
;; - Maybe use majic-mode-alist to check
;;   for bigquery comment at the top of the file.
;;   E.g.
;;   -- bigquery
;;   -- project: kaitain-dw


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

