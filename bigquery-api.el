(provide 'bigquery-api)

(require 'ivy)
(require 'json)
(require 'url-http)


;;; BigQuery API

(defconst bigquery/api-base-url
  "https://bigquery.googleapis.com/bigquery/v2")


(defun bigquery/api-handle-auth-and-call (api-fn &rest args)
  (let* ((token-info (bigquery/google-oauth2-refresh-access-and-store-token))
	 (token (cdr (assoc 'access_token token-info))))
    (when token (apply api-fn token args))))


(defun bigquery/api-make-api-call (token api-url-fn &rest args)
  (let* ((url (apply api-url-fn args))
	 (url-request-method "GET")
	 (url-request-extra-headers (bigquery/api-make-auth-header token))
	 (response-buffer (url-retrieve-synchronously url t)))
    (bigquery/api-json-parse-response-body response-buffer)))
    
			    
(defun bigquery/api-make-auth-header (token)
  (list (cons "Authorization" (format "Bearer %s" token))))


(defun bigquery/api-json-parse-response-body (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$")
    (let ((response-data (json-read)))
      (kill-buffer (current-buffer))
      response-data)))


;; REST API endpoint URLs
;; ----------------------

(defun bigquery/api-projects-list-url ()
  (concat bigquery/api-base-url
	  "/projects?max_results=1000"))


(defun bigquery/api-datasets-list-url (project-id)
  (concat bigquery/api-base-url
	  "/projects/" project-id
	  "/datasets?max_results=1000"))


(defun bigquery/api-tables-list-url (project-id dataset-id)
  (concat bigquery/api-base-url
	  "/projects/" project-id
	  "/datasets/" dataset-id
	  "/tables?max_results=1000"))


(defun bigquery/api-table-get-url (project-id dataset-id table-id)
  (concat bigquery/api-base-url
	  "/projects/" project-id
	  "/datasets/" dataset-id
	  "/tables/"   table-id))


;; List projects
;; -------------

(defun bigquery/api-noauth-projects-list (token)
  (bigquery/api-make-api-call token 'bigquery/api-projects-list-url))


(defun bigquery/api-projects-list ()
  (bigquery/api-handle-auth-and-call 'bigquery/api-noauth-projects-list))


(defun bigquery/api-get-project-ids-from-response (projects-list-response)
  (let* ((projects (alist-get 'projects projects-list-response))
	 (project-refs (mapcar (lambda (x) (alist-get 'projectReference x)) projects))
	 (project-ids (mapcar (lambda (x) (alist-get 'projectId x)) project-refs)))
      project-ids))


;; List datasets
;; -------------

(defun bigquery/api-noauth-datasets-list (token project-id)
  (bigquery/api-make-api-call token 'bigquery/api-datasets-list-url project-id))


(defun bigquery/api-datasets-list (project-id)
  (bigquery/api-handle-auth-and-call
   'bigquery/api-noauth-datasets-list project-id))


(defun bigquery/api-get-dataset-ids-from-response (datasets-list-response)
  (let* ((datasets (alist-get 'datasets datasets-list-response))
	 (dataset-refs (mapcar (lambda (x) (alist-get 'datasetReference x)) datasets))
	 (dataset-ids (mapcar (lambda (x) (alist-get 'datasetId x)) dataset-refs)))
      dataset-ids))


;; List tables
;; -----------

(defun bigquery/api-noauth-tables-list (token project-id dataset-id)
  (bigquery/api-make-api-call token
			      'bigquery/api-tables-list-url project-id dataset-id))

  
(defun bigquery/api-tables-list (project-id dataset-id)
  (bigquery/api-handle-auth-and-call
   'bigquery/api-noauth-tables-list project-id dataset-id))


(defun bigquery/api-get-table-ids-from-response (table-list-response)
  (let* ((tables (alist-get 'tables table-list-response))
	 (table-refs (mapcar (lambda (x) (alist-get 'tableReference x)) tables))
	 (table-ids (mapcar (lambda (x) (alist-get 'tableId x)) table-refs)))
      table-ids))

;; Get table schema
;; ----------------

(defun bigquery/api-noauth-table-get (token project-id dataset-id table-id)
  (bigquery/api-make-api-call token
			      'bigquery/api-table-get-url
			      project-id dataset-id table-id))


(defun bigquery/api-table-get (project-id dataset-id table-id)
  (bigquery/api-handle-auth-and-call
   'bigquery/api-noauth-table-get project-id dataset-id table-id))


(defun bigquery/api-get-field-names-from-response (table-get-response)
  (let* ((fields (alist-get 'fields (alist-get 'schema table-get-response)))
	 (names  (mapcar (lambda (x) (alist-get 'name x)) fields)))
    names))





    
