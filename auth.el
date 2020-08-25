;;; auth.el -- Functions for Google API Oauth

;;; Commentary


;;; Code

(require 'json)
(require 'url-http)

;;; Google OAuth2.0

(defconst bigquery/google-oauth2-app-secrets-file
  (expand-file-name "bigquery-el/.oauth-app-secrets" user-emacs-directory))

(defconst bigquery/google-oauth2-client-id
  (alist-get 'client_id (json-read-file bigquery/google-oauth2-app-secrets-file)))

(defconst bigquery/google-oauth2-client-secret
  (alist-get 'client_secret (json-read-file bigquery/google-oauth2-app-secrets-file)))

(defconst bigquery/google-oauth2-auth-url
  "https://accounts.google.com/o/oauth2/v2/auth")

(defconst bigquery/google-oauth2-token-url
  "https://www.googleapis.com/oauth2/v4/token")

(defconst bigquery/google-oauth2-scope
  "https://www.googleapis.com/auth/bigquery.readonly"
  "The package only reads from BigQuery datasets.")

(defconst bigquery/google-oauth2-token-cache
  (expand-file-name "bigquery-el/.bigquery-oauth" user-emacs-directory)
  "The cache file for token information.")


(defconst bigquery/google-oauth2-auth-url
  (concat bigquery/google-oauth2-auth-url
	  "?client_id="      (url-hexify-string bigquery/google-oauth2-client-id)
	  "&scope="         (url-hexify-string bigquery/google-oauth2-scope)
	  "&response_type=" "code"
	  "&redirect_uri="  "urn:ietf:wg:oauth:2.0:oob"
	  "&access_type="   "offline"))



(defun bigquery/google-oauth2-request-auth ()
  (browse-url bigquery/google-oauth2-auth-url)
  (read-string "Enter the code from your browser: "))
	  

(defun bigquery/google-oauth2-make-access-request-data ()
  (concat "client_id="      (url-hexify-string bigquery/google-oauth2-client-id)
	  "&client_secret=" bigquery/google-oauth2-client-secret
	  "&code="          (bigquery/google-oauth2-request-auth)
	  "&grant_type="    "authorization_code"
	  "&redirect_uri="  "urn:ietf:wg:oauth:2.0:oob"))
	

(defun bigquery/google-oauth2-parse-access-response ()
  (point-min)
  (json-read))


(defun bigquery/google-oauth2-request-access (request-data)
  (let ((url-request-method "POST")
	(url-request-data request-data)
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (with-current-buffer
	(url-retrieve-synchronously bigquery/google-oauth2-token-url) 
      (let ((response-data (bigquery/google-oauth2-parse-access-response)))
	(kill-buffer (current-buffer))
	response-data))))


(defun bigquery/google-oauth2-read-token-cache ()
  (when (file-readable-p bigquery/google-oauth2-token-cache)
    (let ((token-info (json-read-file bigquery/google-oauth2-token-cache)))
      (if (cdr (assoc 'access_token token-info))
	  token-info
	  (delete-file bigquery/google-oauth2-token-cache)))))


(defun bigquery/google-oauth2-request-access-and-store-token ()
  (let ((cached-token (bigquery/google-oauth2-read-token-cache)))
    (if cached-token cached-token
      (let* ((request-data (bigquery/google-oauth2-make-access-request-data))
	     (response-data (bigquery/google-oauth2-request-access request-data))
	     (response-data-json (json-encode response-data)))
	(with-temp-file bigquery/google-oauth2-token-cache
	  (insert response-data-json)
	  response-data)))))


(defun bigquery/google-oauth2-make-refresh-request-data (token-info)
  (let ((refresh-token (cdr (assoc 'refresh_token token-info))))
    (concat "client_id="      (url-hexify-string bigquery/google-oauth2-client-id)
	    "&client_secret=" (url-hexify-string bigquery/google-oauth2-client-secret)
	    "&refresh_token=" refresh-token
	    "&grant_type="    "refresh_token")))


(defun bigquery/google-oauth2-refresh-access-token ()
  (let ((token-info (bigquery/google-oauth2-read-token-cache)))
    (when token-info
      (let* ((request-data
	      (bigquery/google-oauth2-make-refresh-request-data token-info))
	     (response-data
	      (bigquery/google-oauth2-request-access request-data)))
	(cons (cons 'refresh_token (alist-get 'refresh_token token-info))
	      response-data))))) 


(defun bigquery/google-oauth2-refresh-access-and-store-token ()
  (let ((response-data (bigquery/google-oauth2-refresh-access-token)))
    (if response-data
     	(with-temp-file bigquery/google-oauth2-token-cache
	  (insert (json-encode response-data))
	  response-data)
      (bigquery/google-oauth2-request-access-and-store-token))))



;;; auth.el ends here

