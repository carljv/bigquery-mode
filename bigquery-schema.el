(defun bigquery/schema-get-all ()
  (let ((projects (bigquery/api-get-project-ids-from-response
		   (bigquery/api-projects-list))))
    (mapcar 'bigquery/schema-get-project->datasets projects)))


(defun bigquery/schema-get-project->datasets (project-id)
  (let ((dataset-ids (bigquery/api-get-dataset-ids-from-response
		      (bigquery/api-datasets-list project-id))))
    (cons project-id
	  (mapcar
	   (lambda (ds)
	     (message ds)
	     (bigquery/schema-get-dataset->tables project-id ds))
	   dataset-ids))))
    

(defun bigquery/schema-get-dataset->tables (project-id dataset-id)
  (let ((table-ids (bigquery/api-get-table-ids-from-response
		    (bigquery/api-tables-list project-id dataset-id))))
    (cons dataset-id table-ids)))


(defun bigquery/schema-get-table->fields (project-id dataset-id table-id)
  (cons table-id (bigquery/api-get-field-names-from-response
		  (bigquery/api-table-get project-id dataset-id table-id))))

				  
;; Search schemas with ivy
;; -----------------------

(defvar bigquery/ivy-last-project nil)

(defvar bigquery/ivy-last-dataset nil)

(defvar bigquery/ivy-last-table nil)


(defun bigquery/ivy-bigquery-search-schemas ()
  (interactive)
  (bigquery/ivy-choose-project))


(defun bigquery/ivy-choose-project ()
  (interactive)
  (let ((projects (bigquery/schema-get-projects)))
   (bigquery/schema-cache-projects projects)
   (ivy-read "Choose a project: "
	     projects
	     :preselect
	     (when (seq-contains projects bigquery/ivy-last-project)
	       (concat "^" bigquery/ivy-last-project))
	     :action (lambda (prj) (bigquery/ivy-choose-dataset prj)))))


(defun bigquery/ivy-choose-dataset (project)
  (interactive)
  (setq bigquery/ivy-last-project project)
  (let ((datasets (bigquery/schema-get-datasets project)))
    (bigquery/schema-cache-datasets project datasets)
    (ivy-read (format "Choose a dataset in %s: " project)
	      datasets
	      :preselect
	      (when (seq-contains datasets bigquery/ivy-last-dataset 'string=)
		(concat "^" bigquery/ivy-last-dataset))
	      :action (lambda (ds) (bigquery/ivy-choose-table project ds)))))


(defun bigquery/ivy-choose-table (project dataset)
  (interactive)
  (setq bigquery/ivy-last-dataset dataset)
  (let ((tables (bigquery/schema-get-tables project dataset)))
    (bigquery/schema-cache-tables project dataset tables)
    (ivy-read (format "Choose a table in %s.%s: " project dataset) 
	      tables
	      :preselect
	      (when (seq-contains tables bigquery/ivy-last-table 'string=)
		(concat "^" bigquery/ivy-last-table))
	      :action (lambda (tbl)
			(bigquery/ivy-choose-field project dataset tbl)))))
  

(defun bigquery/ivy-choose-field (project dataset table)
  (interactive)
  (setq bigquery/ivy-last-table table)
  (let ((fields (bigquery/schema-get-fields project dataset table)))
    (bigquery/schema-cache-fields project dataset table fields)
    (ivy-read (format "Choose a field in %s.%s.%s: " project dataset table)
	      fields
	      :action (lambda (f) (insert (concat f " ")))
	      :multi-action (lambda (fs) (insert (mapconcat 'identity fs ", "))))))


(dolist (cmd '(bigquery/ivy-choose-project
	       bigquery/ivy-choose-dataset
	       bigquery/ivy-choose-table))
  (ivy-set-actions cmd '(("i" insert "insert"))))


(defvar bigquery/schema-cached-projects nil)

(defvar bigquery/schema-cached-datasets nil)

(defvar bigquery/schema-cached-tables nil)

(defvar bigquery/schema-cached-fields nil)


(defun bigquery/schema-cache-projects (projects)
  (setq bigquery/schema-cached-projects projects))


(defun bigquery/schema-cache-datasets (project datasets)
  (when (not (assoc project bigquery/schema-cached-datasets))
    (setq bigquery/schema-cached-datasets
	  (cons (cons project datasets) bigquery/schema-cached-datasets))))


(defun bigquery/schema-cache-tables (project dataset tables)
  ;; If the project isn't in this cache,
  ;; cons the project -> dataset -> tables mapping
  ;; into the cache
  (if (not (assoc project bigquery/schema-cached-tables))
      (setq bigquery/schema-cached-tables
	    (cons (cons project (cons dataset tables))
		  bigquery/schema-cached-tables))
    ;; If the project is in the cache, but the dataset isn't,
    ;; cons the dataset -> tables mapping into the project
    (when (not (assoc dataset (cdr (assoc project bigquery/schema-cached-tables))))
      (setf (cdr (assoc project bigquery/schema-cached-tables))
	    (cons (cons dataset tables)
		  (cdr (assoc project bigquery/schema-cached-tables)))))))


(defun bigquery/schema-cache-fields (project dataset table fields)
  ;; If the project isn't in this cache
  ;; cons the project -> dataset -> table -> fields mapping
  ;; into the cache  
  (if (not (assoc project bigquery/schema-cached-fields))
      (setq bigquery/schema-cached-fields
	    (cons (cons project (cons dataset (cons table fields)))
		  bigquery/schema-cached-fields))
    ;; If the project is in the cache, but the dataset isn't,
    ;; cons the dataset -> table -> fields mapping into the project
    (if (not (assoc dataset (cdr (assoc project bigquery/schema-cached-fields))))
	(setf (cdr (assoc project bigquery/schema-cached-fields))
	      (cons (cons dataset (cons table fields))
		    (cdr (assoc project bigquery/schema-cached-tables))))
      ;; If the project and dataset are in the 
      (when (not (assoc table (cdr
			     (assoc dataset
				    (cdr (assoc project bigquery/schema-cached-fields))))))
	(setf (cdr (assoc dataset (cdr (assoc project bigquery/schema-cached-fields))))
	      (cons (cons table fields)
		    (cdr (assoc dataset (cdr (assoc project bigquery/schema-cached-fields))))))))))
	  
		  
(defun bigquery/schema-get-projects ()
  (or bigquery/schema-cached-projects
      (bigquery/api-get-project-ids-from-response
       (bigquery/api-projects-list))))


(defun bigquery/schema-get-datasets (project)
  (or (cdr (assoc project bigquery/schema-cached-datasets))
      (bigquery/api-get-dataset-ids-from-response
       (bigquery/api-datasets-list project))))


(defun bigquery/schema-get-tables (project dataset)
  (or (cdr (assoc dataset (cdr (assoc project bigquery/schema-cached-tables))))
      (bigquery/api-get-table-ids-from-response
       (bigquery/api-tables-list project dataset))))


(defun bigquery/schema-get-fields (project dataset table)
  (or (cdr (assoc table
		  (cdr (assoc dataset
			      (cdr (assoc project
					  bigquery/schema-cached-fields))))))
      (bigquery/api-get-field-names-from-response
       (bigquery/api-table-get project dataset table))))
	


(provide 'bigquery-schema)

;;; bigquery-schema.el ends here
