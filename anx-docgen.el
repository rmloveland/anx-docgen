;;; ANX-Docgen

(defvar *an-stack* nil
  "The stack where we stash our second-pass field definitions.")

(defun an-alistp (object)
  ;; Object -> Boolean
  (and (listp object)
       (every #'consp object)))

(defun an-get-alist-keys (alist)
  ;; Alist -> List
  (if (an-alistp alist)
      (mapcar (lambda (elem)
		(car elem))
	      alist)
    nil))

(defun an-array-of-alistsp (array)
  ;; Object -> Boolean
  (and (arrayp array)
       (an-alistp (elt array 0))))

(defun an-assoc-val (key alist)
  ;; Symbol Alist -> Object
  (let ((result (assoc key alist)))
    (if result
	(cdr result)
      result)))

(defun an-get-random-alist (array-of-alists)
  ;; Array of Alists -> Alist
  (let* ((len (length array-of-alists))
	 (rand (random len)))
    (elt array-of-alists rand)))

(defun an-stack-push (item)
  ;; Item -> State!
  (push item *an-stack*))

(defun an-stack-pop ()
  ;; State!
  (pop *an-stack*))

(defun an-stack-emptyp ()
  (null *an-stack*))

(defun an-clear-stack ()
  ;; State!
  (progn (setq *an-stack* nil)))

(defun an-translate-boolean (symbol)
  ;; Symbol -> String
  (if (equal symbol :json-false)
      "No"
    "Yes"))

(defun an-print-object-standard-fields (object)
  ;; Alist -> IO
  (let* ((fields (an-list-object-standard-fields object)))
    (format *an-table-row*
	    (pop fields)
	    (pop fields)
	    (pop fields)
	    (pop fields))))

(defun an-list-object-standard-fields (object)
  ;; Alist -> List
  (list
   (an-assoc-val 'name object)
   (an-assoc-val 'type object)
   (an-translate-boolean (an-assoc-val 'sort_by object))
   (an-translate-boolean (an-assoc-val 'filter_by object))))

(defun an-object-has-fieldsp (object)
  ;; Alist -> Boolean
  (if (assoc 'fields object)
      t
    nil))

(defun an-save-fields-for-later (object)
  ;; Alist -> State!
  (let ((name (an-assoc-val 'name object)))
    (an-stack-push (cons name (an-assoc-val 'fields object)))))

(defun an-process-object (object)
  ;; Alist -> IO State!
  (progn
    (an-print-to-scratch-buffer
     (an-print-object-standard-fields object))
    (if (an-object-has-fieldsp object)
	(an-save-fields-for-later object))))

(defun an-process-objects (array-of-alists)
  ;; Array -> IO State!
  (an-print-to-scratch-buffer
   (format "\nh4. JSON Fields\n\n"))
  (an-print-to-scratch-buffer
   (format *an-table-header*))
  (mapc (lambda (object)
	  (an-process-object object))
	array-of-alists))

(defvar *an-table-header*
  "|| Name || Type || Sort By? || Filter By? || Description || Default || Required On ||\n")

(defvar *an-table-row*
  "| %s | %s | %s | %s | | | |\n")

(defun an-print-to-scratch-buffer (format-string)
  (princ format-string
	 (get-buffer "*scratch*")))

(defun an-process-stack-item (list)
  ;; List -> IO State!
  (let ((array-of-alists (cdr list))
	(name (capitalize (car list))))
    (an-print-to-scratch-buffer
     (format "\nh4. %s\n\n" name))
    (an-print-to-scratch-buffer
     (format *an-table-header*))
    (mapc (lambda (object)
	    ;; Nothing should have fields at this level (I hope).
	    (an-process-object object))
	  array-of-alists)))

(defun an-process-stack-items ()
  ;; -> IO State!
  (while (not (an-stack-emptyp))
    (an-process-stack-item (an-stack-pop))))

(defun an-print-meta (array-of-alists)
  ;; Array -> IO State!
  (if (an-array-of-alistsp array-of-alists)
      (progn
	(an-clear-stack)
	(an-process-objects array-of-alists)
	(an-process-stack-items))
    (error "`an-print-meta' expects an array of association lists.")))

(defun an-really-print-meta ()
  ;; -> IO State!
  (interactive)
  (let ((array-of-alists (read (buffer-string))))
    (an-print-meta array-of-alists)))

;;; Reporting.

(defvar *an-dimensions-table-header*
  "|| Column || Type || Filter? || Description ||\n")

(defvar *an-metrics-table-header*
  "|| Column || Type || Description ||\n")

(defvar *an-havings-hash* (make-hash-table :test 'equal)) ; -> Exists?
(defvar *an-filters-hash* (make-hash-table :test 'equal)) ; -> Exists?
(defvar *an-columns-hash* (make-hash-table :test 'equal)) ; -> Type

(defun an-build-columns-hash (report-meta-alist)
  ;; Array -> State!
  (mapc (lambda (alist) 
	  (puthash (an-assoc-val 'column alist) 
		   (an-assoc-val 'type alist) 
		   *an-columns-hash*))
	(an-assoc-val 'columns report-meta-alist)))

(defun an-build-filters-hash (report-meta-alist)
  ;; Array -> State!
  (mapc (lambda (alist) 
	  (puthash (an-assoc-val 'column alist) 
		   (an-assoc-val 'type alist)
		   *an-filters-hash*))
	(an-assoc-val 'filters report-meta-alist)))

(defun an-build-havings-hash (report-meta-alist)
  ;; Array -> State!
  (mapc (lambda (alist) 
	  (puthash (an-assoc-val 'column alist) t *an-havings-hash*))
	(an-assoc-val 'havings report-meta-alist)))

(defun an-print-column-standard-fields (alist)
  ;; Alist -> IO
  (format "| %s | %s |\n"
	  (an-assoc-val 'column alist)
	  (an-assoc-val 'type alist)))

(defun add-type-if-exists (key type)
  ;; Symbol String -> IO
  (if (gethash key metric-hash-table)
      (puthash key type metric-hash-table)))

;; anx-docgen.el ends here.
