;;; ANX-Docgen

(defvar *an-stack* nil
  "The stack where we stash our second-pass field definitions.")

(defun an-alistp (alist)
  ;; Object -> Boolean
  "Determine if ALIST is an association list."
  (and (listp alist)
       (every #'consp alist)))

(defun an-get-alist-keys (alist)
  ;; Alist -> List
  "If ALIST is an association list, return a list of its keys."
  (if (an-alistp alist)
      (mapcar (lambda (elem)
		(car elem))
	      alist)
    nil))

(defun an-array-of-alistsp (array)
  ;; Object -> Boolean
  "Determine if ARRAY is an array of association lists."
  (and (arrayp array)
       (an-alistp (elt array 0))))

(defun an-assoc-val (key alist)
  ;; Symbol Alist -> Object
  "Given KEY and ALIST, return the value referenced by KEY.
Unlike `assoc', this function does not return the entire
key-value pair."
  (let ((result (assoc key alist)))
    (if result
	(cdr result)
      result)))

(defun an-get-random-alist (array-of-alists)
  ;; Array of Alists -> Alist
  "Given an ARRAY-OF-ALISTS, return a random association list."
  (let* ((len (length array-of-alists))
	 (rand (random len)))
    (elt array-of-alists rand)))

(defun an-stack-push (item)
  ;; Item -> State!
  "Push ITEM onto the global `*an-stack*'."
  (push item *an-stack*))

(defun an-stack-pop ()
  ;; State!
  "Pop an item off of the global `*an-stack*'."
  (pop *an-stack*))

(defun an-stack-emptyp ()
  ;; -> Boolean
  "Check if the global `*an-stack*' is empty."
  (null *an-stack*))

(defun an-clear-stack ()
  ;; State!
  "Clear the contents of the global `*an-stack*'."
  (progn (setq *an-stack* nil)))

(defun an-translate-boolean (symbol)
  ;; Symbol -> String
  "Given a SYMBOL with a `false' boolean value in JSON, return ``No''.
Otherwise, return ``Yes''."
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
  "\n|| Column || Type || Filter? || Description ||\n")

(defvar *an-metrics-table-header*
  "\n|| Column || Type || Formula || Description ||\n")

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

(defun an-build-dimensions-list ()
  ;; -> List
  (let ((results nil))
    (maphash (lambda (k v) 
	       (unless (gethash k *an-havings-hash*)
		 (push k results)))
	     *an-columns-hash*)
    (reverse results)))

(defun an-build-metrics-list ()
  ;; -> List
  (let ((results nil))
    (maphash (lambda (k v) 
		 (push k results))
	     *an-havings-hash*)
    (reverse results)))

(defun an-build-time-intervals-list (report-meta-alist)
  ;; Alist -> List
  (mapcar (lambda (x) x)
	  (an-assoc-val 'time_intervals network-meta)))

(defun an-get-dimension-type (dimension)
  ;; String -> String
  (if (an-dimensionp dimension)
      (gethash dimension *an-columns-hash*)
    (error (format "%s is not a dimension!" dimension))))

(defun an-get-metric-type (metric)
  ;; String -> String
  (if (an-metricp metric)
      (gethash metric *an-havings-hash*)
    (error (format "%s is not a metric!" metric))))

(defun an-dimensionp (dimension)
  ;; String -> Boolean
  (member dimension (an-build-dimensions-list)))

(defun an-metricp (metric)
  ;; String -> Boolean
  (gethash metric *an-havings-hash*))

(defun an-print-report-meta (report-meta)
  ;; Array -> IO State!
  (progn
    (an-build-columns-hash report-meta)
    (an-build-filters-hash report-meta)
    (an-build-havings-hash report-meta)
    ;; (an-print-time-intervals report-meta)
    (an-print-dimensions-table)
    (an-print-metrics-table)
    (an-clear-hashes)))

(defun an-really-print-report-meta ()
  ;; -> IO State!
  (interactive)
  (let ((report-meta (read (buffer-string))))
    (an-print-report-meta report-meta)))

(defun an-print-time-intervals (report-meta)
  (an-build-time-intervals-list report-meta))

(defun an-print-dimensions-table ()
  ;; Array -> IO State!
  (progn 
    (an-print-to-scratch-buffer *an-dimensions-table-header*)
    (mapcar (lambda (elem)
	      (an-print-to-scratch-buffer (format "| %s | %s | %s | |\n" elem 
						  (gethash elem *an-columns-hash*)
						  (if (gethash elem *an-filters-hash*)
						      "Yes"
						    "No"))))
	    (an-build-dimensions-list))))

(defun an-print-metrics-table ()
  ;; Array -> IO State!
  (progn
    (an-print-to-scratch-buffer *an-metrics-table-header*)
    (mapcar (lambda (elem)
	      (an-print-to-scratch-buffer (format "| %s | %s | | |\n" elem (gethash elem *an-columns-hash*))))
	    (an-build-metrics-list))))

(defun an-clear-hashes ()
  ;; -> State!
  (progn (clrhash *an-havings-hash*)
	 (clrhash *an-columns-hash*)
	 (clrhash *an-filters-hash*)))

;;; Working with existing documentation

(defun anx-split-line-string (line-string)
  ;; String -> List
  (split-string line-string "|" t))

(defun anx-join-line-list (line-list)
  ;; List -> String
  (mapconcat (lambda (x) x) 
	     (mapcar (lambda (box) box) 
		     line-list)
	     "|"))

(defun anx-build-alist (keys vals)
  ;; List List -> Alist
  (anx-build-alist-aux keys vals '()))

(defun anx-build-alist-aux (keys vals results)
  ;; List List List -> Alist
  (cond ((null keys) results)
	(t
	 (let* ((key (car keys))
		(val (car vals))
		(rest-keys (cdr keys))
		(rest-vals (cdr vals))
		(kv 
		 (if (null rest-keys)
		     (cons key (mapconcat (lambda (x) x) (cons val rest-vals) "|"))
		   (cons key val))))
	   (anx-build-alist-aux rest-keys rest-vals (cons kv results))))))

;; anx-docgen.el ends here.
