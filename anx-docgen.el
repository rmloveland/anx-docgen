;;; ANX-Docgen  -*- lexical-binding: t -*-

(defvar *anx-stack* nil
  "The stack where we stash our second-pass field definitions.")

(defun anx-alistp (alist)
  ;; Object -> Boolean
  "Determine if ALIST is an association list."
  (and (listp alist)
       (every #'consp alist)))

(defun anx-get-alist-keys (alist)
  ;; Alist -> List
  "If ALIST is an association list, return a list of its keys."
  (if (anx-alistp alist)
      (mapcar (lambda (elem)
		(car elem))
	      alist)
    nil))

(defun anx-array-of-alistsp (array)
  ;; Object -> Boolean
  "Determine if ARRAY is an array of association lists."
  (and (arrayp array)
       (anx-alistp (elt array 0))))

(defun anx-assoc-val (key alist)
  ;; Symbol Alist -> Object
  "Given KEY and ALIST, return the value referenced by KEY.
Unlike `assoc', this function does not return the entire
key-value pair."
  (let ((result (assoc key alist)))
    (if result
	(cdr result)
      result)))

(defun anx-get-random-alist (array-of-alists)
  ;; Array of Alists -> Alist
  "Given an ARRAY-OF-ALISTS, return a random association list."
  (let* ((len (length array-of-alists))
	 (rand (random len)))
    (elt array-of-alists rand)))

(defun anx-stack-push (item)
  ;; Item -> State!
  "Push ITEM onto the global `*anx-stack*'."
  (push item *anx-stack*))

(defun anx-stack-pop ()
  ;; State!
  "Pop an item off of the global `*anx-stack*'."
  (pop *anx-stack*))

(defun anx-stack-emptyp ()
  ;; -> Boolean
  "Check if the global `*anx-stack*' is empty."
  (null *anx-stack*))

(defun anx-clear-stack ()
  ;; State!
  "Clear the contents of the global `*anx-stack*'."
  (progn (setq *anx-stack* nil)))

(defun anx-translate-boolean (symbol)
  ;; Symbol -> String
  "Given a SYMBOL with a `false' boolean value in JSON, return ``No''.
Otherwise, return ``Yes''."
  (if (equal symbol :json-false)
      "No"
    "Yes"))

(defun anx-print-object-standard-fields (object)
  ;; Alist -> IO
  (let* ((fields (anx-list-object-standard-fields object)))
    (format *anx-table-row*
	    (pop fields)
	    (pop fields)
	    (pop fields)
	    (pop fields))))

(defun anx-list-object-standard-fields (object)
  ;; Alist -> List
  (list
   (anx-assoc-val 'name object)
   (anx-assoc-val 'type object)
   (anx-translate-boolean (anx-assoc-val 'sort_by object))
   (anx-translate-boolean (anx-assoc-val 'filter_by object))))

(defun anx-object-has-fieldsp (object)
  ;; Alist -> Boolean
  (if (assoc 'fields object)
      t
    nil))

(defun anx-save-fields-for-later (object)
  ;; Alist -> State!
  (let ((name (anx-assoc-val 'name object)))
    (anx-stack-push (cons name (anx-assoc-val 'fields object)))))

(defun anx-process-object (object)
  ;; Alist -> IO State!
  (progn
    (anx-print-to-scratch-buffer
     (anx-print-object-standard-fields object))
    (if (anx-object-has-fieldsp object)
	(anx-save-fields-for-later object))))

(defun anx-process-objects (array-of-alists)
  ;; Array -> IO State!
  (anx-print-to-scratch-buffer
   (format "\nh4. JSON Fields\n\n"))
  (anx-print-to-scratch-buffer
   (format *anx-table-header*))
  (mapc (lambda (object)
	  (anx-process-object object))
	array-of-alists))

(defvar *anx-table-header*
  "|| Name || Type || Sort By? || Filter By? || Description || Default || Required On ||\n")

(defvar *anx-table-row*
  "| %s | %s | %s | %s | | | |\n")

(defun anx-print-to-scratch-buffer (format-string)
  (princ format-string
	 (get-buffer "*scratch*")))

(defun anx-process-stack-item (list)
  ;; List -> IO State!
  (let ((array-of-alists (cdr list))
	(name (capitalize (car list))))
    (anx-print-to-scratch-buffer
     (format "\nh4. %s\n\n" name))
    (anx-print-to-scratch-buffer
     (format *anx-table-header*))
    (mapc (lambda (object)
	    ;; Nothing should have fields at this level (I hope).
	    (anx-process-object object))
	  array-of-alists)))

(defun anx-process-stack-items ()
  ;; -> IO State!
  (while (not (anx-stack-emptyp))
    (anx-process-stack-item (anx-stack-pop))))

(defun anx-print-meta (array-of-alists)
  ;; Array -> IO State!
  (if (anx-array-of-alistsp array-of-alists)
      (progn
	(anx-clear-stack)
	(anx-process-objects array-of-alists)
	(anx-process-stack-items))
    (error "`anx-print-meta' expects an array of association lists.")))

(defun anx-really-print-meta ()
  ;; -> IO State!
  (interactive)
  (let ((array-of-alists (read (buffer-string))))
    (anx-print-meta array-of-alists)))

;;; Reporting.

(defvar *anx-dimensions-table-header*
  "\n|| Column || Type || Filter? || Description ||\n")

(defvar *anx-metrics-table-header*
  "\n|| Column || Type || Formula || Description ||\n")

(defvar *anx-havings-hash* (make-hash-table :test 'equal)) ; -> Exists?
(defvar *anx-filters-hash* (make-hash-table :test 'equal)) ; -> Exists?
(defvar *anx-columns-hash* (make-hash-table :test 'equal)) ; -> Type

(defun anx-build-columns-hash (report-meta-alist)
  ;; Array -> State!
  (mapc (lambda (alist) 
	  (puthash (anx-assoc-val 'column alist) 
		   (anx-assoc-val 'type alist) 
		   *anx-columns-hash*))
	(anx-assoc-val 'columns report-meta-alist)))

(defun anx-build-filters-hash (report-meta-alist)
  ;; Array -> State!
  (mapc (lambda (alist) 
	  (puthash (anx-assoc-val 'column alist) 
		   (anx-assoc-val 'type alist)
		   *anx-filters-hash*))
	(anx-assoc-val 'filters report-meta-alist)))

(defun anx-build-havings-hash (report-meta-alist)
  ;; Array -> State!
  (mapc (lambda (alist) 
	  (puthash (anx-assoc-val 'column alist) t *anx-havings-hash*))
	(anx-assoc-val 'havings report-meta-alist)))

(defun anx-print-column-standard-fields (alist)
  ;; Alist -> IO
  (format "| %s | %s |\n"
	  (anx-assoc-val 'column alist)
	  (anx-assoc-val 'type alist)))

(defun anx-build-dimensions-list ()
  ;; -> List
  (let ((results nil))
    (maphash (lambda (k v) 
	       (unless (gethash k *anx-havings-hash*)
		 (push k results)))
	     *anx-columns-hash*)
    (reverse results)))

(defun anx-build-metrics-list ()
  ;; -> List
  (let ((results nil))
    (maphash (lambda (k v) 
		 (push k results))
	     *anx-havings-hash*)
    (reverse results)))

(defun anx-build-time-intervals-list (report-meta-alist)
  ;; Alist -> List
  (mapcar (lambda (x) x)
	  (anx-assoc-val 'time_intervals network-meta)))

(defun anx-get-dimension-type (dimension)
  ;; String -> String
  (if (anx-dimensionp dimension)
      (gethash dimension *anx-columns-hash*)
    (error (format "%s is not a dimension!" dimension))))

(defun anx-get-metric-type (metric)
  ;; String -> String
  (if (anx-metricp metric)
      (gethash metric *anx-havings-hash*)
    (error (format "%s is not a metric!" metric))))

(defun anx-dimensionp (dimension)
  ;; String -> Boolean
  (member dimension (anx-build-dimensions-list)))

(defun anx-metricp (metric)
  ;; String -> Boolean
  (gethash metric *anx-havings-hash*))

(defun anx-print-report-meta (report-meta)
  ;; Array -> IO State!
  (progn
    (anx-build-columns-hash report-meta)
    (anx-build-filters-hash report-meta)
    (anx-build-havings-hash report-meta)
    ;; (anx-print-time-intervals report-meta)
    (anx-print-dimensions-table)
    (anx-print-metrics-table)
    (anx-clear-hashes)))

(defun anx-really-print-report-meta ()
  ;; -> IO State!
  (interactive)
  (let ((report-meta (read (buffer-string))))
    (anx-print-report-meta report-meta)))

(defun anx-print-time-intervals (report-meta)
  (anx-build-time-intervals-list report-meta))

(defun anx-print-dimensions-table ()
  ;; Array -> IO State!
  (progn 
    (anx-print-to-scratch-buffer *anx-dimensions-table-header*)
    (mapcar (lambda (elem)
	      (anx-print-to-scratch-buffer (format "| %s | %s | %s | |\n" elem 
						  (gethash elem *anx-columns-hash*)
						  (if (gethash elem *anx-filters-hash*)
						      "Yes"
						    "No"))))
	    (anx-build-dimensions-list))))

(defun anx-print-metrics-table ()
  ;; Array -> IO State!
  (progn
    (anx-print-to-scratch-buffer *anx-metrics-table-header*)
    (mapcar (lambda (elem)
	      (anx-print-to-scratch-buffer (format "| %s | %s | | |\n" elem (gethash elem *anx-columns-hash*))))
	    (anx-build-metrics-list))))

(defun anx-clear-hashes ()
  ;; -> State!
  (progn (clrhash *anx-havings-hash*)
	 (clrhash *anx-columns-hash*)
	 (clrhash *anx-filters-hash*)))

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

(defun anx-table-lines-to-list (buf)
  ;; Buffer -> List
  (with-current-buffer buf
    (let ((result nil))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^|[^|]" nil t)
	  (let ((beg (point))
		(end)
		(str)
		(inhibit-field-text-motion t))
	    (re-search-forward "|\n" nil t)
	    (setq end (point))
	    (setq str (buffer-substring-no-properties beg end))
	    (push str result)
	    (goto-char beg))))
      (reverse result))))

(defun anx-print-table-lines (buf)
  ;; List -> IO
  (let ((list (anx-table-lines-to-list buf)))
    (mapcar (lambda (elem)
	      (anx-print-to-scratch-buffer (format "| %s" elem)))
	    list)))

(defun anx-really-print-table-lines ()
  ;; -> IO
  (interactive)
  (let ((buf (current-buffer)))
    (anx-print-table-lines buf)))

;;; Mobile Error Messages

(defvar *anx-sdk-error-table-header*
  "\n|| SDK Type || Error Message || Key ||\n")

(defvar *anx-android-sdk-errors* (make-hash-table :test 'equal))

(defvar *anx-ios-sdk-errors* (make-hash-table :test 'equal))

(defun anx-sdk-error:on (error-object)
  (anx-assoc-val 'on error-object))

(defun anx-sdk-error:message (error-object)
  (anx-assoc-val 'message error-object))

(defun anx-sdk-error:key (error-object)
  (anx-assoc-val 'key error-object))

(defun anx-sdk-error:android-p (error-object)
  ;; Alist -> Boolean
  (if (aref (anx-sdk-error:on error-object) 0)
      t
    nil))

(defun anx-sdk-error:ios-p (error-object)
  ;; Alist -> Boolean
  (let ((len 
	 (length (anx-sdk-error:on error-object))))
    (if (>= len 2)
	t
      nil)))

(defun anx-really-print-sdk-error-table ()
  ;; -> IO State!
  (interactive)
  (let ((sdk-error-array (read (buffer-string))))
    (anx-print-sdk-error-table sdk-error-array)))

(defun anx-print-sdk-error-table (sdk-error-array)
  ;; Array -> IO State!
  (progn
    (anx-print-to-scratch-buffer *anx-sdk-error-table-header*)
    (mapcar (lambda (e)
	      (let ((sdk-type (anx-sdk-error:on e))
		    (message (anx-sdk-error:message e))
		    (key (anx-sdk-error:key e)))
		(anx-print-to-scratch-buffer (format "| %s | %s | {{%s}} |\n" sdk-type message key))))
	    sdk-error-array)))

;; anx-docgen.el ends here.
