;;; anx-docgen.el --- A tool for generating structured API documentation.
;;; -*- lexical-binding: t -*-

;;; Commentary:
;;  Note that json.el must be configured to represent JSON objects as
;;  association lists.

;;; Code:

;; Part 1. Standard API Services

(defvar *anx-json-stack* nil
  "The stack where we stash our second-level JSON field definitions.")

(defun anx-alistp (object)
  ;; Object -> Boolean
  "Determine if this OBJECT is an association list."
  (and (listp object)
       (every #'consp object)))

(defun anx-get-alist-keys (object)
  ;; Object -> List
  "If OBJECT is an association list, return a list of its keys.
Otherwise return nil."
  (if (anx-alistp object)
      (mapcar (lambda (elem)
		(car elem))
	      object)
    nil))

(defun anx-array-of-alists-p (object)
  ;; Object -> Boolean
  "Determine if OBJECT is an array of association lists."
  (and (arrayp object)
       (equalp nil (remove-if (lambda (x) (equalp x t)) 
			      (mapcar (lambda (x) (anx-alistp x)) 
				      object)))))

(defun anx-assoc-val (key alist)
  ;; Symbol Alist -> Object
  "Given KEY and ALIST, return the value associated with KEY.
Unlike `assoc', this function does not return the entire
key-value pair."
  (let ((result (assoc key alist)))
    (if result
	(cdr result)
      result)))

(defun anx-stack-push (item)
  ;; Item -> State!
  "Push ITEM onto `*anx-json-stack*'."
  (push item *anx-json-stack*))

(defun anx-stack-pop ()
  ;; -> State!
  "Pop an item off of `*anx-json-stack*'."
  (pop *anx-json-stack*))

(defun anx-stack-empty-p ()
  ;; -> Boolean
  "Check if `*anx-json-stack*' is empty."
  (null *anx-json-stack*))

(defun anx-clear-stack ()
  ;; -> State!
  "Clear the contents of `*anx-json-stack*'."
  (progn (setq *anx-json-stack* nil)))

(defun anx-translate-boolean (symbol)
  ;; Symbol -> String
  "Translate the boolean SYMBOL into something suitable for printing."
  (if (or (equal symbol :json-false)
	  (null symbol))
      "No"
    "Yes"))

(defun anx-format-object-standard-fields (json-object)
  ;; Alist -> IO
  "Format the fields of JSON-OBJECT for printing using `*anx-standard-table-row*'."
  (let* ((fields (anx-list-object-standard-fields json-object)))
    (format *anx-standard-table-row*
	    (pop fields)
	    (pop fields)
	    (pop fields)
	    (pop fields))))

(defun anx-list-object-standard-fields (json-object)
  ;; Alist -> List
  "Return a list of JSON-OBJECT's fields."
  (list
   (anx-assoc-val 'name json-object)
   (anx-assoc-val 'type json-object)
   (anx-translate-boolean (anx-assoc-val 'sort_by json-object))
   (anx-translate-boolean (anx-assoc-val 'filter_by json-object))))

(defun anx-object-has-fields-p (json-object)
  ;; Alist -> Boolean
  "Determine if JSON-OBJECT has any sub-fields that need their own tables."
  (if (assoc 'fields json-object)
      t
    nil))

(defun anx-save-fields-for-later (json-object)
  ;; Alist -> State!
  "Given JSON-OBJECT, pushes a (NAME . FIELDS) pair on `*anx-json-stack*'."
  (let ((name (anx-assoc-val 'name json-object)))
    (anx-stack-push (cons name (anx-assoc-val 'fields json-object)))))

(defun anx-process-object (json-object)
  ;; Alist -> IO State!
  "Prints the fields from JSON-OBJECT in the *scratch* buffer.
If JSON-OBJECT has additional nested fields, saves them for
further processing."
  (progn
    (anx-print-to-scratch-buffer
     (anx-format-object-standard-fields json-object))
    (if (anx-object-has-fields-p json-object)
	(anx-save-fields-for-later json-object))))

(defun anx-process-objects (array-of-alists)
  ;; Array -> IO State!
  "Print a wiki table built from ARRAY-OF-ALISTS to the *scratch* buffer."
  (anx-print-to-scratch-buffer
   (format "\nh4. JSON Fields\n\n"))
  (anx-print-to-scratch-buffer
   (format *anx-standard-table-header*))
  (mapc (lambda (json-object)
	  (anx-process-object json-object))
	array-of-alists))

(defvar *anx-standard-table-header*
  "|| Name || Type || Sort By? || Filter By? || Description || Default || Required On ||\n"
  "The format string used for wiki table columns in documentation for standard API services.")

(defvar *anx-standard-table-row*
  "| %s | %s | %s | %s | | | |\n"
  "The format string used for wiki table rows in documentation for standard API services.")

(defun anx-print-to-scratch-buffer (format-string)
  ;; -> IO
  "Print FORMAT-STRING to the *scratch* buffer."
  (princ format-string
	 (get-buffer "*scratch*")))

(defun anx-process-stack-item (list)
  ;; List -> IO State!
  "Given a LIST of the form (NAME . FIELDS), print documentation tables.
These are created when JSON fields in the primary table contain
additional fields that need to be defined in their own tables."
  (let ((name (capitalize (car list)))
	(array-of-alists (cdr list)))
    (anx-print-to-scratch-buffer
     (format "\nh4. %s\n\n" name))
    (anx-print-to-scratch-buffer
     (format *anx-standard-table-header*))
    (mapc (lambda (object)
	    ;; Nothing should have fields at this level (I hope).
	    (anx-process-object object))
	  array-of-alists)))

(defun anx-process-stack-items ()
  ;; -> IO State!
  "Pop items off of `*anx-json-stack*' and process them with `anx-process-stack-item'."
  (while (not (anx-stack-empty-p))
    (anx-process-stack-item (anx-stack-pop))))

(defun anx-print-meta (array-of-alists)
  ;; Array -> IO State!
  "Given an ARRAY-OF-ALISTS, print documentation tables from it."
  (if (anx-array-of-alists-p array-of-alists)
      (progn
	(anx-clear-stack)
	(anx-process-objects array-of-alists)
	(anx-process-stack-items))
    (error "`anx-print-meta' expects an array of association lists.")))

(defun anx-really-print-meta ()
  ;; -> IO State!
  "Generate documentation from the contents of the current buffer."
  (interactive)
  (let ((array-of-alists (read (buffer-string))))
    (anx-print-meta array-of-alists)))


;; Part 2. Reporting

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

	 
;;; Part 3. Working with existing documentation

(defvar *anx-current-meta-names* (make-hash-table))

(defvar *anx-existing-table-names* (make-hash-table))

(defun anx-clear-meta-hashes ()
  ;; -> State!
  (progn (clrhash *anx-current-meta-names*)
	 (clrhash *anx-existing-table-names*)))

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

(defun anx-extract-field-names-list (table-list)
  ;; List -> List
  (mapcar (lambda (line-string)
	    (let ((first-word (car (anx-split-line-string line-string))))
	      (string-match "[a-z_]+" first-word 0)
	      (match-string-no-properties 0 first-word)))
	  table-list))

(defun anx-build-existing-table-names-hash (table-list)
  ;; List -> State!
  (let ((names (anx-extract-existing-names-list table-list)))
    (mapcar (lambda (name) (puthash name 1 *anx-existing-table-names*) names))))

(defun anx-build-current-meta-names-hash (array-of-alists)
  (let ((names (mapcar (lambda (alist)
			 (anx-assoc-val 'name alist))
		       array-of-alists)))
    (mapcar (lambda (name) (puthash name 1 *anx-existing-table-names*))
	    names)))

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


;;; Part 4. Mobile SDK Error Messages

(defvar *anx-sdk-error-table-header*
  "\n|| Android? || iOS? || Message || Key ||\n")

(defvar *anx-new-sdk-error-table-header*
  "\n|| Message || Key ||\n")

(defvar *anx-android-sdk-errors* (make-hash-table :test 'equal))

(defvar *anx-ios-sdk-errors* (make-hash-table :test 'equal))

(defun anx-clear-sdk-error-hashes ()
  ;; -> State!
  (progn (clrhash *anx-android-sdk-errors*)
	 (clrhash *anx-ios-sdk-errors*)))

(defun anx-sdk-error:on (error-object)
  (anx-assoc-val 'on error-object))

(defun anx-sdk-error:message (error-object)
  (anx-assoc-val 'message error-object))

(defun anx-sdk-error:key (error-object)
  (anx-assoc-val 'key error-object))

(defun anx-sdk-error:android-p (error-object)
  ;; Alist -> Boolean
  (let* ((devices (anx-sdk-error:on error-object))
	 (len (length devices)))
    (if (>= len 1)
	t
      nil)))

(defun anx-sdk-error:ios-p (error-object)
  ;; Alist -> Boolean
  (let* ((devices (anx-sdk-error:on error-object))
	 (len (length devices)))
    (if (> len 1)
	t
      nil)))

;; New version, prints one table for each device.

(defun anx-print-sdk-error-tables (sdk-error-array)
  ;; Array -> IO State!
  (progn
    ;; First, build hash tables
    (mapcar (lambda (e)
	      (let ((android-p (anx-sdk-error:android-p e))
		    (ios-p (anx-sdk-error:ios-p e))
		    (message (anx-sdk-error:message e))
		    (key (anx-sdk-error:key e)))
		(if android-p
		    (puthash key message *anx-android-sdk-errors*))
		(if ios-p
		    (puthash key message *anx-ios-sdk-errors*))))
	    sdk-error-array)
    ;; Then, maphash and build each table, starting with iOS
    (anx-print-to-scratch-buffer (format "\n\nh3. iOS\n\n"))
    (anx-print-to-scratch-buffer *anx-new-sdk-error-table-header*)
    (maphash (lambda (k v)
	       (anx-print-to-scratch-buffer (format "| %s | {{%s}} |\n" v k)))
	     *anx-ios-sdk-errors*)
    ;; Now for Android
    (anx-print-to-scratch-buffer (format "\n\nh3. Android\n\n"))
    (anx-print-to-scratch-buffer *anx-new-sdk-error-table-header*)
    (maphash (lambda (k v)
	       (anx-print-to-scratch-buffer (format "| %s | {{%s}} |\n" v k)))
	     *anx-android-sdk-errors*)))

(defun anx-really-print-sdk-error-tables ()
  ;; -> IO State!
  (interactive)
  (let ((sdk-error-array (read (buffer-string))))
    (anx-print-sdk-error-tables sdk-error-array)))

;; anx-docgen.el ends here.

(provide 'anx-docgen)

;;; anx-docgen.el ends here
