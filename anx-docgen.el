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
  (format "| %s | %s | %s | %s | | | |\n"
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
  "|| Column || Type || Group? || Filter? || Description ||\n")

(defvar *an-metrics-table-header*
  "|| Column || Type || Formula || Description ||\n")

(defun an-print-column-standard-fields (alist)
	 ;; Alist -> IO
  (format "| %s | %s |\n"
	  (an-assoc-val 'column alist)
	  (an-assoc-val 'type alist)))

(defun an-print-metric-standard-fields (alist)
  ;; Alist -> IO
  (format "| %s |\n"
	  (an-assoc-val 'column alist)))

(defun an-process-column (alist)
  ;; Alist -> IO State!
  (an-print-to-scratch-buffer
   (an-print-column-standard-fields alist)))

(defun an-process-columns (array-of-alists)
  ;; Array -> IO State!
  (an-print-to-scratch-buffer
   (format "\nh4. Dimensions\n\n")) ;; FIXME: Is this the right header?
  (an-print-to-scratch-buffer
   (format *an-dimensions-table-header*))
  (mapc (lambda (alist)
	  (an-process-column alist))
	array-of-alists))

(defun an-print-time-granularity (alist)
  ;; Alist -> IO
  (format "%s\n"
	  (assoc-val 'time_granularity network-analytics-meta)))

(defun an-print-time-frame (alist)
  ;; Alist -> IO
  (mapc (lambda (elem) 
	  (an-print-to-scratch-buffer
	     (format "%s\n" elem)))
	(assoc-val 'time_intervals network-analytics-meta)))
  
;; anx-docgen.el ends here.
