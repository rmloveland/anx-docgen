;;; ANX-Docgen

(defvar *an-stack* nil
  "The stack where we stash our second-pass field definitions.")

(defun an-get-response-object (title response-json)
  ;; Symbol Alist -> Alist
  (let ((result (assoc title (assoc 'response response-json))))
    (cond ((equal title 'invoices)
	   (elt (cdr result) 0))
	  (t result))))

(defun alistp (object)
  ;; Object -> Boolean
  (and (listp object)
       (every #'consp object)))

(defun get-alist-keys (alist)
  ;; Alist -> List
  (if (alistp alist)
      (mapcar (lambda (elem)
		(car elem))
	      alist)
    nil))

(defun array-of-alistsp (array)
  ;; Object -> Boolean
  (and (arrayp array)
       (alistp (elt array 0))))

(defun assoc-val (key alist)
  ;; Symbol Alist -> Object
  (let ((result (assoc key alist)))
    (if result
	(cdr result)
      result)))

(defun get-random-alist (array-of-alists)
  ;; Array of Alists -> Alist
  (let* ((len (length array-of-alists))
	 (rand (random len)))
    (elt array-of-alists rand)))

(defun stack-push (item)
  ;; Item -> State!
  (push item *an-stack*))

(defun stack-pop ()
  ;; State!
  (pop *an-stack*))

(defun an-stack-emptyp ()
  (null *an-stack*))

(defun an-build-json-stack (response-object)
  ;; Alist -> State!
  ;; (an-get-response-object 'invoices msft-invoice)
  (cond ((null response-object) nil)
	((array-of-objectsp response-object)
	 (let ((obj (get-random-object response-object)))
	   (push obj *an-json-stack*)
	   (an-build-json-stack obj)))
	((objectp response-object)
	 (let ((keys (get-object-keys response-object)))
	   (progn
	     (push keys *an-json-stack*)
	     (mapc (lambda (x)
		     (let ((val (assoc x response-object)))
		       (if (array-of-objectsp val)
			   (push (get-random-object val) *an-json-stack*)
			 (push val *an-json-stack*))
		       (an-build-json-stack val)))
		     keys))))
	(t nil)))

(defun an-clear-stack ()
  ;; State!
  (progn (setq *an-stack* nil)
	 (setq *an-count* 0)))

(defun an-print-object-standard-fields (object)
	 ;; Alist -> IO
  (format "| %s | %s | %s | %s | | | |\n"
	  (assoc-val 'name object)
	  (assoc-val 'type object)
	  (assoc-val 'sort_by object)
	  (assoc-val 'filter_by object)))

(defun an-object-has-fieldsp (object)
	 ;; Alist -> Boolean
	 (if (assoc 'fields object)
	     t
	   nil))

(defun an-save-fields-for-later (object)
  ;; Alist -> State!
  (let ((name (assoc-val 'name object)))
    (stack-push (cons name (assoc-val 'fields object)))))

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
   (format "\nh4. JSON Fields\n"))
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
     (format "\nh4. %s\n" name))
    (an-print-to-scratch-buffer
     (format *an-table-header*))
    (mapc (lambda (object)
	    ;; Nothing should have fields at this level (I hope).
	    (an-process-object object))
	  array-of-alists)))

(defun an-process-stack-items ()
  ;; -> IO State!
  (while (not (an-stack-emptyp))
    (an-process-stack-item (stack-pop))))

;; anx-docgen.el ends here.
