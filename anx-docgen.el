;;; ANX-Docgen

(defvar *an-stack* nil
  "The stack where we stash our second-pass field definitions.")

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

(defun an-clear-stack ()
  ;; State!
  (progn (setq *an-stack* nil)
	 (setq *an-count* 0)))

(defun an-translate-boolean (symbol)
  ;; Symbol -> String
  (if (equal symbol :json-false)
      "No"
    "Yes"))

(defun an-print-object-standard-fields (object)
	 ;; Alist -> IO
  (format "| %s | %s | %s | %s | | | |\n"
	  (assoc-val 'name object)
	  (assoc-val 'type object)
	  (an-translate-boolean (assoc-val 'sort_by object))
	  (an-translate-boolean (assoc-val 'filter_by object))))

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

(defun an-print-meta (array-of-alists)
  ;; Array -> IO State!
  (if (array-of-alistsp array-of-alists)
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

;; anx-docgen.el ends here.
