;;; ANX-Docgen

(setq *an-json-stack* nil)

(defun an-get-response-object (title response-json)
  ""
  (let ((result (assoc title (assoc 'response response-json))))
    (cond ((equal title 'invoices)
	   (elt (cdr result) 0))
	  (t result))))

(defun alistp (object)
  (and (listp object)
       (every #'consp object)))

(defun get-alist-keys (alist)
  (mapcar (lambda (elem)
	    (car elem))
	  alist))

(defun objectp (it) (alistp it))

(defun get-object-keys (it)
  (get-alist-keys it))

(defun array-of-objectsp (a)
  (and (arrayp a)
       (objectp (elt a 0))))

(defun get-random-object (array-of-objects)
  (let* ((len (length array-of-objects))
	 (rand (random len)))
    (elt array-of-objects rand)))

(defun object-get-val (key object)
  (assoc key object))

(defun an-build-json-stack (response-object) ;; (an-get-response-object 'invoices msft-invoice)
  (cond ((null response-object) nil)
	((array-of-objectsp response-object)
	 (an-build-json-stack
	  (get-random-object response-object)))
	((objectp response-object)
	 (let ((keys (get-object-keys response-object)))
	   (progn
	     (push keys *an-json-stack*)
	     (mapc (lambda (x)
		     (let ((val (object-get-val x response-object)))
		       (if (array-of-objectsp val)
			   (push (get-random-object val) *an-json-stack*)
			 (push val *an-json-stack*))
		       (an-build-json-stack val)))
		     keys))))
	(t nil)))

(defun an-traverse-stack ()
  (mapcar (lambda (x) (progn (format "Level: %d %S" *an-count* x)) (reverse *an-json-stack*))
  (mapcar (lambda (x)
	    (*an-count*
	     x
		 (incf *an-count*)))
	(reverse *an-json-stack*)))

(defun an-clear-stack ()
  (progn (setq *an-json-stack* nil)
	 (setq *an-count* 0)))

(defun an-invoice-test ()
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat (buffer-name) " (PARSED)"))
	(mode 'emacs-lisp-mode))
    (an-build-json-stack (an-get-response-object 'invoices it))
    (smart-print-buf bufname (reverse *an-json-stack*) mode)
    (an-clear-stack)))

;; anx-docgen.el ends here.
