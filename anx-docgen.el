;;; ANX-Docgen

(setq *an-json-stack* nil)

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
  (mapcar (lambda (elem)
	    (car elem))
	  alist))

(defun objectp (it)
  ;; Object -> Boolean
  (alistp it))

(defun get-object-keys (it)
  ;; Alist -> List
  (get-alist-keys it))

(defun array-of-objectsp (a)
  ;; Object -> Boolean
  (and (arrayp a)
       (objectp (elt a 0))))

(defun get-random-object (array-of-objects)
  ;; Array of Objects -> Object
  (let* ((len (length array-of-objects))
	 (rand (random len)))
    (elt array-of-objects rand)))

(defun object-get-val (key object)
  ;; Key Object -> Value
  (assoc key object))

(defun stack-push (item)
  ;; Item -> State!
  (push item *an-json-stack*))

(defun stack-pop ()
  ;; State!
  (pop *an-json-stack*))

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
		     (let ((val (object-get-val x response-object)))
		       (if (array-of-objectsp val)
			   (push (get-random-object val) *an-json-stack*)
			 (push val *an-json-stack*))
		       (an-build-json-stack val)))
		     keys))))
	(t nil)))

(defun an-clear-stack ()
  ;; State!
  (progn (setq *an-json-stack* nil)
	 (setq *an-count* 0)))

;; anx-docgen.el ends here.
