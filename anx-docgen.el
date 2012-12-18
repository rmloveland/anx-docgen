;;; ANX-Docgen

(defvar *an-json-stack* nil
  "The global stack.")

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
		     (let ((val (assoc x response-object)))
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
