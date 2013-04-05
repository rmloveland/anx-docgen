;;; anx-docgen.el --- A tool for generating structured API documentation.
;;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rich Loveland

;; Author: Rich Loveland
;; Version: 0.1
;; Keywords: convenience, JSON, REST, API, Documentation

;; This file is NOT part of GNU Emacs.

;; This code is written by Richard M. Loveland and placed in the
;; Public Domain. All warranties are disclaimed.

;;; Commentary:
;;  In order for this to work, json.el must be configured to represent
;;  JSON objects as association lists.

;;; Code:

(require 'elisp-format)

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

(defun anx-alistify-object (json-object)
  ;; Alist -> Alist
  "Given JSON-OBJECT (an alist), return an alist using our preferred intermediate representation."
  (list
   (cons 'name (anx-assoc-val 'name json-object))
   (cons 'type (anx-assoc-val 'type json-object))
   (cons 'sort_by (anx-translate-boolean (anx-assoc-val 'sort_by json-object)))
   (cons 'filter_by (anx-translate-boolean (anx-assoc-val 'filter_by json-object)))
   (cons 'description "")
   (cons 'default "")
   (cons 'required_on "")))

(defun anx-alistify-objects (array-of-alists)
  ;; Array -> Alist
  "Given an ARRAY-OF-ALISTS, build an alist using our intermediate representation."
  (mapcar (lambda (json-object)
	    (anx-alistify-object json-object))
	  array-of-alists))

(defun anx-process-stack-item (list)
  ;; List -> IO State!
  "Given a LIST of the form (NAME . ARRAY-OF-ALISTS), return our intermediate representation.
These are created when top-level JSON fields contain child fields
that need to be defined in their own tables."
  (let* ((lc-name (car list))
	 (uc-name (capitalize lc-name))
	 (array-of-alists (cdr list)))
    (list (list 'title
		(list 'text uc-name))
	  (list 'header *anx-standard-table-header*)
	  (list (cons 'items
		      (mapcar (lambda (object)
				;; Nothing should have fields at this level (I hope).
				(anx-alistify-object object))
			      array-of-alists))))))

(defun anx-process-stack-items ()
  ;; -> IO State!
  "Pop items off of `*anx-json-stack*' and process them with `anx-process-stack-item'."
  (let ((result nil))
    (while (not (anx-stack-empty-p))
      (push (anx-process-stack-item (anx-stack-pop)) result))
    result))

(defun anx-object-has-fields-p (json-object)
  ;; Alist -> Boolean
  "Determine if JSON-OBJECT has any sub-fields that need their own tables."
  (if (anx-assoc-val 'fields json-object)
      t
      nil))

(defun anx-save-fields-for-later (json-object)
  ;; Alist -> State!
  "Given JSON-OBJECT, pushes a (NAME . FIELDS) pair on `*anx-json-stack*'."
  (let ((name (anx-assoc-val 'name json-object)))
    (anx-stack-push (cons name (anx-assoc-val 'fields json-object)))))

(defun anx-process-object (json-object)
  ;; Alist -> Alist State!
  "Given JSON-OBJECT, we alistify it and stash any child fields on the stack."
  (progn
    (if (anx-object-has-fields-p json-object)
	(anx-save-fields-for-later json-object)
      nil)
    (anx-alistify-object json-object)))

(defun anx-process-objects (array-of-alists)
  ;; Array -> IO State!
  "Given ARRAY-OF-ALISTS, ..."
  (cons 'items
	(mapcar (lambda (json-object)
		  (anx-process-object json-object))
		array-of-alists)))

(defvar *anx-standard-table-header*
  '("Name" "Type" "Sort by?" "Filter by?" "Description" "Default" "Required on")
  "The format string used for wiki table columns in documentation for standard API services.")

(defvar *anx-standard-table-row*
  "| %s | %s | %s | %s | | | |\n"
  "The format string used for wiki table rows in documentation for standard API services.")

(defun anx-print-to-scratch-buffer (format-string)
  ;; -> IO
  "Print FORMAT-STRING to the *scratch* buffer."
  (princ format-string
	 (get-buffer "*scratch*")))

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
  "Generates API service documentation from the contents of the current buffer.
Prints its output to the *scratch* buffer."
  (interactive)
  (let ((array-of-alists (read (buffer-string))))
    (anx-print-meta array-of-alists)))


;; Part 2. Reporting

(defvar *anx-report-dimensions-table-header*
  "\n|| Column || Type || Filter? || Description ||\n"
  "The format string used for Dimensions table columns in documentation for reporting API services.")

(defvar *anx-report-metrics-table-header*
  "\n|| Column || Type || Formula || Description ||\n"
  "The format string used for Metrics table columns in documentation for reporting API services.")

(defvar *anx-havings-hash* (make-hash-table :test 'equal)
  "Records the existence of 'column fields (using t) from the 'havings' array returned by Report Service /meta calls")

(defvar *anx-filters-hash* (make-hash-table :test 'equal)
  "Associates 'column and 'type fields from the 'filters' array returned by Report Service /meta calls")

(defvar *anx-columns-hash* (make-hash-table :test 'equal)
  "Associates 'column and 'type fields from the 'columns' array returned by Report Service /meta calls")

(defun anx-build-columns-hash (report-meta-alist)
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx-columns-hash* from it."
  (mapc (lambda (alist)
	  (puthash (anx-assoc-val 'column alist) 
		   (anx-assoc-val 'type alist) 
		   *anx-columns-hash*))
	(anx-assoc-val 'columns report-meta-alist)))

(defun anx-build-filters-hash (report-meta-alist)
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx-filters-hash* from it."
  (mapc (lambda (alist) 
	  (puthash (anx-assoc-val 'column alist) 
		   (anx-assoc-val 'type alist)
		   *anx-filters-hash*))
	(anx-assoc-val 'filters report-meta-alist)))

(defun anx-build-havings-hash (report-meta-alist)
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx-havings-hash* from it."
  (mapc (lambda (alist) 
	  (puthash (anx-assoc-val 'column alist) t *anx-havings-hash*))
	(anx-assoc-val 'havings report-meta-alist)))

(defun anx-build-dimensions-list ()
  ;; -> List
  "Builds a list from elements of 'columns' that are not also in 'havings'.
In other words, return only the dimensions and not the metrics."
  (let ((results nil))
    (maphash (lambda (k v) 
	       (unless (gethash k *anx-havings-hash*)
		 (push k results)))
	     *anx-columns-hash*)
    (reverse results)))

(defun anx-build-metrics-list ()
  ;; -> List
  "Builds a list from elements of 'havings', which are metrics."
  (let ((results nil))
    (maphash (lambda (k v) 
		 (push k results))
	     *anx-havings-hash*)
    (reverse results)))

(defun anx-print-report-meta (report-meta-alist)
  ;; Array -> IO State!
  "Generates report documentation from REPORT-META-ALIST.
Along the way, sets up and tears down hash tables to hold the
necessary state."
  (progn
    (anx-build-columns-hash report-meta-alist)
    (anx-build-filters-hash report-meta-alist)
    (anx-build-havings-hash report-meta-alist)
    (anx-print-dimensions-table)
    (anx-print-metrics-table)
    (anx-clear-report-hashes)))

(defun anx-really-print-report-meta ()
  ;; -> IO State!
  "Generates Reporting API documentation from the contents of the current buffer.
Prints its output to the *scratch* buffer."
  (interactive)
  (let ((report-meta (read (buffer-string))))
    (anx-print-report-meta report-meta)))

(defun anx-print-dimensions-table ()
  ;; -> IO State!
  "Prints a table of the report's dimensions in the *scratch* buffer."
  (progn 
    (anx-print-to-scratch-buffer *anx-report-dimensions-table-header*)
    (mapcar (lambda (elem)
	      (anx-print-to-scratch-buffer (format "| %s | %s | %s | |\n" elem 
						  (gethash elem *anx-columns-hash*)
						  (if (gethash elem *anx-filters-hash*)
						      "Yes"
						    "No"))))
	    (anx-build-dimensions-list))))

(defun anx-print-metrics-table ()
  ;; Array -> IO State!
  "Prints a table of the report's metrics in the *scratch* buffer."
  (progn
    (anx-print-to-scratch-buffer *anx-report-metrics-table-header*)
    (mapcar (lambda (elem)
	      (anx-print-to-scratch-buffer (format "| %s | %s | | |\n" elem (gethash elem *anx-columns-hash*))))
	    (anx-build-metrics-list))))

(defun anx-clear-report-hashes ()
  ;; -> State!
  "Clear the state hash tables used to generate documentation for Reporting APIs."
  (progn (clrhash *anx-havings-hash*)
	 (clrhash *anx-columns-hash*)
	 (clrhash *anx-filters-hash*)))


;;; Part 3. Mobile SDK Error Messages

(defvar *anx-mobile-sdk-error-table-header*
  "\n|| Message || Key ||\n"
  "The format string used for table columns in Mobile SDK error documentation.")

(defvar *anx-android-sdk-errors* (make-hash-table :test 'equal)
  "Stores the keys and associated messages that display in Android logs.")

(defvar *anx-ios-sdk-errors* (make-hash-table :test 'equal)
  "Stores the keys and associated messages that display in iOS logs.")

(defun anx-clear-sdk-error-hashes ()
  ;; -> State!
  "Cleans up the hash tables used to maintain state when generating Mobile SDK error tables."
  (progn (clrhash *anx-android-sdk-errors*)
	 (clrhash *anx-ios-sdk-errors*)))

(defun anx-sdk-error:on (error-object)
  ;; Alist -> Array
  "Gets the 'on field of ERROR-OBJECT."
  (anx-assoc-val 'on error-object))

(defun anx-sdk-error:message (error-object)
  ;; Alist -> String
  "Gets the 'message field of ERROR-OBJECT."
  (anx-assoc-val 'message error-object))

(defun anx-sdk-error:key (error-object)
  ;; Alist -> String
  "Gets the 'key field of ERROR-OBJECT."
  (anx-assoc-val 'key error-object))

(defun anx-sdk-error:android-p (error-object)
  ;; Alist -> Boolean
  "Given ERROR-OBJECT, determine whether it can occur in the Android SDK."
  (let* ((devices (anx-sdk-error:on error-object))
	 (len (length devices)))
    (if (>= len 1)
	t
      nil)))

(defun anx-sdk-error:ios-p (error-object)
  ;; Alist -> Boolean
  "Given ERROR-OBJECT, determine whether it can occur in the iOS SDK."
  (let* ((devices (anx-sdk-error:on error-object))
	 (len (length devices)))
    (if (> len 1)
	t
      nil)))

(defun anx-print-sdk-error-tables (sdk-error-array)
  ;; Array -> IO State!
  "Given SDK-ERROR-ARRAY, generate documentation tables from it."
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
    (anx-print-to-scratch-buffer (format "\n\nh3. iOS\n"))
    (anx-print-to-scratch-buffer *anx-mobile-sdk-error-table-header*)
    (maphash (lambda (k v)
	       (anx-print-to-scratch-buffer (format "| %s | {{%s}} |\n" v k)))
	     *anx-ios-sdk-errors*)
    ;; Now for Android
    (anx-print-to-scratch-buffer (format "\n\nh3. Android\n"))
    (anx-print-to-scratch-buffer *anx-mobile-sdk-error-table-header*)
    (maphash (lambda (k v)
	       (anx-print-to-scratch-buffer (format "| %s | {{%s}} |\n" v k)))
	     *anx-android-sdk-errors*)
    (anx-clear-sdk-error-hashes)))

(defun anx-really-print-sdk-error-tables ()
  ;; -> IO State!
  "Generates Mobile SDK error documentation from the contents of the current buffer."
  (interactive)
  (let ((sdk-error-array (read (buffer-string))))
    (anx-print-sdk-error-tables sdk-error-array)))

;;; Part 4. Working with existing documentation

(defvar *anx-new-fields* (make-hash-table :test 'equal)
  "A hash table for storing the fields from a freshly generated API service document.")

(defvar *anx-old-fields* (make-hash-table :test 'equal)
  "A hash table for storing the fields from an existing API service's documentation.")

(defun anx-hash-incf (key table)
  ;; String Hash -> State!
  "Given a KEY and TABLE, increment the value of KEY.
If KEY is not numeric (perhaps because it's not defined), give it
a value of 1."
  (let ((curval (gethash key table)))
    (if (numberp curval)
	(puthash key (+ 1 curval) table)
      (puthash key 1 table))))

(defun anx-old-fields-incf (field)
  ;; String -> State!
  "A convenience function to increment the value of FIELD in `*anx-old-fields*'."
  (anx-hash-incf field *anx-old-fields*))

(defun anx-new-fields-incf (field)
  ;; String ->State!
  "A convenience function to increment the value of FIELD in `*anx-new-fields*'."
  (anx-hash-incf field *anx-new-fields*))

(defun anx-clear-fields ()
  ;; -> State!
  "A convenience function for clearing the state of our fields storage."
  (progn
    (clrhash *anx-old-fields*)
    (clrhash *anx-new-fields*)))

(defun anx-extract-fields (buffer)
  ;; Buffer -> List
  "Make a list from all of the field names listed in BUFFER."
  (let ((result nil))
    (save-excursion
      (with-current-buffer buffer
	(goto-char (point-min))
	(while (re-search-forward "^| \\([A-Za-z_.]+\\)" nil t)
	  (push (match-string-no-properties 1) result))))
    result))

(defun anx-build-fields (buffer hash)
  ;; Hash Buffer -> State!
  "Add the fields from BUFFER to HASH."
  (let ((fields (anx-extract-fields buffer)))
    (mapc (lambda (x) (anx-hash-incf x hash))
	  fields)))

(defun anx-build-old-fields (buffer)
  ;; Buffer -> State!
  "A convenience function for extracting fields from BUFFER.
BUFFER should contain the old (which is to say, existing)
documentation."
  (anx-build-fields buffer *anx-old-fields*))

(defun anx-build-new-fields (buffer)
  ;; Buffer -> State!
  "A convenience function for extracting fields from BUFFER.
BUFFER should contain the new (which is to say, freshly generated)
tables ready to be filled out with documentation."
  (anx-build-fields buffer *anx-new-fields*))

(defun anx-delta-fields ()
  ;; -> Alist
  "Checks the new fields table against the old, returning the delta.
Returns an alist with elements of the form (FIELD . OCCURRENCES).
Note that we currently only return a delta if the new table
contains fields the old one does not (not the other way around)."
  (let ((result nil))
    (maphash (lambda (k v)
	       (if (= (anx-old-fields-incf k) 1)
		   ;; If 1, key didn't exist in old table, so is new
		   (push (cons k (- (anx-new-fields-incf k) 1)) result)))
	     *anx-new-fields*)
    result))

(defun anx-delta-wiki-buffers (buf1 buf2)
  ;; Buffer Buffer -> IO State!
  "Check the fields listed in BUF1 against those in BUF2, returning the delta.

The delta is opened in a new buffer, and is only shown if the
newly generated buffer contains fields the existing documentation
does not.

BUF1 should contain the existing documentation, and BUF2 the
freshly generated tables."
  (interactive "bBuffer 1 (Old): \nbBuffer 2 (New): ")
  (let ((deltabuf (generate-new-buffer "*ANX-Docgen Delta*")))
    (save-excursion
      ;; First, clear old hash contents
      (anx-clear-field-name-hashes)
      (anx-build-new-fields (get-buffer buf1))
      (anx-build-old-fields (get-buffer buf2))
      (with-current-buffer deltabuf
	(princ (anx-delta-fields) (current-buffer))
	(emacs-lisp-mode)
	(elisp-format-buffer))
      (switch-to-buffer-other-window deltabuf))))

(provide 'anx-docgen)

;;; anx-docgen.el ends here
