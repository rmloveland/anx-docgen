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

(require 'anx-api)

;; Part 1. Standard API Services

(defvar *anx--json-stack* nil
  "The stack where we stash our second-level JSON field definitions.")

(defun anx--alistp (object)
  ;; Object -> Boolean
  "Determine if this OBJECT is an association list."
  (and (listp object)
       (cl-every #'consp object)))

(defun anx--get-alist-keys (object)
  ;; Object -> List
  "If OBJECT is an association list, return a list of its keys.
Otherwise return nil."
  (if (anx--alistp object)
      (mapcar (lambda (elem)
		(car elem))
	      object)
    nil))

(defun anx--array-of-alists-p (object)
  ;; Object -> Boolean
  "Determine if OBJECT is an array of association lists."
  (and (arrayp object)
       (equal nil (cl-remove-if (lambda (x) (equal x t))
			      (mapcar (lambda (x) (anx--alistp x))
				      object)))))

(defun anx--assoc-val (key alist)
  ;; Symbol Alist -> Object
  "Given KEY and ALIST, return the value associated with KEY.
Unlike `assoc', this function does not return the entire
key-value pair."
  (let ((result (assoc key alist)))
    (if result
	(cdr result)
      result)))

(defun anx--stack-push (item)
  ;; Item -> State!
  "Push ITEM onto `*anx--json-stack*'."
  (push item *anx--json-stack*))

(defun anx--stack-pop ()
  ;; -> State!
  "Pop an item off of `*anx--json-stack*'."
  (pop *anx--json-stack*))

(defun anx--stack-empty-p ()
  ;; -> Boolean
  "Check if `*anx--json-stack*' is empty."
  (null *anx--json-stack*))

(defun anx--clear-stack ()
  ;; -> State!
  "Clear the contents of `*anx--json-stack*'."
  (progn (setq *anx--json-stack* nil)))

(defun anx--translate-boolean (symbol)
  ;; Symbol -> String
  "Translate the boolean SYMBOL into something suitable for printing."
  (if (or (equal symbol :json-false)
	  (null symbol))
      "No"
    "Yes"))

(defun anx--alistify-object (json-object)
  ;; Alist -> Alist
  "Given an alist JSON-OBJECT, return an alist in intermediate representation."
  (list
   (cons 'name (anx--assoc-val 'name json-object))
   (cons 'type (anx--assoc-val 'type json-object))
   (cons 'sort_by (anx--translate-boolean (anx--assoc-val 'sort_by json-object)))
   (cons 'filter_by (anx--translate-boolean (anx--assoc-val 'filter_by json-object)))
   (cons 'description "")
   (cons 'default "")
   (cons 'required_on "")))

(defun anx--alistify-objects (array-of-alists)
  ;; Array -> Alist
  "Given an ARRAY-OF-ALISTS, return an alist in intermediate representation."
  (mapcar (lambda (json-object)
	    (anx--alistify-object json-object))
	  array-of-alists))

(defun anx--process-stack-item (list)
  ;; List -> IO State!
  "Given a LIST, return an intermediate Lisp representation of the document.
These are created when some JSON fields contain child fields
that need to be defined in their own tables."
  (let* ((lc-name (car list))
	 (uc-name (mapconcat (lambda (x) x) (split-string (capitalize lc-name) "_") " "))
	 (array-of-alists (cdr list)))
    (list
     (list 'title
	   (list 'text uc-name))
     (list 'columns
	   *anx--standard-table-header*)
     (cons 'rows
	   (mapcar (lambda (object)
		     ;; Nothing should have fields at this level (I hope).
		     (anx--alistify-object object))
		   array-of-alists)))))

(defun anx--process-stack-items ()
  ;; -> IO State!
  "Pop items off of `*anx--json-stack*' and process them with `anx--process-stack-item'."
  (let ((result nil))
    (while (not (anx--stack-empty-p))
      (push (anx--process-stack-item (anx--stack-pop)) result))
    result))

(defun anx--object-has-fields-p (json-object)
  ;; Alist -> Boolean
  "Determine if JSON-OBJECT has any sub-fields that need their own tables."
  (if (anx--assoc-val 'fields json-object)
      t
      nil))

(defun anx--save-fields-for-later (json-object)
  ;; Alist -> State!
  "Given JSON-OBJECT, pushes a (NAME . FIELDS) pair on `*anx--json-stack*'."
  (let ((name (anx--assoc-val 'name json-object)))
    (anx--stack-push (cons name (anx--assoc-val 'fields json-object)))))

(defun anx--process-object (json-object)
  ;; Alist -> Alist State!
  "Given JSON-OBJECT, we alistify it and stash any child fields on the stack."
  (progn
    (if (anx--object-has-fields-p json-object)
	(anx--save-fields-for-later json-object)
      nil)
    (anx--alistify-object json-object)))

(defun anx--process-objects (array-of-alists)
  ;; Array -> IO State!
  "Given ARRAY-OF-ALISTS, ..."
  (list
   (list 'title
	 (list 'text "JSON Fields"))
   (list 'columns
	 *anx--standard-table-header*)
   (cons 'rows
	      (mapcar (lambda (json-object)
			(anx--process-object json-object))
		      array-of-alists))))

(defvar *anx--standard-table-header*
  '("Name" "Type" "Sort by?" "Filter by?" "Description" "Default" "Required on")
  "Titles for standard API wiki table columns.")

(defvar *anx--standard-table-row*
  "| %s | %s | %s | %s | %s | %s | %s |\n"
  "Format string for standard API wiki table rows.")

(defun anx--print-to-scratch-buffer (format-string)
  ;; -> IO
  "Print FORMAT-STRING to the *scratch* buffer."
  (princ format-string
	 (get-buffer "*scratch*")))

(defun anx--process-meta (array-of-alists)
  ;; Array -> Alist State!
  "Given ARRAY-OF-ALISTS, return an alist in intermediate representation."
  (let ((parent (anx--process-objects array-of-alists))
	(children (anx--process-stack-items)))
    (anx--clear-stack)
    (list (list 'parent parent)
	  (list 'children children))))

(defun anx-really-process-meta ()
  ;; -> IO State!
  "Convert the current buffer's contents to intermediate representation."
  (interactive)
  (let* ((array-of-alists (read (buffer-string)))
	 (result (anx--process-meta array-of-alists))
	 (bufname (concat (buffer-name) " [INTERMEDIATE REPRESENTATION]")))
    (anx--pop-up-buffer bufname result 'emacs-lisp-mode)))

(defun anx--parent:title (document)
  ;; Alist -> String
  "Given a Lisp DOCUMENT, return the title of the parent table."
  (car (anx--assoc-val 'text
		      (anx--assoc-val 'title
				     (car (anx--assoc-val 'parent document))))))

(defun anx--print-parent (parent-alist)
  ;; Alist -> IO
  "Given PARENT-ALIST, print its documentation table."
  (anx--print-parent-or-child parent-alist))

(defun anx--print-children (children)
  ;; List -> IO
  "Given a list CHILDREN, print each of their documentation tables."
  (mapc (lambda (x) (anx--print-parent x))
	children))

(defun anx--print-parent-or-child (parent-or-child-alist)
  ;; Alist -> IO
  "Given a PARENT-OR-CHILD-ALIST, print an API documentation table from it."
  (let ((title (second (car (anx--assoc-val 'title parent-or-child-alist))))
	(columns *anx--standard-table-header*)
	(rows (anx--assoc-val 'rows parent-or-child-alist)))
    (progn (anx--print-to-scratch-buffer (format "\nh2. %s\n\n" title))
	   (anx--print-to-scratch-buffer
	    (format "%s\n"
		    (concat "|| " (mapconcat
				   (lambda (x) x)
				   *anx--standard-table-header* " || ") " ||")))
	   (mapc (lambda (row)
		   (anx--print-to-scratch-buffer
		    (format *anx--standard-table-row*
			    (anx--assoc-val 'name row)
			    (anx--assoc-val 'type row)
			    (anx--assoc-val 'sort_by row)
			    (anx--assoc-val 'filter_by row)
			    (anx--assoc-val 'description row)
			    (anx--assoc-val 'default row)
			    (anx--assoc-val 'required_on row))))
		 rows))))

(defun anx--print-meta (array-of-alists)
  ;; Array -> IO State!
  "Given an ARRAY-OF-ALISTS, print documentation tables from it."
  (if (anx--array-of-alists-p array-of-alists)
      (let* ((ir (anx--process-meta array-of-alists))
	     (parent (car (anx--assoc-val 'parent ir)))
	     (children (car (anx--assoc-val 'children ir))))
	(progn
	  (anx--clear-stack)
	  (anx--process-objects array-of-alists)
	  (anx--process-stack-items)
	  (anx--print-parent parent)
	  (anx--print-children children)))
  (error "`anx--print-meta' expects an array of association lists")))

(defun anx-really-print-meta ()
  ;; -> IO State!
  "Generate API service documentation from the contents of the current buffer.

Prints its output to the *scratch* buffer."
  (interactive)
  (let ((array-of-alists (read (buffer-string))))
    (anx--print-meta array-of-alists)))


;; Part 2. Reporting

(defvar *anx--report-dimensions-table-header*
  '("Column" "Type" "Filter?" "Description")
  "Titles for reporting API dimensions columns.")

(defvar *anx--report-dimensions-table-row*
  "| %s | %s | %s | %s |\n"
  "Format string for reporting API dimensions rows.")

(defvar *anx--report-metrics-table-header*
  '("Column" "Type" "Formula" "Description")
  "Titles for reporting API Metrics columns.")

(defvar *anx--report-metrics-table-row*
  "| %s | %s | %s | %s |\n"
  "Format string for reporting API metrics rows.")

(defvar *anx--havings-hash* (make-hash-table :test 'equal)
  "Record the existence of 'column' fields from the 'havings' array.")

(defvar *anx--filters-hash* (make-hash-table :test 'equal)
  "Associate 'column' and 'type' fields from the 'filters' array.")

(defvar *anx--columns-hash* (make-hash-table :test 'equal)
  "Associate 'column' and 'type' fields from the 'columns' array.")

(defun anx--build-columns-hash (report-meta-alist)
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx--columns-hash* from it."
  (mapc (lambda (alist)
	  (puthash (anx--assoc-val 'column alist)
		   (anx--assoc-val 'type alist)
		   *anx--columns-hash*))
	(anx--assoc-val 'columns report-meta-alist)))

(defun anx--build-filters-hash (report-meta-alist)
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx--filters-hash* from it."
  (mapc (lambda (alist)
	  (puthash (anx--assoc-val 'column alist)
		   (anx--assoc-val 'type alist)
		   *anx--filters-hash*))
	(anx--assoc-val 'filters report-meta-alist)))

(defun anx--build-havings-hash (report-meta-alist)
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx--havings-hash* from it."
  (mapc (lambda (alist)
	  (puthash (anx--assoc-val 'column alist) t *anx--havings-hash*))
	(anx--assoc-val 'havings report-meta-alist)))

(defun anx--build-dimensions-list ()
  ;; -> List
  "Builds a list from elements of 'columns' that are not also in 'havings'.
In other words, return only the dimensions and not the metrics."
  (let ((results nil))
    (maphash (lambda (k v)
	       (unless (gethash k *anx--havings-hash*)
		 (push k results)))
	     *anx--columns-hash*)
    (reverse results)))

(defun anx--build-metrics-list ()
  ;; -> List
  "Builds a list from elements of 'havings', a.k.a. the metrics."
  (let ((results nil))
    (maphash (lambda (k v)
		 (push k results))
	     *anx--havings-hash*)
    (reverse results)))

(defun anx--process-dimensions-and-metrics ()
  ;; -> Alist
  "Given lists DIMENSIONS and METRICS, return an alist."
  (let ((dimensions (anx--process-dimensions))
	(metrics (anx--process-metrics)))
    (list dimensions metrics)))

(defun anx--process-dimensions ()
  ;; -> List
  "Return a Lisp list representation of the report dimensions."
  (list 'dimensions
	(list 'title
	      (list 'text "Dimensions"))
	(list 'header *anx--report-dimensions-table-header*)
	(list 'items
	      (mapcar (lambda (elem)
			(list (cons 'name elem)
			      (cons 'type (anx--get-column-type elem))
			      (cons 'filter_by (anx--translate-boolean (anx--report-filter-p elem)))
			      (cons 'description "")))
		      (anx--build-dimensions-list)))))

(defun anx--process-metrics ()
  ;; -> List
  "Return a Lisp list representation of the report metrics."
  (list 'metrics
	(list 'title
	      (list 'text "Metrics"))
	(list 'header *anx--report-metrics-table-header*) ; column type formula description
	(list 'items
	      (mapcar (lambda (elem)
			(list (cons 'name elem)
			      (cons 'type (anx--get-column-type elem))
			      (cons 'formula "")
			      (cons 'description "")))
		      (anx--build-metrics-list)))))

(defun anx--report-filter-p (item)
  ;; String -> Boolean
  "Given report column ITEM, determine if it can be a reporting filter.
Use `anx--translate-boolean' to create a representation suitable for printing."
  (if (gethash item *anx--filters-hash*)
      t
    nil))

(defun anx--get-column-type (item)
  ;; String -> String
  "Given report column ITEM, return its type."
  (gethash item *anx--columns-hash*))

(defun anx--print-dimensions-table ()
  ;; -> IO
  "Print a table of the report's dimensions in the *scratch* buffer."
  (let* ((dimensions (anx--process-dimensions))
	 (items (car (anx--assoc-val 'items dimensions)))
	 (title (cadar (anx--assoc-val 'title dimensions)))
	 (header (car (anx--assoc-val 'header (anx--process-dimensions))))
	 (header-string
	  (concat "|| " (mapconcat
			 (lambda (x) x)
			 header " || ") " ||")))
    (anx--print-to-scratch-buffer (format "\nh2. %s\n\n" title))
    (anx--print-to-scratch-buffer (format "%s\n" header-string))
    (mapc (lambda (elem)
	    (anx--print-to-scratch-buffer
	     (format *anx--report-dimensions-table-row*
		     (anx--assoc-val 'name elem)
		     (anx--assoc-val 'type elem)
		     (anx--translate-boolean (anx--assoc-val 'filter_by elem))
		     (anx--assoc-val 'description elem))))
	  items)))

(defun anx--print-metrics-table ()
  ;; Array -> IO State!
  "Print a table of the report's metrics in the *scratch* buffer."
  (let* ((metrics (anx--process-metrics))
	 (items (car (anx--assoc-val 'items metrics)))
	 (title (cadar (anx--assoc-val 'title metrics)))
	 (header (car (anx--assoc-val 'header metrics)))
	 (header-string
	  (concat "|| " (mapconcat
			 (lambda (x) x)
			 header " || ") " ||")))
    (anx--print-to-scratch-buffer (format "\nh2. %s\n\n" title))
    (anx--print-to-scratch-buffer (format "%s\n" header-string))
    (mapc (lambda (elem)
	    (anx--print-to-scratch-buffer
	     (format *anx--report-metrics-table-row*
		     (anx--assoc-val 'name elem)
		     (anx--assoc-val 'type elem)
		     ""
		     (anx--assoc-val 'description elem))))
	  items)))

(defun anx--print-report-meta (report-meta-alist)
  ;; Array -> IO State!
  "Generate report documentation from REPORT-META-ALIST.
Along the way, sets up and tears down hash tables to hold the
necessary state."
  (progn
    (anx--build-columns-hash report-meta-alist)
    (anx--build-filters-hash report-meta-alist)
    (anx--build-havings-hash report-meta-alist)
    (anx--print-dimensions-table)
    (anx--print-metrics-table)
    (anx--clear-report-hashes)))

(defun anx-really-print-report-meta ()
  ;; -> IO State!
  "Generate reporting API documentation from the current buffer.
Prints its output to the *scratch* buffer."
  (interactive)
  (let ((report-meta (read (buffer-string))))
    (anx--print-report-meta report-meta)))

(defun anx--clear-report-hashes ()
  ;; -> State!
  "Clear state hash tables used to generate documentation for reporting APIs."
  (progn (clrhash *anx--havings-hash*)
	 (clrhash *anx--columns-hash*)
	 (clrhash *anx--filters-hash*)))


;;; Part 3. Mobile SDK Error Messages

(defvar *anx--mobile-sdk-error-table-header*
  "\n|| Message || Key ||\n"
  "Format string for table columns in mobile SDK error documentation.")

(defvar *anx--android-sdk-errors* (make-hash-table :test 'equal)
  "Store the keys and messages that display in Android logs.")

(defvar *anx--ios-sdk-errors* (make-hash-table :test 'equal)
  "Store the keys and messages that display in iOS logs.")

(defun anx--clear-sdk-error-hashes ()
  ;; -> State!
  "Clean up hash tables used when generating mobile SDK error tables."
  (progn (clrhash *anx--android-sdk-errors*)
	 (clrhash *anx--ios-sdk-errors*)))

(defun anx--sdk-error:on (error-object)
  ;; Alist -> Array
  "Gets the 'on field of ERROR-OBJECT."
  (anx--assoc-val 'on error-object))

(defun anx--sdk-error:message (error-object)
  ;; Alist -> String
  "Gets the 'message field of ERROR-OBJECT."
  (anx--assoc-val 'message error-object))

(defun anx--sdk-error:key (error-object)
  ;; Alist -> String
  "Gets the 'key field of ERROR-OBJECT."
  (anx--assoc-val 'key error-object))

(defun anx--sdk-error:android-p (error-object)
  ;; Alist -> Boolean
  "Given ERROR-OBJECT, determine whether it occurs in the Android SDK."
  (let* ((devices (anx--sdk-error:on error-object))
	 (len (length devices)))
    (if (>= len 1)
	t
      nil)))

(defun anx--sdk-error:ios-p (error-object)
  ;; Alist -> Boolean
  "Given ERROR-OBJECT, determine whether it occurs in the iOS SDK."
  (let* ((devices (anx--sdk-error:on error-object))
	 (len (length devices)))
    (if (> len 1)
	t
      nil)))

(defun anx--print-sdk-error-tables (sdk-error-array)
  ;; Array -> IO State!
  "Given SDK-ERROR-ARRAY, generate documentation tables from it."
  (progn
    ;; First, build hash tables
    (mapc (lambda (e)
	      (let ((android-p (anx--sdk-error:android-p e))
		    (ios-p (anx--sdk-error:ios-p e))
		    (message (anx--sdk-error:message e))
		    (key (anx--sdk-error:key e)))
		(if android-p
		    (puthash key message *anx--android-sdk-errors*))
		(if ios-p
		    (puthash key message *anx--ios-sdk-errors*))))
	    sdk-error-array)
    ;; Then, maphash and build each table, starting with iOS
    (anx--print-to-scratch-buffer (format "\n\nh3. iOS\n"))
    (anx--print-to-scratch-buffer *anx--mobile-sdk-error-table-header*)
    (maphash (lambda (k v)
	       (anx--print-to-scratch-buffer (format "| %s | {{%s}} |\n" v k)))
	     *anx--ios-sdk-errors*)
    ;; Now for Android
    (anx--print-to-scratch-buffer (format "\n\nh3. Android\n"))
    (anx--print-to-scratch-buffer *anx--mobile-sdk-error-table-header*)
    (maphash (lambda (k v)
	       (anx--print-to-scratch-buffer (format "| %s | {{%s}} |\n" v k)))
	     *anx--android-sdk-errors*)
    (anx--clear-sdk-error-hashes)))

(defun anx-really-print-sdk-error-tables ()
  ;; -> IO State!
  "Generate mobile SDK error documentation from the current buffer."
  (interactive)
  (let ((sdk-error-array (read (buffer-string))))
    (anx--print-sdk-error-tables sdk-error-array)))

;;; Part 4. Working with existing documentation

(defvar *anx--new-fields* (make-hash-table :test 'equal)
  "Hash table for storing the fields from a newly generated API document.")

(defvar *anx--old-fields* (make-hash-table :test 'equal)
  "Hash table for storing the fields from an existing API document.")

(defun anx--hash-incf (key table)
  ;; String Hash -> State!
  "Given a KEY and TABLE, increment the value of KEY.
If KEY is not numeric (perhaps because it's not defined), give it
a value of 1."
  (let ((curval (gethash key table)))
    (if (numberp curval)
	(puthash key (+ 1 curval) table)
      (puthash key 1 table))))

(defun anx--old-fields-incf (field)
  ;; String -> State!
  "Convenience function to increment FIELD in `*anx--old-fields*'."
  (anx--hash-incf field *anx--old-fields*))

(defun anx--new-fields-incf (field)
  ;; String ->State!
  "Convenience function to increment FIELD in `*anx--new-fields*'."
  (anx--hash-incf field *anx--new-fields*))

(defun anx--clear-fields ()
  ;; -> State!
  "Convenience function for clearing the hashes that store JSON fields."
  (progn
    (clrhash *anx--old-fields*)
    (clrhash *anx--new-fields*)))

(defun anx--extract-fields (buffer &optional slurp-whole-line)
  ;; Buffer -> List
  "Make a list from all of the field names listed in BUFFER.

If optional SLURP-WHOLE-LINE is t, grab the field and all of its info.
FIXME: Optional behavior not yet implemented."
  (let ((result nil))
    (save-excursion
      (with-current-buffer buffer
	(goto-char (point-min))
	(while (re-search-forward "^| \\([A-Za-z_.]+\\)" nil t)
	  (push (match-string-no-properties 1) result))))
    result))

(defun anx--build-fields (buffer hash)
  ;; Hash Buffer -> State!
  "Add the fields from BUFFER to HASH."
  (let ((fields (anx--extract-fields buffer)))
    (mapc (lambda (x) (anx--hash-incf x hash))
	  fields)))

(defun anx--build-old-fields (buffer)
  ;; Buffer -> State!
  "A convenience function for extracting fields from BUFFER.
BUFFER should contain the old (which is to say, existing)
documentation."
  (anx--build-fields buffer *anx--old-fields*))

(defun anx--build-new-fields (buffer)
  ;; Buffer -> State!
  "A convenience function for extracting fields from BUFFER.
BUFFER should contain the new (which is to say, freshly generated)
tables ready to be filled out with documentation."
  (anx--build-fields buffer *anx--new-fields*))

(defun anx--delta-fields ()
  ;; -> Alist
  "Check the new fields table against the old, returning the delta.
Returns an alist with elements of the form (FIELD . OCCURRENCES).
Note that we currently only return a delta if the new table
contains fields the old one does not (not the other way around)."
  (let ((result nil))
    (maphash (lambda (k v)
	       (if (= (anx--old-fields-incf k) 1)
		   ;; If 1, key didn't exist in old table, so is new
		   (push (cons k (- (anx--new-fields-incf k) 1)) result)))
	     *anx--new-fields*)
    result))

(defun anx--delta-wiki-buffers (buf1 buf2)
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
      (anx--clear-fields)
      (anx--build-old-fields (get-buffer buf1))
      (anx--build-new-fields (get-buffer buf2))
      (with-current-buffer deltabuf
	(princ (anx--delta-fields) (current-buffer))
	(emacs-lisp-mode)
	(switch-to-buffer-other-window deltabuf)))))

;; Extracting fields from API responses. This should live somewhere
;; else. Right now I just need to pull it out of `anx-api'.

(defun anx-extract-meta-fields ()
  "Extract the 'fields' variable from the API response."
  (interactive)
  (let* ((it (read (buffer-string)))
	 (response (let ((json-object-type 'alist))
		     (assoc 'response it)))
	 (fields (cdr (assoc 'fields response)))
	 (bufname (concat (buffer-name) " (META FIELDS ONLY)"))
	(mode 'emacs-lisp-mode))
    (anx--pop-up-buffer bufname fields mode)))

(defun anx-extract-report-meta-fields ()
  "Extract the 'fields' variable from the reporting API response."
  (interactive)
  (let* ((it (read (buffer-string)))
	 (response (let ((json-object-type 'alist))
		     (assoc 'response it)))
	 (fields (cdr (assoc 'meta response)))
	 (bufname (concat (buffer-name) " (REPORT META FIELDS ONLY)"))
	(mode 'emacs-lisp-mode))
    (anx--pop-up-buffer bufname fields mode)))

;; Keybindings

(global-set-key (kbd "C-x C-a M") 'anx-extract-meta-fields)
(global-set-key (kbd "C-x C-a R") 'anx-extract-report-meta-fields)

;; Let's work on automatically generating Console documentation!

(defun anx--titlecase (field-name)
  ;; String -> String
  "Reformat FIELD-NAME (as used by our API) for Console documentation."
  ;; We need to wrap the body in SAVE-MATCH-DATA because SPLIT-STRING
  ;; frobs global regexp match state.
  (save-match-data
    (mapconcat 
     (lambda (x) x)
     (mapcar
      (lambda (word) (capitalize word))
      (split-string field-name "_")) " ")))

(provide 'anx-docgen)

;;; anx-docgen.el ends here
