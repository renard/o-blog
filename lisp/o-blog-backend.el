;;; o-blog-backend.el --- Base class for o-blog backends

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-12-04
;; Last changed: 2013-02-09 19:45:32
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile
  (require 'eieio nil t))


(defclass ob:backend nil
  ((index-file :initarg :index-file
	       :type string
	       :documentation "Path to o-blog index file.")
   (source-dir :initarg :source-dir
		:type string
		:documentation "Path to publishing
		directory (relative to o-blog configuration file
		path).")
   (source-files :initarg :file-name
		 :type list
		 :documentation "List of o-blog source files")
   (publish-dir :initarg :publish-dir
		:type string
		:documentation "Path to publishing
		directory (relative to o-blog configuration file
		path).")
   (template-dir :initarg :publish-dir
		 :type string
		 :documentation "Path to publishing
		 directory (relative to o-blog configuration file
		 path).")
   (articles :initarg :articles
	     :type list
	     :documentation "List of ob:article")
   (pages :initarg :pages
	  :type list
	  :documentation "List of ob:page")
   (snippets :initarg :snippets
	     :type list
	     :documentation "List of ob:snippet")
   (tags :initarg :tags
	 :type list
	 :documentation "List of ob:tag"))
  
  "Object type handeling o-blog backend. All file or directory
paths are relative to the o-blog configuration file."
  :abstract t)

(defmethod ob:publish ((self ob:backend))
  "Publish a new blog."
  (ob:find-files self)
  (ob:parse-config self))

(defmethod ob:get-name ((self ob:backend))
  "Return class name"
  (aref self object-name))



;; Useful functions / macros

(defmacro ob:with-source-buffer (self &rest body)
  "Like `with-current-buffer'"
  `(let ((file (ob:get-name ,self)))
     ;; Make sure we are in the o-blog org file
     (with-current-buffer (or (get-file-buffer file)
			     (find-file-noselect file))
       ,@body)))
      

(defmethod ob:must-override-1 ((self ob:backend) method)
  "Generate a warning when METHOD is not overridden in subclass."
  (message "Method `%s' is not defined in class `%s'."
	   method (object-class self)))

(defmethod ob:find-files-1 ((self ob:backend) extension
			    &optional dir ignore-dir original-dir)
  ""
  (let* ((dir (or dir (ob:get-source-directory self)))
	 (original-dir (or original-dir dir))
	 (extension (if (listp extension) extension (list extension)))
	 (ignore-dir (or ignore-dir '(".." "." ".git"))))
    (loop for file in (directory-files dir nil nil t)
	  for file-path = (format "%s%s"
				  (file-name-as-directory dir) file)
	  when (and
		(file-directory-p file-path)
		(not (member file ignore-dir)))
	  nconc (ob:find-files-1 self extension file-path ignore-dir original-dir)
	  when (member (file-name-extension file) extension)
	  collect (file-relative-name file-path original-dir))))


;;
;; Methods that must be overridden
;;
(defmethod ob:find-files ((self ob:backend))
  "Generic method for finding files. This method MUST be
overriden in subclasses."
  (ob:must-override-1 self 'ob:find-files))

(defmethod ob:parse-config ((self ob:backend))
  "Parse blog configuration. This method MUST be
overriden in subclasses."
  (ob:must-override-1 self 'ob:parse-config))


;; Basic primitives
(defmethod ob:get-configuration-file ((self ob:backend))
  "Return o-blog configuration file, which is SELF instance name."
  (aref self object-name))

(defmethod ob:get-source-directory ((self ob:backend))
  "Return o-blog source directory from ob:backend SELF object."
  (file-name-as-directory
   (format "%s%s"
	   (file-name-as-directory
	    (file-name-directory (ob:get-configuration-file self)))
	   (if (and
		(slot-exists-p self 'source-dir)
		(slot-boundp self 'source-dir))
	       (oref self source-dir)
	     ""))))


(defmethod ob:get-all-posts ((self ob:backend))
  "Generic method for finding files. This method MUST be
overriden in subclasses."
  (ob:must-override-1 self 'ob:get-all-posts))


(defmethod ob:compute-tags ((self ob:backend) &optional min_r max_r)
  "Return a sorted list of all ob:tags by name.

Compute tag occurrence and their HTML percentage value.

MIN_R and MAX_R are the minimum and maximum percentage value. If
not provided 80 and 220 are used. This means ob:size is always
within MIN_R and MAX_R inclusive."
  (let* ((tags (sort
		(loop for article in (slot-value self 'articles)
		      append (slot-value article 'tags))
		#'(lambda (a b) (string< (aref a object-name) 
					 (aref b object-name)))))
	 (min_r (or min_r 80))
	 (max_r (or max_r 220))
	 (min_f (length tags))
	 (max_f 0))
    (set-slot-value
     self 'tags
     (loop for (count tag) in
    	   ;; Here extract uniq tags and count occurrences
    	   ;; (such as uniq -c does)
    	   ;; Each item of returned list is
    	   ;; (VALUE COUNT)
    	   ;; See http://stackoverflow.com/a/6055795
    	   (loop for (i . j) on tags
    		 with k = 1
    		 when (and
    		       j
    		       (string= (aref i object-name)
    				(aref (car j) object-name)))
    		 do (incf k)
    		 else
    		 collect (progn
    			   (when (> k max_f) (setf max_f k))
    			   (when (< k min_f) (setf min_f k))
    			   (list k i))
		 and do (setf k 1))
    	   do (progn
    		(set-slot-value tag 'count count)
    		;; This is the tricky part
    		;; Formula is:
    		;; % = min_r + (count - min_f) * (max_r - min_r) / (max_f - min_f)
    		;; the `max' is on purpose in case of max_f = min_f
    		(set-slot-value tag 'size
    				(+ min_r
    				   (/
    				    (* (- count min_f) (- max_r min_r))
    				    (max 1.0 (float (- max_f min_f)))))))
    	   collect tag))))


(provide 'o-blog-backend)

;; o-blog-backend.el ends here
