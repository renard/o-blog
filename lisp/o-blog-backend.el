;;; o-blog-backend.el --- Base class for o-blog backends

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-12-04
;; Last changed: 2013-01-22 01:13:01
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
	     :documentation "")
   (pages :initarg :pages
	  :type list
	  :documentation "")
   (snippets :initarg :snippets
	     :type list
	     :documentation ""))

  
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


(provide 'o-blog-backend)

;; o-blog-backend.el ends here
