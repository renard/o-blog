;;; o-blog-entry.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-01-21
;; Last changed: 2014-07-11 17:31:26
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile
  (require 'htmlize nil t)
  (require 'sgml-mode nil t)
  (require 'eieio nil t))


(defclass ob:entry ()
  ((id :initarg :id
       :type integer
       :documentation "entry ID")
   (title :initarg :title
	  :type string
	  :documentation "")
   (source :initarg :source
	   :type string
	   :documentation "Article raw source")
   (html :initarg :html
	 :type string
	 :documentation "Article html result")
   (path :initarg :path
	   :type string
	   :documentation "Path to article publication")
   (file :initarg :file
	   :type string
	   :documentation "")
   (files-to-copy :initarg :files-to-copy
		  :type list
		  :documentation "List of files to copy when publishing")
   (tags :initarg :tags
	 :type list
	 :documentation "List of ob:tag")

   (excerpt :initarg :excerpt
	 :type string
	 :documentation "small entry extract")

   (htmlfile :initarg :htmlfile
	   :type string
	   :documentation "")
   (path-to-root :initarg :path-to-root
		 :type string
		 :documentation "")
   )
  "Object type handeling o-blog entries.")




;; Class aliases

(defclass ob:article (ob:entry)
  ((timestamp :initarg :timestamp
	      :type list
	      :documentation "")
   (year :initarg :year
	 :type integer
	 :documentation "")
   (month :initarg :month
	 :type integer
	 :documentation "")
   (day :initarg :day
	 :type integer
	 :documentation "")
   (category :initarg :category
	     ;;	   :type ob:category
	   :documentation "")
   (template :initarg :template
	     :initform "blog_post.html"
	     :type string
	     :documentation "Template file to use for publication")
   )
  "O-blog page article")

(defmethod ob:entry:set-path ((self ob:entry))
  ""
  (set-slot-value
   self 'path (format "%s"
		      (ob:sanitize-string (oref self title))))
  (set-slot-value
   self 'htmlfile (format "%s/%s"
		      (oref self path)
		      (oref self path))))

(defmethod ob:entry:compute-dates ((self ob:article))
  ""
  (let ((timestamp (oref self timestamp)))
    (when timestamp
      (loop for (p f) in '((year "%Y") (month "%m") (day "%d"))
	    do (set-slot-value
		self p
		(string-to-number
		 (format-time-string f timestamp)))))))

(defmethod ob:entry:set-path ((self ob:article))
  ""
  (set-slot-value
   self 'path (format "%s/%.4d/%.2d"
		      (ob:sanitize-string (oref (oref self category) safe))
		      (oref self year)
		      (oref self month)))
  (set-slot-value
   self 'file (format "%.2d_%s.html"
		      (oref self day)
		      (ob:sanitize-string (oref self title))))
  (set-slot-value
   self 'htmlfile (if (string= "." (oref self path))
		      (oref self file)
		    (format "%s/%s"
			    (oref self path)
			    (oref self file))))
  (set-slot-value
   self 'path-to-root (file-relative-name
		       "."
		       (oref self path))))

(defmethod ob:get-post-excerpt ((self ob:entry) &optional words ellipsis)
  "Return the first WORDS from POST html content.

The return string would be unformatted plain text postfixed by
ELLIPSIS if defined.."
  (when (slot-exists-p self 'excerpt)
    (with-temp-buffer
      (insert (ob:get 'html self))
      (let ((words (or words 20))
	    (ellipsis (or ellipsis "…"))
	    (html2text-remove-tag-list
	     (loop for tag in html-tag-alist
		   collect (car tag))))
	(html2text)
	(goto-char (point-min))
	;; remove all comments
	(save-excursion
	  (save-match-data
	    (while (search-forward-regexp "<!--" nil t)
	      (let ((start (- (point) 4)))
		(save-match-data
		  (search-forward-regexp "-->"))
		(delete-region start (point))))))
	(save-excursion
	  (save-match-data
	    (while (search-forward-regexp "\\(\\s-*\n\\s-*\\)" nil t)
	      (delete-region (- (point) (length (match-string 0))) (point))
	      (insert " "))))
	(loop for x from 0 below words do (forward-word))
	(set-slot-value self 'excerpt
			(concat
			 (buffer-substring-no-properties
			  (point-min) (point))
			 ellipsis))))))


(defmethod ob:entry:publish ((self ob:entry) &optional blog-obj)
  ""
  (let ((blog-obj (or blog-obj
		      (when (boundp 'BLOG) BLOG)
		      (when (boundp 'blog) blog)))
	(FILE (oref self htmlfile)))
    (unless (ob:backend-child-p blog-obj)
      (error "`ob:entry:publish': blog-obj is not an `ob:backend' child class."))
    (when (slot-exists-p self 'template)
      (with-temp-buffer
	(ob:insert-template (oref self template))
	(message "Write to: %s"
		 (expand-file-name (format "%s/%s"
					   (oref BLOG publish-dir)
					   FILE)))
	(ob:write-file (format "%s/%s"
			       (oref BLOG publish-dir)
			       FILE)))

      (loop for file in (ob:get 'files-to-copy self)
	    do (ob-do-copy (format "%s/%s"
				   (or
				    (file-name-directory (ob:get-name self)) ".")
				   file)
			   (format (format "%s/%s"
					   (oref BLOG publish-dir)
					   (oref self path)))))

      ;; (with-temp-buffer
      ;; 	(insert (oref self html))
      ;; 	(ob:write-file (format "%s/%s.txt"
      ;; 			       (oref BLOG publish-dir)
      ;; 			       (oref self htmlfile))))
      )))

(defclass ob:page (ob:entry)
  ((template :initarg :template
	     :initform "blog_static.html"
	     :type string
	     :documentation "Template file to use for publication"))
  "O-blog page class")

(defmethod ob:entry:set-path ((self ob:page) page)
  ""
  (set-slot-value self 'path ".")
  (set-slot-value self 'file page)
  (set-slot-value
   self 'htmlfile (if (string= "." (oref self path))
		      (oref self file)
		    (format "%s/%s"
			    (oref self path)
			    (oref self file))))
  (set-slot-value
   self 'path-to-root (file-relative-name
		       "."
		       (oref self path))))

(defclass ob:snippet (ob:entry)
  nil
  "O-blog snippet class")



(defun ob:entry:get (value &optional entry)
  ""
  (let ((entry (or entry
		   (when (boundp 'POST) POST))))
    (slot-value entry value)))


(defun ob:entry:sort-by-date (a b)
  "Sort both A and B posts by date (newer posts first)."
  (> (float-time (ob:get 'timestamp a))
     (float-time (ob:get 'timestamp b))))

(defun ob:get-post-by-id (id)
  "Return post which id is ID"
  (when (>= id 0)
    (nth id POSTS)))



(provide 'o-blog-entry)

;; o-blog-entry.el ends here
