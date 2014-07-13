;;; o-blog-backend-markdown.el --- Markdown backend for o-blog

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-08-22
;; Last changed: 2014-07-13 21:16:43
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'cl nil t)
  (require 'o-blog-backend nil t)
  (require 'markdown-mode nil t))



(cl-defstruct (ob:backend:markdown
	       (:include ob:backend)))

(defun ob:markdown:find-files (self)
  (%ob:set self 'source-files (ob:find-files self "txt")))




(defun ob:markdown:parse-tags (tags)
  "Parse TAGS and generate a list of `ob:tags'."
  (let ((tags (if (listp tags)
		  tags (split-string tags "\\s-?,\\s-"))))
    (loop for tag in tags
	  for td = (ob:replace-in-string tag '(("_" " ") ("@" "-")))
	  collect (make-ob:tag td))))


(defun ob:markdown:parse-entries (self)
  (loop for f in (ob:get 'source-files self)
	do (with-temp-buffer
	     (insert-file-contents f)
	     (let* ((headers (ob:markdown:get-headers))
		    (type (intern (plist-get headers 'type)))
		    (types (intern (format "%ss" type)))
		    (obj (funcall (intern (format "make-ob:%s" type))
			  :file f
			  :source (buffer-substring-no-properties
				   (point-min) (point-max))))
		    (obj-list (ob:get types self)))
	       
	       (when (ob:slot-exists-p obj 'timestamp)
		 (%ob:set obj 'timestamp
			 (nth 5 (file-attributes f)))
		 (ob:entry:compute-dates obj))

	       (when (ob:slot-exists-p obj 'tags)
		 (%ob:set obj 'tags
			  (ob:parse-tags (plist-get headers 'tags))))

	       (when (ob:slot-exists-p obj 'category)
		 (%ob:set obj 'category
			  (make-ob:category
			   (plist-get headers 'category))))

	       ;; Add rest of header parameters to object
	       (loop for header in headers by #'cddr
		     when (and
			   (ob:slot-exists-p obj header)
			   (not (member header '(tags category))))
		     do (%ob:set
			 obj header (plist-get headers header)))

	       (when (eq type 'page)
		 (unless (ob:get 'page obj)
		   (%ob:set obj 'page
			    (ob:sanitize-string (ob:get 'title obj)))))
	      
	       (ob:entry:set-path obj)
	       
	       (%ob:set self types (append obj-list (list obj)))

	       (%ob:set obj 'files-to-copy
			(ob:markdown:get-images obj))))))




(defun ob:markdown:convert-entry (self entry)
  ""
  (with-temp-buffer
    (insert (ob:get 'source entry))
	(ob:framework-expand "<\\([a-z][a-z0-9-]*\\)\\([^>]+\\)?>"  "</%s>" "" "" "#")
    (let ((buff-src (current-buffer))
	  html)
      (with-temp-buffer
	(let ((buff-out (current-buffer)))
	  (with-current-buffer buff-src
	    (call-process-region (point-min) (point-max) "pandoc" nil buff-out))
	  (setf html (buffer-substring-no-properties (point-min)(point-max)))))
      (with-temp-buffer
	(insert html)

	(%ob:set
	 entry 'html (buffer-substring-no-properties (point-min)(point-max))))
      (ob:get-post-excerpt entry))))


(defun ob:markdown:get-images (object)
  ""
  (with-temp-buffer
    (insert (ob:get 'source object))
    (goto-char (point-min))
    (let (files)
      ;; images
      (save-match-data
	(while (search-forward-regexp "\\]\\(([^)]+)\\)" nil t)
	  (let ((link (match-string-no-properties 1)))
	    (with-temp-buffer
	      (insert link)
	      (goto-char (point-min))
	      (save-match-data
		(while (search-forward-regexp "(\\(.+?\\)\\(?: \"[^\"]*\"\\)?)"
					      nil t)
		  (let ((file  (match-string-no-properties 1)))
		    (when (file-exists-p file)
		      (push file files)))))))))
      files)))


(defun ob:markdown:get-headers ()
  "Return all headers from a markdown file.

Headers must be defined within the first Comment block enclosed
into \"<!--/\" \"/-->\"."
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
	(let* ((comment-start "<!--/")
	       (comment-end "/-->")
	       (start (when (search-forward comment-start nil t)
			(backward-char (length comment-start))
			(point)))
	       (end (when (search-forward comment-end nil t)
		      (point))))
	  (when (and start end)
	    
	    (loop for line in (split-string
			       (buffer-substring-no-properties start end)
			       "\n")
		  when (string-match
			"^\\s-*\\([^#]+?\\)\\s-*\\(?:[=:]\\)\\s-*\\(.+?\\)\\s-*$" line)
		  nconc (let ((k (intern (match-string 1 line)))
			      (v (match-string 2 line)))
			 (list k v)))))))))


(ob:register-backend
 (%ob:get-type (make-ob:backend:markdown))
 :find-files 'ob:markdown:find-files
 :parse-config 'ob:parse-config
 :parse-entries 'ob:markdown:parse-entries
 :convert-entry 'ob:markdown:convert-entry)


(provide 'o-blog-backend-markdown)

;; o-blog-backend-markdown.el ends here
