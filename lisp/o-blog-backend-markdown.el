;;; o-blog-backend-markdown.el --- Markdown backend for o-blog

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-08-22
;; Last changed: 2014-10-01 01:37:51
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'o-blog-backend)
  (require 'markdown-mode))



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


(defun ob:markdown:parse-file-as-entry (file)
  "Try to load FILE as a blog entry."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((headers (ob:markdown:get-headers))
	   (type (intern (plist-get headers 'type)))
	   (obj (funcall (intern (format "make-ob:%s" type))
			 :file file
			 :source-file file
			 :source (buffer-substring-no-properties
				  (point-min) (point-max)))))

      ;; Compute timestamp
      (when (ob:slot-exists-p obj 'timestamp)
	(%ob:set obj 'timestamp
		 (condition-case nil
		     (apply #'encode-time
			    (map 'list #'(lambda(x) (or x 0))
				 (parse-time-string
				  (plist-get headers 'timestamp))))
		   (nth 5 (file-attributes file))))
	(ob:entry:compute-dates obj))
      
      ;; compute tags
      (when (ob:slot-exists-p obj 'tags)
	(%ob:set obj 'tags
		 (ob:markdown:parse-tags (plist-get headers 'tags))))

      ;; compute category
      (when (ob:slot-exists-p obj 'category)
	(%ob:set obj 'category
		 (make-ob:category
		  (plist-get headers 'category))))

      ;; Add rest of header parameters to object
      (loop for header in headers by #'cddr
	    when (and
		  (ob:slot-exists-p obj header)
		  (not (member header '(tags timestamp category))))
	    do (%ob:set
		obj header (plist-get headers header)))

      (when (eq type 'page)
	(unless (ob:get 'page obj)
	  (%ob:set obj 'page
		   (ob:sanitize-string (ob:get 'title obj)))))

      ;; Compute object paths
      (ob:entry:set-path obj)

      ;; Find extra files to copy
      (let ((default-directory
	      (format "%s/%s" default-directory
		      (or
		       (file-name-directory (ob:get 'source-file obj))))))
	(%ob:set obj 'files-to-copy
		 (ob:markdown:get-images)))

      obj)))


(defun ob:markdown:parse-entries (backend)
  (loop for f in (ob:get 'source-files backend)
	when (file-exists-p f)
	do
	(let ((cache-file (format "%s/%s.cache"
				  (ob:get 'cache-dir backend)
				  (file-name-sans-extension f))))


	  (let* ((entry
		  (if (and (file-exists-p cache-file)
			   (time-less-p (nth 5 (file-attributes f))
					(nth 5 (file-attributes cache-file))))
		      (with-temp-buffer
			(insert-file-contents cache-file)
			(car (read-from-string (buffer-string))))
		    (ob:markdown:parse-file-as-entry f)))
		   (class (%ob:get-type entry))
		   (class-type (intern (format "%ss" (substring-no-properties
						      (symbol-name class) 3))))
		   (obj-list (ob:get class-type backend)))
	      (%ob:set entry 'cache-file cache-file)
	      (%ob:set backend class-type (append obj-list (list entry)))))))


(defun ob:markdown:convert-entry (self entry)
  ""
  (with-temp-buffer
    (insert (ob:get 'source entry))
    (let ((buff-src (current-buffer))
	  (default-directory (format "%s/%s"
				     default-directory
				     (file-name-directory
				      (ob:get 'source-file entry))))
	  html)
      (ob:framework-expand "<\\([a-z][a-z0-9-]*\\)\\([^>]+\\)?>"  "</%s>" "" "" "#" entry)
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


(defun ob:markdown:get-images ();;object)
  ""
  ;; (with-temp-buffer
  ;;   (insert (ob:get 'source object))
  (save-excursion
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
