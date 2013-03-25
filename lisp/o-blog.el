;;; o-blog.el --- 

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-12-03
;; Last changed: 2013-02-09 11:18:04
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(defun o-blog-guess-backend-from-file (file)
  "Try to guess o-blog back from FILE or current buffer."
  (with-current-buffer (or (get-file-buffer file)
			   (find-file-noselect file))
    ;; Assume all mode end with "-mode"
    (let ((type (intern (substring
			 (symbol-name major-mode) 0 -5))))
      (intern (format "ob:backend:%s" type)))))

;;;###autoload
(defun o-blog-publish(&optional file backend)
  "Publish FILE using o-blog BACKEND.

If FILE is not provided, try to guess FILE and BACKEND from
current buffer."
  (interactive)
  (let* ((file
	  (or
	   file
	   (buffer-file-name)
	   (read-file-name "O-blog file to publish: " nil nil t)))
	 (backend (or
		   backend
		   (o-blog-guess-backend-from-file file)))
	 (blog (funcall backend file)))
    (ob:parse-config blog)
    (ob:parse-entries blog)
    (loop for type in '(articles pages snippets)
	  do (loop for entry in (slot-value blog type)
		   do (ob:convert-entry blog entry)))
    blog))

  
(defun ob:parse-blog-config (&optional file type)
  ""
  (interactive)
  
  (let* ((type (or (intern (format "ob:blog:%s" type)) 'ob:blog))
	 (file (or
		file
		(read-file-name "Blog configuration file: " nil nil t)))
	 (blog (funcall type file))
	 (lines (split-string
		 (with-temp-buffer
		   (insert-file-contents file)
		   (buffer-string))
		 "\n")))
    (save-match-data
      (loop for line in lines
	    when (string-match "^\\s-*\\([^#]+?\\)\\s-*=\\s-*\\(.+?\\)\\s-*$" line)
	    do (let ((k (intern (match-string 1 line)))
		     (v (match-string 2 line)))
		 (when (slot-exists-p blog k)
		   (set-slot-value blog k v)))))
    blog))





(provide 'o-blog)

;; o-blog.el ends here
