;;; o-blog-tag.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-02-09
;; Last changed: 2014-10-07 00:32:24
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 

;;; Code:

(require 'cl)

(cl-defstruct
    (ob:category
     (:constructor
      make-ob:category (display &aux (safe (ob:sanitize-string display)))))
  display
  safe)

(cl-defstruct
    (ob:tag
     (:include ob:category)
     (:constructor
      make-ob:tag (display &key (count 0) (size 0)
			   &aux (safe (ob:sanitize-string display)))))
  count
  size)


(defun ob:tag:publish (tags blog)
  "Publish all TAGS from BLOG"
  ;; publish tags
  (let ((PATH-TO-ROOT ".."))
    (let ((FILE "tags/index.html"))
      (ob:eval-template-to-file "blog_tags.html"
				(format "%s/%s"
					(ob:get 'publish-dir blog)
					FILE)))
    (loop for TAG in TAGS
	  do
	  (let ((FILE (format "tags/%s.html" (ob:get 'safe TAG))))
	    (ob:eval-template-to-file "blog_tags-details.html"
				      (format "%s/%s"
					      (ob:get 'publish-dir blog)
					      FILE))
	    (let ((POSTS (ob:get-posts  (lambda (x)
					  (member (ob:get 'safe TAG)
						  (mapcar 'ob:tag-safe
							  (ob:post-tags x)))))))
	      (ob:eval-template-to-file "blog_rss.html"
					(format "%s/tags/%s.xml"
						(ob:get 'publish-dir BLOG)
						(ob:get 'safe TAG)))))))
  
    ;; Write tags JSON
    (with-temp-file
	(format "%s/%s" (ob:get 'publish-dir BLOG) "tags.js")
      (insert "{\"tags\":[")
      (if (not TAGS)
	  (insert " ")
	(loop for TAG in TAGS
	      do (insert
		  (format
		   "{\"size\":\"%.2f\",\"path\":\"tags/%s.html\",\"tag\":\"%s\"},"
		   (ob:get 'size TAG) (ob:get 'safe TAG) (ob:get 'display TAG))))
	;; remove last comma
	(delete-char -1))
      (insert "]}")))


  


(provide 'o-blog-tag)

;; o-blog-tag.el ends here
