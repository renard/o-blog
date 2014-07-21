;;; o-blog-entry.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-01-21
;; Last changed: 2014-07-21 22:29:40
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile
  (require 'html2text)
  (require 'sgml-mode)
  (require 'cl))

(cl-defstruct
    (ob:entry)
  "Object type handling o-blog entries. `ob:entry` is the base
structure for `ob:page` and `ob:article`.

Slots are:

- `id`: the numerical ID of the entry.

- `title`: the entry title.

- `source`: raw entry source.

- `html`: html converted entry.

- `path`: the entry path for publication.

- `file`: the entry source file location.

- `files-to-copy`: List of files to copy when publishing

- `tags`: list of `ob:tag`.

- `excerpt`: small entry excerpt.

- `htmlfile` path to html file.

- `path-to-root`: path to site root directory relative from
  current entry.

"
  id
  title
  source
  html
  path
  file
  files-to-copy
  tags
  excerpt
  htmlfile
  path-to-root)
  






;; Class aliases


(cl-defstruct
    (ob:article (:include ob:entry))
  timestamp
  year
  month
  day
  category
  (template "blog_post.html"))


(cl-defstruct
    (ob:page
     (:include ob:entry))
  (template "blog_static.html")
  page)

(cl-defstruct
    (ob:snippet (:include ob:entry)))



(defun ob:entry:set-path-entry (self)
  ""
  (%ob:set
   self 'path (format "%s"
		      (ob:sanitize-string (ob:get 'title self))))
  (%ob:set
   self 'htmlfile (format "%s/%s"
		      (ob:get 'path self)
		      (ob:get 'path self))))

(defun ob:entry:compute-dates (self)
  ""
  (let ((timestamp (ob:get 'timestamp self)))
    (when timestamp
      (loop for (p f) in '((year "%Y") (month "%m") (day "%d"))
	    do (%ob:set
		self p
		(string-to-number
		 (format-time-string f timestamp)))))))

(defun ob:entry:set-path-article (self)
  ""
  (%ob:set
   self 'path (format "%s/%.4d/%.2d"
		      (ob:sanitize-string (ob:get 'safe (ob:get 'category  self)))
		      (ob:get 'year  self)
		      (ob:get 'month  self)))
  (%ob:set
   self 'file (format "%.2d_%s.html"
		      (ob:get 'day  self)
		      (ob:sanitize-string (ob:get 'title  self))))
  (%ob:set
   self 'htmlfile (if (string= "." (ob:get 'path  self))
		      (ob:get 'file  self)
		    (format "%s/%s"
			    (ob:get 'path  self)
			    (ob:get 'file  self))))
  (%ob:set
   self 'path-to-root (file-relative-name
		       "."
		       (ob:get 'path self))))

(defun ob:get-post-excerpt (self &optional words ellipsis)
  "Return the first WORDS from POST html content.

The return string would be unformatted plain text postfixed by
ELLIPSIS if defined.."
  (when (ob:slot-exists-p self 'excerpt)
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
	(%ob:set self 'excerpt
		 (concat
		  (buffer-substring-no-properties
		   (point-min) (point))
		  ellipsis))))))


(defun ob:entry:publish (self &optional blog-obj)
  ""
  (let ((blog-obj (or blog-obj
		      (when (boundp 'BLOG) BLOG)
		      (when (boundp 'blog) blog)))
	(FILE (ob:get 'htmlfile self)))
    (when (ob:slot-exists-p self 'template)
      (with-temp-buffer
	(ob:insert-template (ob:get 'template self) blog-obj)
	(message "Write to: %s"
		 (expand-file-name (format "%s/%s"
					   (ob:get 'publish-dir BLOG)
					   FILE)))
	(ob:write-file (format "%s/%s"
			       (ob:get 'publish-dir BLOG)
			       FILE)))

      (loop for file in (ob:get 'files-to-copy self)
	    do (ob-do-copy (format "%s/%s"
				   (or
				    (file-name-directory (ob:get 'file self)) ".")
				   file)
			   (format (format "%s/%s"
					   (ob:get 'publish-dir BLOG)
					   (ob:get 'path self))))))))


(defun ob:entry:set-path-page (self)
  ""
  (%ob:set self 'path (directory-file-name
		       (or
			(file-name-directory (ob:get 'page self))
			".")))
  (%ob:set self 'file (concat
		       (ob:sanitize-string
			(or (file-name-nondirectory (ob:get 'page self))
			    (ob:get 'title self)))
		       ".html"))
  (%ob:set
   self 'htmlfile (if (string= "." (ob:get 'path self))
		      (ob:get 'file self)
		    (format "%s/%s"
			    (ob:get 'path self )
			    (ob:get 'file self))))
  (%ob:set
   self 'path-to-root (file-relative-name
		       "."
		       (ob:get 'path self))))


(defun ob:entry:set-path (self)
  (let ((class (%ob:get-type self)))
    (cond
     ((eq 'ob:entry class) (ob:entry:set-path-entry self))
     ((eq 'ob:page class) (ob:entry:set-path-page self))
     ((eq 'ob:article class) (ob:entry:set-path-article self)))))


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
