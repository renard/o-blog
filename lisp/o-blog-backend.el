;;; o-blog-backend.el --- Base class for o-blog backends

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-12-04
;; Last changed: 2014-07-07 23:26:14
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile
  (require 'find-func nil t)
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
		:initform "out"
		:type string
		:documentation "Path to publishing
		directory (relative to o-blog configuration file
		path).")
   (style-dir :initarg :style-dir
		:initform "../templates/style"
		:type string
		:documentation "Path to style
		directory (relative to publish-dir).")
   (template-dir :initarg :publish-dir
		 :initform (expand-file-name
			    (concat (file-name-directory
				     (find-library-name "o-blog"))
				    "../templates"))
		 :type string
		 :documentation "Path to publishing
		 directory (relative to o-blog configuration file
		 path).")
   (articles :initarg :articles
	     :type list
	     :initform nil
	     :documentation "List of ob:article")
   (pages :initarg :pages
	  :type list
	  :initform nil
	  :documentation "List of ob:page")
   (snippets :initarg :snippets
	     :type list
	     :initform nil
	     :documentation "List of ob:snippet")
   (tags :initarg :tags
	 :type list
	 :initarg nil
	 :documentation "List of ob:tag")
   (title :initarg :title
	  :type string
	  :documentation "Site title")
   (description :initarg :description
		:type string
		:documentation "Site description")
   (url :initarg :url
		:type string
		:documentation "Site url")

   (posts-sorter :initarg :posts-sorter
		 :initform ob:entry:sort-by-date
		 :type symbol
		 :documentation)

   )
  
  "Object type handeling o-blog backend. All file or directory
paths are relative to the o-blog configuration file."
  :abstract t)

(defmethod ob:publish ((self ob:backend))
  "Publish a new blog.

Some global variables are set:
- `BLOG': the full `ob:backend' child class.
- `POSTS': a list of all sorted posts.
- `TAGS': a list of all tags as `ob:tags' class.

"
  (ob:find-files self)
  (ob:parse-config self)
  (ob:parse-entries self)
  (ob:compute-tags self)
  
  (let* ((BLOG self)
	 (PAGES (ob:get 'pages BLOG))
	 (POSTS (ob:get 'articles BLOG))
	 (ALL-POSTS POSTS)
	 (TAGS  (ob:get 'tags BLOG)))

    ;; Convert each entry to HTML format
    (loop for type in '(snippets articles pages)
	  do (loop for entry in (slot-value BLOG type)
		   with id = 0
		   do (progn
			(when (eq type 'articles)
			  (set-slot-value entry 'id id)
			  (setf id (1+ id)))
			(ob:convert-entry BLOG entry))))

    ;; Publish both articles static pages
    (loop for type in '(articles pages)
	  do (loop for POST in (slot-value BLOG type)
		   do (ob:entry:publish POST)))


    ;; publish tags
    (let ((PATH-TO-ROOT ".."))
      (let ((FILE "tags/index.html"))
	(ob:eval-template-to-file "blog_tags.html"
				  (format "%s/%s"
					  (ob:get 'publish-dir BLOG)
					  FILE)))
      (loop for TAG in TAGS
	    do
	    (let ((FILE (format "tags/%s.html" (ob:get 'safe TAG))))
	      (ob:eval-template-to-file "blog_tags-details.html"
					(format "%s/%s"
						(ob:get 'publish-dir BLOG)
						FILE))
	      (let ((POSTS (ob:get-posts  (lambda (x)
					    (member (ob:tags-safe TAG)
						    (mapcar 'ob:tags-safe
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
      (insert "]}"))

    ;; Write articles JSON
    (with-temp-file
	(format "%s/%s" (ob:get 'publish-dir BLOG) "articles.js")
      (insert "{\"articles\":{ ")
      (message "LEN: %s / %s: %s"
	       (length (ob:get-posts nil nil nil 'category))
	       (length (ob:get-posts))
	       (ob:get-posts nil nil nil 'category))
      
      (loop for CATEGORY in (ob:get-posts nil nil nil 'category)
	    do (progn
		 (insert (format "\"%s\":[ " (ob:get 'safe CATEGORY)))
		 (loop for article in
		       (ob:get-posts (lambda (x)
		  		       (equal CATEGORY
					      (ob:get 'category x))))
		       do (insert
		  	   (format
		  	    "{\"title\":%S,\"path\":%S,\"excerpt\":%S},"
			    (ob:get 'title article)
		  	    (ob:get 'htmlfile article)
			    (ob:get 'excerpt article)
			    )))
		 ;; remove last comma
		 (delete-char -1)
		 (insert "],")))
      ;; remove last comma
      (delete-char -1)
      (insert "}}"))

    
    (let ((BREADCRUMB "Archives"))
      (let ((FILE "archives.html"))
	(ob:eval-template-to-file "blog_archives.html"
				  (format "%s/%s"
					  (ob:get 'publish-dir BLOG)
					  FILE))))
    
    (ob:eval-template-to-file "blog_rss.html"
			      (format "%s/index.xml"
				      (ob:get 'publish-dir BLOG)))

    (ob:eval-template-to-file "blog_sitemap.html"
			      (format "%s/sitemap.xml"
				      (ob:get 'publish-dir BLOG)))

    (loop for CATEGORY in (ob:get-posts nil nil nil 'category)
	  with PATH-TO-ROOT = ".."
	  do
	  (loop for YEAR in (ob:get-posts
			     (lambda (x)
			       (equal CATEGORY (ob:get 'category x)))
			     nil nil 'year)
		with PATH-TO-ROOT = "../.."
		do
		(loop for MONTH in (ob:get-posts
				    (lambda (x)
				      (and
				       (equal CATEGORY (ob:get 'category x))
				       (= YEAR (ob:get 'year x))))
				    nil nil 'month)
		      with PATH-TO-ROOT = "../../.."
		      do (ob-process-index "blog_index_month.html" CATEGORY YEAR MONTH))
		and do (ob-process-index "blog_index_year.html" CATEGORY YEAR))
	  and do (unless (equal "." CATEGORY)
		   (ob:eval-template-to-file "blog_rss.html"
					     (format "%s/%s/index.xml"
						     (ob:get 'publish-dir BLOG)
						     (ob:get 'safe CATEGORY)))
		   (ob-process-index "blog_index_category.html" CATEGORY)))
    )
  (ob:publish-style self)
  self)

(defun ob-process-index (template &optional category year month)
  "Low-level function for `ob-write-index'.

Template is read from TEMPLATE file.

If provided CATEGORY YEAR and MONTH are used to select articles."
  (let* ((FILE (format "%s/index.html"
		       (cond
			((and category year month)
			 (format "%s/%.4d/%.2d"
				 (ob:get 'safe category)
				 year month))
			((and category year)
			 (format "%s/%.4d"
				 (ob:get 'safe category)
				 year))
			(t (ob:get 'safe category)))))
	 (fp (format "%s/%s" (ob:get 'publish-dir BLOG) FILE))

	 (POSTS (ob:get-posts
		 (lambda (x)
		   (and
		    (if category (equal category (ob:get 'category x)) t)
		    (if year (= year (ob:get 'year x)) t)
		    (if month (= month (ob:get 'month x)) t))))))
    (ob:eval-template-to-file template fp)))





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
  "List all files under and `ob:backend' `source-dir' or DIR
whose extensions are defined in EXTENSION list.

IGNORE-DIR is a list of directory to ignore, by default (\"..\"
\".\" \".git\")

ORIGINAL-DIR is used to keep trace of source directory when
called recursively."
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
  (ob:get-name self))

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
  (let* ((tags-art (loop for article in (append (slot-value self 'articles))
			 append (slot-value article 'tags)))
	 (tags-page (loop for page in (append (slot-value self 'pages))
			 append (slot-value page 'tags)))
	 
	 (tags (sort (append tags-art tags-page)
		     #'(lambda (a b) (string< (ob:get-name  a)
					      (ob:get-name  b)))))
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
    		       (string= (ob:get-name i)
    				(ob:get-name (car j))))
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


;;
;; Publishing functions
;;



;; (defmethod ob:publish ((self ob:backend))
;;   ""
;;   ;; Publish both articles static pages
;;   (loop for type in '(articles pages)
;; 	do (loop for POST in (slot-value blog type)
;; 		 do (ob:entry:publish POST)))

;;   ;; publish tags
;;   (with-temp-buffer
;;     (ob:insert-template "page_tags.html")
;;     (ob:write-file (format "%s/tags/index.html"
;; 			   (oref blog publish-dir))))

;;     ;; publish tags
;;   (with-temp-buffer
;;     (ob:insert-template "page_tags.html")
;;     (ob:write-file (format "%s/tags/index.html"
;; 			   (oref blog publish-dir))))
;;   (loop for TAG in TAGS
;; 	do
;; 	(ob-write-index-to-file "blog_tags-details.html"
;; 				(format "%s/tags/%s.html"
;; 					(ob:blog-publish-dir BLOG)
;; 					(ob:tags-safe TAG)))))


(defun ob:get-posts (&optional predicate count sortfunc collect)
  "Return posts (from `POSTS' list of `ob:entry' as defined in
`o-blog-publish') matching PREDICATE. Limit to COUNT results if
defined and sorted using SORTFUNC.

PREDICATE is a function run for each post with the post itself as
argument. If PREDICATE is nil, no filter would be done on posts.

SORTFUNC is used a `sort' PREDICATE.

If COLLECT is defined, only returns the COLLECT slot of
`ob:entry' class.

Examples:

 - Getting last 10 posts:
   \(ob:get-posts nil 10\)

 - Getting post from January 2012:
   \(ob:get-posts
      \(lambda \(x\)
         \(and \(= 2012 \(ob:get 'year x\)\)
              \(= 1 \(ob:get 'month x\)\)\)\)\)

 - getting all categories
    \(ob:get-posts nil nil nil 'category\)

"
  (let* ((posts (if predicate
		    (loop for post in POSTS
			  when (funcall predicate post)
			  collect post)
		  POSTS))
	 (len (length posts)))
    (when (and count (> count 0) (< count len))
      (setq posts (butlast posts (- len count))))
    (when sortfunc
      (setq posts (sort posts sortfunc)))
    (when collect
      (setq posts
	    (loop for post in posts
		  with ret = nil
		  collect (ob:get collect post)
		  into ret
		  finally return (delete-dups ret))))
    posts))




(defun ob:get-last-post (&optional category nth)
  "Get the NTH last post in from CATEGORY or \"blog\" if not defined."
  (let ((POSTS ALL-POSTS)
	(nth (or nth 0)))
    (nth nth (ob:get-posts (lambda (x)
			     (equal (or category "blog")
				    (ob:get-name (ob:get 'category x))))))))


(defun ob:get-snippet (name &optional slot blog)
  ""
  (let* ((blog (or
		blog
		(when (boundp 'BLOG) BLOG)
		(when (boundp 'blog) blog)))
	 (POSTS (ob:get 'snippets blog))
	 (snippet (car
		   (ob:get-posts
		    (lambda(x)
		      (equal name (ob:get 'title x)))))))
    (if (and slot snippet)
	(ob:get slot snippet)
      snippet)))




(provide 'o-blog-backend)

;; o-blog-backend.el ends here
