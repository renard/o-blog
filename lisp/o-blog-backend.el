;;; o-blog-backend.el --- Base class for o-blog backends

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-12-04
;; Last changed: 2014-10-01 00:36:06
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile
  (require 'find-func)
  (require 'cl))


(cl-defstruct
    (ob:backend)
  config-file
  index-file
  source-dir
  source-files
  (publish-dir "out")
  (style-dir (expand-file-name
	      (concat (file-name-directory
		       (find-library-name "o-blog"))
		      "../templates/style")))
  (template-dir  (expand-file-name
		  (concat (file-name-directory
			   (find-library-name "o-blog"))
			  "../templates")))
  (cache-dir "../_cache")
  articles
  pages
  snippets
  tags
  title
  description
  url
  (posts-sorter 'ob:entry:sort-by-date)

  ;; External plugins
  muut-forum
  muut-title
  muut-channel
  disqus
  google-analytics
  google-plus
  twitter-account
  twitter-follow
  share-linkedin
)

(defun ob:publish (self)
  "Publish a new blog.

Some global variables are set:
- `BLOG': the full `ob:backend' child class.
- `POSTS': a list of all sorted posts.
- `TAGS': a list of all tags as `ob:tags' class.

"
  (let ((type (%ob:get-type self)))
    (ob:profile
     "Find files"
     (funcall (ob:get-backend-function type :find-files) self))
    (ob:profile
     "Parse config"
     (funcall (ob:get-backend-function type :parse-config) self))
    (ob:profile
     "Parse entries"
     (funcall (ob:get-backend-function type :parse-entries) self)))

  (ob:profile
   "Compute tags"
   (ob:compute-tags self))

  ;; Strange behavior here, SORT is supposed to modify list by side effect.
  ;; In fact only some articles are missing if 'ARTICLES is not set back
  ;; with the sorted list.
  (%ob:set self 'articles (sort (ob:get 'articles self)
				(ob:get 'posts-sorter self)))

  (let* ((BLOG self)
	 (PAGES (ob:get 'pages BLOG))
	 (POSTS (ob:get 'articles BLOG))
	 (ALL-POSTS POSTS)
	 (TAGS  (ob:get 'tags BLOG))
	 (convert-entry (ob:get-backend-function
			 (%ob:get-type self) :convert-entry)))

    ;; Convert each entry to HTML format
    (ob:profile
     "Convert entries to HTML"
     (loop for type in '(snippets articles pages)
	   do (loop for entry in (ob:get type BLOG)
		    with id = 0
		    do (progn
			 (when (eq type 'articles)
			   (%ob:set entry 'id id)
			   (setf id (1+ id)))
			 (unless (ob:get 'html entry)
			   (funcall convert-entry BLOG entry)
			   (with-temp-buffer
			     (insert (format "%S" entry))
			     (ob:write-file (ob:get 'cache-file entry))))))))

    ;; Publish both articles static pages
    (ob:profile
     "Publish articles and pages"
     (loop for type in '(articles pages)
	   do (loop for POST in (ob:get type BLOG)
		    do (ob:entry:publish POST))))


    (ob:profile
     "Publish tags"
     (ob:tag:publish TAGS BLOG))

    (ob:profile
     "Publish json"
     (ob:publish-articles-json BLOG))
    
    (ob:profile
     "Publish archives"
     (let ((BREADCRUMB "Archives"))
       (let ((FILE "archives.html"))
	 (ob:eval-template-to-file "blog_archives.html"
				   (format "%s/%s"
					   (ob:get 'publish-dir BLOG)
					   FILE)))))

    (ob:profile
     "Publish RSS"
     (ob:eval-template-to-file "blog_rss.html"
			       (format "%s/index.xml"
				       (ob:get 'publish-dir BLOG))))

    (ob:profile
     "Publish Sitemap"
     (ob:eval-template-to-file "blog_sitemap.html"
			       (format "%s/sitemap.xml"
				       (ob:get 'publish-dir BLOG))))

    (ob:profile
     "Publish Category / years / month"

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
		    (ob-process-index "blog_index_category.html" CATEGORY))))
    )
  


    (ob:profile
     "Publish style"
     (ob:publish-style self))
    (message "o-blog publication done")
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



(defun ob:publish-articles-json (blog)
  ;; Write articles JSON
  (with-temp-buffer
    (insert "{\"articles\":{ ")
      
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
			  (or (ob:get 'excerpt article) "")
			  )))
	       ;; remove last comma
	       (delete-char -1)
	       (insert "],")))
    ;; remove last comma
    (delete-char -1)
    (insert "}}")
    (ob:write-file (format "%s/%s" (ob:get 'publish-dir blog) "articles.js"))))






(defun ob:parse-config (blog &optional file)
  "Parse global configuration FILE.

Parse each key / value configuration from FILE. If key is a known
BLOG slot it is save into the BLOG structure.

If FILE is not defined \"o-blog.conf\" would be tried."
  (let* ((file (or file
		   (when (file-exists-p "o-blog.conf")
		     "o-blog.conf")))
	 (lines (when file
		  (split-string
		   (with-temp-buffer
		     (insert-file-contents file)
		     (buffer-string))
		   "\n"))))
    (when lines
      (save-match-data
	(loop for line in lines
	      when (string-match "^\\s-*\\([^#]+?\\)\\s-*=\\s-*\\(.+?\\)\\s-*$" line)
	      do (let ((k (intern (match-string 1 line)))
		       (v (match-string 2 line)))
		   (when (ob:slot-exists-p blog k)
		     (%ob:set blog k v))))))))


;; Useful functions / macros

(defmacro ob:with-source-buffer (self &rest body)
  "Like `with-current-buffer'"
  `(let ((file (ob:get 'config-file ,self)))
     ;; Make sure we are in the o-blog org file
     (with-current-buffer (or (get-file-buffer file)
			     (find-file-noselect file))
       ,@body)))
      

(defun ob:find-files (self extension
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
	  nconc (ob:find-files self extension file-path ignore-dir original-dir)
	  when (member (file-name-extension file) extension)
	  collect (file-relative-name file-path original-dir))))


(defun ob:get-source-directory (self)
  "Return o-blog source directory from ob:backend SELF object."
  (file-name-as-directory
   (format "%s%s"
	   (file-name-as-directory
	    (file-name-directory (ob:get 'config-file self)))
	   (or (ob:get 'source-dir self) ""))))


(defun ob:compute-tags (self &optional min_r max_r)
  "Return a sorted list of all ob:tags by name.

Compute tag occurrence and their HTML percentage value.

MIN_R and MAX_R are the minimum and maximum percentage value. If
not provided 80 and 220 are used. This means ob:size is always
within MIN_R and MAX_R inclusive."
  (let* ((tags-art (loop for article in (append (ob:get 'articles self))
			 append (ob:get 'tags article)))
	 (tags-page (loop for page in (append (ob:get 'pages self))
			 append (ob:get 'tags page)))
	 
	 (tags (sort (append tags-art tags-page)
		     #'(lambda (a b) (string< (ob:get 'display  a)
					      (ob:get 'display  b)))))
	 (min_r (or min_r 80))
	 (max_r (or max_r 220))
	 (min_f (length tags))
	 (max_f 0))
    (%ob:set
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
    		       (string= (ob:get 'display i)
    				(ob:get 'display (car j))))
    		 do (incf k)
    		 else
    		 collect (progn
    			   (when (> k max_f) (setf max_f k))
    			   (when (< k min_f) (setf min_f k))
    			   (list k i))
		 and do (setf k 1))
    	   do (progn
    		(%ob:set tag 'count count)
    		;; This is the tricky part
    		;; Formula is:
    		;; % = min_r + (count - min_f) * (max_r - min_r) / (max_f - min_f)
    		;; the `max' is on purpose in case of max_f = min_f
    		(%ob:set tag 'size
			 (+ min_r
			    (/
			     (* (- count min_f) (- max_r min_r))
			     (max 1.0 (float (- max_f min_f)))))))
    	   collect tag))))


;;
;; Publishing functions
;;


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
				    (ob:get 'display (ob:get 'category x))))))))


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
