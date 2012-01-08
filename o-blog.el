;;; o-blog.el --- Org-blog exporter

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-01-04
;; Last changed: 2012-01-08 02:32:21
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile
  (require 'cl nil t)
  (require 'find-func nil t))
(require 'time-stamp nil t)


(defcustom ob-async-opts nil
  "Extra options to be used when compiling with
`org-publish-blog-async'.")

(defstruct (ob:blog (:type list) :named)
  (file nil :read-only)
  (buffer nil :read-only)
  publish-dir
  template-dir
  style-dir
  posts-filter
  static-filter
  snippet-filter
  title
  description
  cache-dir
  )


(defstruct (ob:post (:type list) :named)
  id
  title
  timestamp
  year
  month
  day
  category
  tags
  template
  filepath
  filename
  htmlfile
  path-to-root
  content
  content-html)


(defstruct (ob:tags (:type list) :named)
  "Tag structure with following slots:

 - name: string defying the tag name.
 - count: how many time the tag is used.
 - size: the font size in percent."
  name count size)


;;;###autoload
(defun org-publish-blog (&optional file async)
  "Publish FILE as a blog synchronously execpt ib ASYNC is
defined, or interactivelly called with `prefix-arg'.
"
  (interactive
   (list (or
	  (when (eq major-mode 'org-mode) (buffer-file-name))
	  (read-file-name "Publish blog from: " nil nil t))
	 current-prefix-arg))
  (if async
      (org-publish-blog-async file)
      (org-publish-blog-sync file)))

;;;###autoload
(defun org-publish-blog-sync (file)
  "Publish FILE synchronously."
  (with-current-buffer (or
			(get-file-buffer file)
			(find-file file))

    (let* ((start-time (current-time)) ;; for statistic purposes only
	   ;; make sure we are on the correct directory.
	   (default-directory (file-name-directory file))
	   (BLOG (ob-parse-blog-headers))
	   (POSTS (ob-parse-entries
	   	   (org-map-entries 'point-marker
	   			    (ob:blog-posts-filter BLOG)
	   			    'file-with-archives)))
	   (ALL-POSTS POSTS)
	   (STATIC (ob-parse-entries
		    (org-map-entries 'point-marker
				     (ob:blog-static-filter BLOG)
				     'file-with-archives)))
	   (SNIPPETS (ob-parse-entries
		      (org-map-entries 'point-marker
				       (ob:blog-snippet-filter BLOG)
				       'file-with-archives)))

	   (TAGS (ob-compute-tags POSTS)))

      (ob-write-static)
      (ob-write-posts)
      (ob-write-tags)
      (ob-write-index)
      (copy-directory (format "%s/%s"
			      (ob:blog-template-dir BLOG)
			      (ob:blog-style-dir BLOG))
		      (ob:blog-publish-dir BLOG))
      (message (format "Blog %s published in %ss"
		       file
		       (format-time-string "%s.%3N"
					   (time-subtract (current-time) start-time)))))))

(defun org-blog-publish-run-processes-sentinel (proc change)
  "Sentinel in charge of cleaning `org-publish-blog-async' on success."
  (when (eq (process-status proc) 'exit)
    (let ((status  (process-exit-status proc))
	  (cmd (process-get proc :cmd))
	  (cmd-buf (process-get proc :cmd-buf)))
      (if (not (eq 0 status))
	  (progn
	    (when (process-buffer proc)
	      (set-window-buffer (selected-window) cmd-buf))
	    (error "Org blog ERROR: %s" cmd))
	(message  "Org blog OK: %s" cmd))
      (when cmd-buf (kill-buffer cmd-buf)))))


(defun org-publish-blog-async (file)
  "Publish FILE synchronously."
  (let* ((cmd-line (append command-line-args
			   `("--batch"
			     "-l" ,(concat (file-name-as-directory
					    user-emacs-directory)
					   "init.el")
			     ,@ob-async-opts
			     "--eval"
			     ,(format "(org-publish-blog \"%s\")" file))))
	 (cmd-buf (get-buffer-create (format "ORG blog build %s" file)))
	 (proc (apply 'start-process (car cmd-line)
		      cmd-buf (car cmd-line) (cdr cmd-line))))
    (message "Run: %S" cmd-line)
    (process-put proc :cmd (format "Build %s" file))
    (process-put proc :cmd-buf cmd-buf)
    (set-process-sentinel proc 'org-blog-publish-run-processes-sentinel)))

(defun ob-get-linked-files (s)
  "Return a lis of linked files from S."
  (save-match-data
    (with-temp-buffer
      (insert s)
      (beginning-of-buffer)
      (let (ret)
	(while (re-search-forward org-any-link-re nil t)
	  (loop for s in (list (match-string-no-properties 2)
			       (match-string-no-properties 4))
		when s
		do (let ((f (save-match-data
			      (string-match "^\\(file:\\)?\\(.+\\)" s)
			      (match-string 2 s))))
		     (when (and f
				(file-exists-p f))
		       (add-to-list 'ret f)))))
	ret))))

;; Internal functions

(defun ob-parse-blog-headers (&optional file)
  "Parse blog related variable from current-buffer."
  (let* ((file (or file (buffer-file-name)))
	 (blog (make-ob:blog :file file :buffer (current-buffer))))
    (setf (ob:blog-publish-dir blog) (or (ob:get-header "PUBLISH_DIR") "out"))
    (setf (ob:blog-template-dir blog) (or (ob:get-header "TEMPLATE_DIR")
					  (concat
					   (file-name-directory
					    (file-name-directory
					     (find-library-name "o-blog")))
					   "templates")))
    (setf (ob:blog-style-dir blog) (or (ob:get-header "STYLE_DIR") "style"))
    (setf (ob:blog-posts-filter blog) (or (ob:get-header "POSTS_FILTER") "+TODO=\"DONE\""))
    (setf (ob:blog-static-filter blog) (or (ob:get-header "STATIC_FILTER") "+PAGE={.+\.html}"))
    (setf (ob:blog-snippet-filter blog) (or (ob:get-header "SNIPPET_FILTER") "+SNIPPET={.+}"))
    (setf (ob:blog-cache-dir blog) (or (ob:get-header "CACHE_DIR") "cache"))
    (setf (ob:blog-title blog) (or (ob:get-header "TITLE") "title"))
    (setf (ob:blog-description blog) (or (ob:get-header "DESCRIPTION") "Description"))
    blog))


(defun ob-parse-entries (markers)
  "Parse blog entries from current buffer.

MARKERS is a list of entries given by `org-map-entries'."
  (save-excursion
    (loop for marker in markers
	  with posts = nil
	  ;; Parse each entry into posts
	  collect (with-current-buffer (marker-buffer marker)
		    (goto-char (marker-position marker))
		    (ob-parse-entry))
	  into posts
	  ;; Then wee need to set the post id in all all sorted posts.
	  finally return (loop for post in (sort posts 'ob-sort-posts-by-date)
			       with id = 0
			       do (setf (ob:post-id post) id)
			       and do (incf id 1)
			       and collect post))))

(defun ob-sort-posts-by-date (a b)
  "Sort both A and B posts by date (newer posts first)."
  (> (float-time (ob:post-timestamp a))
     (float-time (ob:post-timestamp b))))


(defun ob-parse-entry()
  "Parse blog entry from current position"
  (when (search-forward-regexp org-complex-heading-regexp
			       (point-at-eol)
			       t)
    (let* ((title (match-string-no-properties 4))
	   ;; tags must be split on comma
	   (tags (split-string (or (org-entry-get (point) "Tags") "")
			       "\\s-*,\\s-*" t))

	   ;; Timestamp is taken from either the CLOSED property or the
	   ;; current timestamp.
	   (timestamp (apply 'encode-time
			     (org-parse-time-string
			      (or (org-entry-get (point) "CLOSED")
				  (time-stamp-string)))))
	   ;; Some other time variables
	   (year (string-to-number (format-time-string "%Y" timestamp)))
	   (month (string-to-number (format-time-string "%m" timestamp)))
	   (day (string-to-number (format-time-string "%d" timestamp)))
	   
	   (category (or (org-entry-get (point) "category") "blog"))

	   (page (org-entry-get (point) "PAGE"))

	   (filename (ob:sanitize-string title))
	   (filepath (format "%s/%.4d/%.2d" category year month))
	   (htmlfile (format "%s/%.2d_%s.html" filepath day filename))

	   (content (ob-get-entry-text))
	   (linked-files (ob-get-linked-files content)))

      (when page
	(setq htmlfile page
	      filename (file-name-sans-extension (file-name-nondirectory htmlfile))
	      filepath (file-name-directory htmlfile)))

      (loop for f in linked-files
	    do (let ((target (format "%s/%s/%s"
				     (ob:blog-publish-dir BLOG)
				     filepath f)))
		 (mkdir (file-name-directory target) t)
		 (copy-file f target t t t t)))


      (make-ob:post :title title
		    :tags tags
		    :timestamp timestamp
		    :year year
		    :month month
		    :day day
		    :filename filename
		    :filepath filepath
		    :path-to-root (file-relative-name "." filepath)
		    :htmlfile htmlfile
		    :template (or (org-entry-get (point) "TEMPLATE")
				  (if page "blog_static.html" "blog_post.html"))
		    :content content
		    :content-html (ob-export-string-to-html content)
		    :category category
		    ))))


(defun ob-get-entry-text ()
  "Return entry text from point with not properties.

Please note that a blank line _MUST_ be present between entry
headers and body."
  (save-excursion
    (save-restriction
      (save-match-data
	(org-narrow-to-subtree)
	(goto-char (point-min))
	(let ((p (search-forward-regexp "^\\s-*$" nil t)))
	  (when p (goto-char p)))
	(buffer-substring-no-properties (point) (point-max))))))


(defun ob-export-string-to-html (string)
  "Convert STRING to html using `org-mode' syntax."
  (with-temp-buffer
    (insert string)
    (substring-no-properties (org-export-as-html nil nil nil 'string t))))


(defun ob-compute-tags (posts &optional min_r max_r)
  "Return a list of all tags sorted by usage.

Each item is: \(TAG COUNT PERCENT \)

CONTENT-LIST is a list of all articles such as generated in
`org-publish-blog'.

MIN_R and MAX_R are the minimum and maximum percentage value. If
not provided 80 and 220 are used."
  (let* ((tags (sort (loop for post in posts
			   append (ob:post-tags post))
		     #'string<))
	 (min_r (or min_r 80))
	 (max_r (or max_r 220))
	 (min_f (length tags))
	 (max_f 0))

    (loop for item in
	    ;; Here extract uniq tags and count occurrences
	    ;; (such as uniq -c does)
	    ;; Each item of returned list is
	    ;; (VALUE COUNT)
	  (loop for (i . j) on tags
		with k = 1
		when (string= i (car j)) do (incf k)
		else collect (progn
			       (when (> k max_f) (setf max_f k))
			       (when (< k min_f) (setf min_f k))
			       (cons i (list k)))
		and do (setf k 1))

	  collect (let ((val (cadr item)))
		    (make-ob:tags
		     :name (car item)
		     :count val
		     ;; This is the tricky part
		     ;; Formula is:
		     ;; % = min_r + (val - min_f) * (max_r - min_r) / (max_f - min_f)
		     ;; the `max' is on purpose in case of max_f = min_f
		     :size (+ min_r
			      (/
			       (* (- val min_f) (- max_r min_r))
			       (max 1.0 (float (- max_f min_f))))))))))

(defun ob-eval-lisp()
  "Eval embeded lisp code defined by <lisp> tags in html fragment
when publishing a page."
  (save-excursion
    (save-restriction
      (save-match-data
	;; needed for thing-at-point
	(html-mode)
	(beginning-of-buffer)
	(let ((open-tag "<lisp>")
	      (close-tag "</lisp>")
	      beg end sexp)
	  (while (search-forward open-tag nil t)
	    (setq beg (- (point) (length open-tag)))
	    (when (search-forward close-tag nil t)
	      (setq end (point))
	      (backward-char (length close-tag))
	      (backward-sexp)
	      (setq sexp (substring-no-properties (thing-at-point 'sexp)))
	      (delete-region beg end)
	      (insert
	       (save-match-data
		 (condition-case err
		     (let ((object (eval (read sexp))))
		       (cond
			;; result is a string
			((stringp object) object)
			;; a list
			((and (listp object)
			      (not (eq object nil)))
			 (let ((string (pp-to-string object)))
			   (substring string 0 (1- (length string)))))
			;; a number
			((numberp object)
			 (number-to-string object))
			;; nil
			((eq object nil) "")
			;; otherwise
			(t (pp-to-string object))))
		   ;; error handler
		   (error
		    (format "Lisp error in %s: %s" (buffer-file-name) err)))))
	      (goto-char beg))))))))


;; Publish functions

(defun ob-write-file (file)
  "Write current buffer to FILE and create full path if necessary."
  (mkdir (file-name-directory file) t)
  (write-file file))

(defun ob-write-index()
  "Publish all indexes (default, categories, year, month)"
  (ob-write-index-to-file "blog_archives.html"
  			  (format "%s/archives.html"
  				  (ob:blog-publish-dir BLOG)))

  (ob-write-index-to-file "blog_rss.html"
  			  (format "%s/index.xml"
  				  (ob:blog-publish-dir BLOG)))


  (loop for CATEGORY in (ob:get-posts nil nil nil 'category)
	with PATH-TO-ROOT = ".."
	do
	(loop for YEAR in (ob:get-posts
			   (lambda (x) (equal CATEGORY (ob:post-category x)))
			   nil nil 'year)
	      with PATH-TO-ROOT = "../.."
	      do
	      (loop for MONTH in (ob:get-posts
				  (lambda (x) (and
					       (equal CATEGORY (ob:post-category x))
					       (= YEAR (ob:post-year x))))
				  nil nil 'month)
		    with PATH-TO-ROOT = "../../.."
		    do (ob-process-index "blog_index_month.html" CATEGORY YEAR MONTH))
	      and do (ob-process-index "blog_index_year.html" CATEGORY YEAR))
	and do (unless (equal "." CATEGORY)
		 (ob-process-index "blog_index_category.html" CATEGORY))))

(defun ob-process-index (template &optional category year month)
  "Low-level function for `ob-write-index'.

Template is read from TEMPLATE file.

If provided CATEGORY YEAR and MONTH are used to select articles."
  (let* ((fp (format "%s/%s/index.html"
		     (ob:blog-publish-dir BLOG)
		     (cond
		      ((and category year month) (format "%s/%.4d/%.2d" category year month))
		      ((and category year) (format "%s/%.4d" category year))
		      (t category))))

	 (POSTS (ob:get-posts
		 (lambda (x) (and
			      (if category (equal category (ob:post-category x)) t)
			      (if year (= year (ob:post-year x)) t)
			      (if month (= month (ob:post-month x)) t))))))
	 (ob-write-index-to-file template fp)))


(defun ob-write-index-to-file (template outfile)
  ""
  (with-temp-buffer
    "*ORG blog publish index*"
    (erase-buffer)
    (insert-file-contents
     (format "%s/%s" (ob:blog-template-dir BLOG) template))
    (ob-eval-lisp)
    (ob-write-file outfile)))


(defun ob-write-static ()
  "Publish static pages"
  (loop for POST in STATIC
	do (ob-write-index-to-file
	    (ob:post-template POST)
	    (format "%s/%s"
		    (ob:blog-publish-dir BLOG)
		    (ob:post-htmlfile POST)))))

(defun ob-write-posts ()
  "Publish all posts"
  (loop for POST in POSTS
	do (ob-write-index-to-file
	    (ob:post-template POST)
	    (format "%s/%s"
		    (ob:blog-publish-dir BLOG)
		    (ob:post-htmlfile POST)))))



(defun ob-write-tags ()
  "Publish all tags into directory named \"tags\"."
  (let ((PATH-TO-ROOT ".."))
    (ob-write-index-to-file "blog_tags.html"
			    (format "%s/tags/index.html"
				    (ob:blog-publish-dir BLOG)))

    (loop for TAG in TAGS
	  do
	  (ob-write-index-to-file "blog_tags-details.html"
				  (format "%s/tags/%s.html"
					  (ob:blog-publish-dir BLOG)
					  (ob:tags-name TAG))))))


(defun ob:sanitize-string (s)
  "Sanitize string S by:

- converting all charcters ton pure ASCII
- replacing non alphanumerical chars to \"-\"
- downcasing all letters
- trimming leading and tailing \"-\""
  (loop for c across s
	with gc
	with ret
	do (setf gc (get-char-code-property c 'general-category))
	if (member gc '(Lu Ll))
	collect (downcase (char-to-string
			   (car (get-char-code-property c 'decomposition))))
	into ret
	else if (member gc '(Zs))
	collect "-" into ret
	finally return (replace-regexp-in-string
			"--+" "-"
			(replace-regexp-in-string
			 "^-+\\|-+$" ""
			 (mapconcat 'identity ret "")))))


;; template accessible functions


(defun ob:get-posts (&optional predicate count sortfunc collect)
  "Return posts (from `POSTS' as defined in `org-publish-blog')
matching PREDICATE. Limit to COUNT results if defined and sorted
using SORTFUNC.

PREDICATE is a function run for each post with the post itself as
argument. If PREDICATE is nil, no filter would be done on posts.

SORTFUNC is used a `sort' PREDICATE.

If COLLECT is defined, only returns the COLLECT field of a
`ob:post' structure.

Examples:

 - Getting last 10 posts:
   \(ob:get-posts nil 10\)

 - Getting post from January 2012:
   \(ob:get-posts
      \(lambda \(x\)
         \(and \(= 2012 \(ob:post-year x\)\)
              \(= 1 \(ob:post-month x\)\)\)\)\)

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
		  collect (funcall (intern (format "ob:post-%s" collect)) post)
		  into ret
		  finally return (delete-dups ret))))
    posts))


(defun ob:get-post-by-id (id)
  "Return post which id is ID"
  (when (>= id 0)
    (nth id POSTS)))

(defun ob:get-snippet (name)
  "Get first snippet matching NAME."
  (let ((POSTS SNIPPETS))
    (car
     (ob:get-posts (lambda(x) (equal name (ob:post-title x)))))))


(defun ob:get-header (header &optional all)
  "Get HEADER from blog buffer as defined in BLOG global context
variable.

Returns only fist match execpt is ALL is defined."
  (with-current-buffer
      ;; Be sure we are in blog buffer
      (if (boundp 'BLOG)
	  (ob:blog-buffer BLOG)
	(current-buffer))
    (save-excursion
      (save-restriction
	(save-match-data
	  (widen)
	  (goto-char (point-min))
	  (let (values)
	    (while (re-search-forward (format "^#\\+%s:?[ \t]+\\(.*\\)" header) nil t)
	      (add-to-list 'values (substring-no-properties (match-string 1))))
	    (if all
		values
	      (car values))))))))

(defun ob:insert-template (template)
  "Insert TEMPLATE in current buffer."
  (insert
   (with-temp-buffer
     "*Org-Publish-Template*"
     (erase-buffer)
     (insert-file-contents (format "%s/%s" (ob:blog-template-dir BLOG) template))
     (ob-eval-lisp)
     (buffer-string))))

(defun ob:format-date (date &optional format locale)
  "Format DATE using FORMAT and LOCALE.

DATE can heither be string suitable for `parse-time-string' or a
list of interger using `current-time' format.

FORMAT is a `format-time-string' compatible definition. If not
set ISO8601 \"%Y-%m-%dT%TZ\" format would be used."
  (let* ((date (cond
		((stringp date)
		 (apply 'encode-time
			(parse-time-string date)))
		((listp date)
		 date)))
	 (format (or format "%Y-%m-%dT%TZ"))
	 (system-time-locale locale))
    (format-time-string format date)))


(defun ob:path-to-root ()
  "Return path to site root from `PATH-TO-ROOT' or `POST'
path-to-root slot."
  (cond
   ((boundp 'PATH-TO-ROOT) PATH-TO-ROOT)
   ((boundp 'POST) (ob:post-path-to-root POST))
   (t ".")))

(provide 'o-blog)
