;;; o-blog.el --- Org-blog exporter

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-01-04
;; Last changed: 2012-01-04 20:31:20
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(defstruct (ob:blog (:type list) :named)
  (file nil :read-only)
  (buffer nil :read-only)
  publish-dir
  template-dir
  posts-filter
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
  content
  content-html)

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


(defun org-publish-blog-1 (file)
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
				    (or (ob:blog-posts-filter BLOG)
					"+TODO=\"DONE\"")
				    'file-with-archives))))
      (message (format "Blog %s published in %ss"
		       file
		       (format-time-string "%s.%3N"
					   (time-subtract (current-time) start-time))))
      POSTS)))

;; Internal functions

(defun ob-parse-blog-headers ()
  "Parse blog related variable from current-buffer."
  (let ((blog (make-ob:blog :file file :buffer (current-buffer))))
    (setf (ob:blog-publish-dir blog) (ob:get-header "PUBLISH_DIR"))
    (setf (ob:blog-template-dir blog) (ob:get-header "TEMPLATE_DIR"))
    (setf (ob:blog-template-dir blog) (ob:get-header "POSTS_FILTER"))
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
	   
	   (category (or (org-entry-get (point) "category") "."))


	   (filename (ob:sanitize-string title))
	   (filepath (format "%s/%.4d/%.2d" category year month))
	   (htmlfile (format "%s/%.2d_%s.html" filepath day filename))

	   (content (ob-get-entry-text))
	   

	   (post
	    (make-ob:post :title title
			  :tags tags
			  :timestamp timestamp
			  :year year
			  :month month
			  :day day
			  :filename filename
			  :filepath filepath
			  :htmlfile htmlfile
			  :template (or (org-entry-get (point) "TEMPLATE") "_post.html")
			  :content content
			  :content-html (ob-export-string-to-html content)
			  )))
      post)))



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






(defun ob:sanitize-string (s)
  "Sanitize string S by:

- converting all charcters ton pure ASCII
- replacing non alphanumerical chars to \"-\"
- downcasing all letters
- trimming leading and tailing \"-\""
  (with-temp-buffer
    (insert s)
    (call-process-region
     (point-min) (point-max)
     "iconv" t t nil "--to-code=ASCII//TRANSLIT")
    (replace-regexp "[^a-zA-Z0-9]+"
		    "-" nil (point-min) (point-max))
    (downcase-region (point-min) (point-max))
    (replace-regexp "^-+\\|-+$"
		    "" nil (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))




;; template accessible functions
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





(provide 'o-blog)
