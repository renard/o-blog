;;; o-blog-backend-org.el --- org-mode backend for o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-12-04
;; Last changed: 2014-07-07 19:31:20
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'eieio nil t)
  (require 'o-blog-backend nil t)
  (require 'ox)
  (require 'org-archive)
  (require 'org-table))


(defclass ob:backend:org (ob:backend)
  ((articles-filter :initarg :articles-filter
		    :type string
		    :initform "+TODO=\"DONE\""
		    :documentation "")
   (pages-filter :initarg :pages-filter
		 :type string
		 :initform "+PAGE={.+\.html}"
		 :documentation "")
   (snippets-filter :initarg :snippets-filter
		    :type string
		    :initform "+SNIPPET={.+}"
		    :documentation ""))
  "Object type handeling o-blog files in org-mode.")



(defmethod ob:find-files ((self ob:backend:org) &optional file)
  ""
  (ob:find-files-1 self '("org")))

(defmethod ob:org-get-header ((self ob:backend:org) header &optional all)
  "Return value of HEADER option as a string from current org-buffer. If ALL is
T, returns all occurrences of HEADER in a list."
  (ob:with-source-buffer
   self
   (save-excursion
     (save-restriction
       (save-match-data
	 (widen)
	 (goto-char (point-min))
	 (let (values)
	   (while (re-search-forward
		   (format "^#\\+%s:?[ \t]*\\(.*\\)"
			   (replace-regexp-in-string
			    "-" "[_-]" (symbol-name header))) nil t)
	     (add-to-list 'values (substring-no-properties (match-string 1))))
	   (if all
	       values
	     (car values))))))))

(defmethod ob:org-get-entry-text ((self ob:backend:org))
  "Return entry text from point with not properties.

Please note that a blank line _MUST_ be present between entry
headers and body."
  (save-excursion
    (save-restriction
      (save-match-data
	(org-narrow-to-subtree)
	(let ((text (buffer-string)))
	  (with-temp-buffer
	    (insert text)
	    (goto-char (point-min))
	    (org-mode)
	    (while (<= 2 (save-match-data (funcall outline-level)))
	      (org-promote-subtree))
	    (goto-char (point-min))
	    (when (search-forward-regexp "^\\s-*$" nil t)
	      (goto-char (match-end 0)))
	    (save-excursion
	      (insert "#+OPTIONS: H:7 num:nil toc:nil d:nil todo:nil <:nil pri:nil tags:nil\n\n"))
	    (buffer-substring-no-properties (point) (point-max))))))))


(defmethod ob:parse-config ((self ob:backend:org))
  "Par o-blog configuration directely from org header."
      (loop for slot in (object-slots self)
	    for value = (ob:org-get-header self slot)
	    when value
	    do (set-slot-value self slot value))
      ;; Set some imutalbe values.
      (let ((file (ob:get-name self)))
	(set-slot-value self 'index-file file)
	(set-slot-value self 'source-files (list file))
	(set-slot-value self 'source-dir (file-name-directory file)))
      self)


(defmethod ob:get-tags-list ((self ob:backend:org))
  ""
  (loop for tn in (org-get-local-tags)
	for td = (ob:replace-in-string tn '(("_" " ") ("@" "-")))
	for ts = (ob:sanitize-string td)
	collect (ob:tag tn :display td :safe ts)))

(defmethod ob:parse-entry ((self ob:backend:org) marker type)
  "Parse an org-mode entry at position defined by MARKER."
  (save-excursion
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (when (search-forward-regexp org-complex-heading-regexp
				   (point-at-eol)
				   t)
	(let* ((type (or type 'article))
	       (class (intern (format "ob:%s" type)))
	       (entry (funcall class marker)))

	  (set-slot-value
	   entry 'title
	   (match-string-no-properties 4))

	  (when (slot-exists-p entry 'timestamp)
	    (set-slot-value
	     entry 'timestamp
	     (apply 'encode-time
		    (org-parse-time-string
		     (or (org-entry-get (point) "CLOSED")
			 (time-stamp-string
			  "%:y-%02m-%02d %02H:%02M:%02S %u")))))
	    (ob:entry:compute-dates entry))

	  (when (slot-exists-p entry 'tags)
	    (set-slot-value entry 'tags
			    (ob:get-tags-list self)))

	  (when (slot-exists-p entry 'category)
	    (let ((cat (or (org-entry-get (point) "category")
			   (car (last (org-get-outline-path))))))
	    (set-slot-value entry
			    'category
			    (ob:category:init (ob:category cat)))))

	  (when (slot-exists-p entry 'template)
	    (let ((template (org-entry-get (point) "template")))
	      (when template
		(set-slot-value entry 'template template))))

	  (if (eq 'page type)
	      (ob:entry:set-path entry
				 (org-entry-get (point) "PAGE"))
	    (ob:entry:set-path entry))

	  (set-slot-value
	   entry 'source
	   (ob:org-get-entry-text self))

	  entry)))))


(defmethod ob:parse-entries-1 ((self ob:backend:org) type)
  "Collect all entries defined by TYPE from current org tree
using `ob:parse-entry'."
  (let ((markers (org-map-entries
		  'point-marker
		  (slot-value self (intern (format "%ss-filter" type)))
		  'file-with-archives)))
    (loop for marker in markers
	  collect (ob:parse-entry self marker type) into items
	  finally return (loop for item in (sort items (ob:get 'posts-sorter self))
			      for id = 0 then (incf id)
			      do (set-slot-value item 'id id)
			      and collect item))))

(defmethod ob:parse-entries ((self ob:backend:org))
  "Parse all entries (articles, pages and snippets from current org tree."
  (ob:with-source-buffer
   self
   (loop for type in '(article page snippet)
	 for class-type = (intern (format "%ss" type))
	 do (set-slot-value self class-type (ob:parse-entries-1 self type)))
   self))


(defmethod ob:org-fix-html-level-numbering ((self ob:backend:org))
  "Promote every org-generated heading level by one in current buffer."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while
	  ;;(rx (group "foo") ":" (backref 1))
	  ;; as defined in `org-html-level-start'
	  (re-search-forward "\n<div id=\"outline-container-\\([^\"]+?\\)\" class=\"outline-\\([1-9]+\\)\">\n<h\\2 id=\"\\([^\"]+\\)\">\\(.*?\\\)</h\\2>\n\\(<div class=\"outline-text-\\2\" id=\"\\([^\"]+?\\)\">\n\\)?" nil t)
	(let ((container (match-string-no-properties 1))
	      (level (1- (string-to-int (match-string-no-properties 2))))
	      (title-class (match-string-no-properties 3))
	      (title (match-string-no-properties 4))
	      (text (match-string-no-properties 5))
	      (text-id (match-string-no-properties 6)))
	  (goto-char (match-beginning 0))
	  ;;(narrow-to-region (match-beginning 0) (match-end 0))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert
	   (format
	    "<div id=\"outline-container-%s\" class=\"outline-%d\">\n"
	    container level)
	   (format
	    "<h%d id=\"%s\">%s</h%d>\n" level title-class title level))
	  (when text
	    (insert (format
		     "<div class=\"\outline-text-%d\" id=\"%s\">\n"
		     level text-id))))))))


(defmethod ob:org-fix-icons ((self ob:backend:org))
  "Convert all \"<i>icon-...</i>\" to \"<i class=\"icon-...\"/>\"
in current-buffer."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while
	  (re-search-forward "<i>\\(icon-\\(?:[[:alpha:]]\\|[- ]\\)+\\)</i>" nil t)
	(let ((icon (match-string-no-properties 1)))
	  (goto-char (match-beginning 0))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (format
		   "<i class=\"%s\"></i>" icon)))))))

(defmethod ob:org-fix-html ((self ob:backend:org) html)
  "Perform some html fixes on org html export."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    ;; Org export html wrapped within <div id="content">...</div>
    ;; remove that div.
    (save-excursion
      (save-match-data
	(when (search-forward "<div id=\"content\">" nil t)
	  (delete-region (point-at-bol) (point-at-eol))
	  (goto-char (point-max))
	  (delete-region (point-at-bol) (point-at-eol)))))
    (ob:org-fix-html-level-numbering self)
    (ob:org-fix-icons self)
    (buffer-substring-no-properties (point-min) (point-max))))


(defmethod ob:convert-entry ((self ob:backend:org) entry)
  "Convert ENTRY to html using `org-mode' syntax."
  (with-temp-buffer
    ;; insert dummy comment in order to make inline html work.
    (insert (oref entry source))
    (org-mode)
    (goto-char (point-min))
    (ob:org-fix-org)
    (ob:org:publish-linked-files self entry)
    (ob:framework-expand)
    (ob:framework-expand "<\\([^:][^/ \n\t>]+\\)\\([^>]*\\)?>" "</%s>")

    ;; exporting block with ditaa is kinda messy since it requires a real
    ;; file (does not work with a temp-buffer which is not associated to any
    ;; file).
    (let* ((saved-file
	    (when
		(re-search-forward "^#\\+BEGIN_SRC:?[ \t]+\\(ditaa\\)" nil t)
	      (format "/%s/%s/%s.src.org"
		      default-directory
		      (ob:get 'publish-dir BLOG)
		      ;; variable inherited from `ob-parse-entry'
		      (ob:get 'htmlfile entry))))
	   (default-directory
	      (if saved-file
		  (file-name-directory saved-file)
		default-directory))
	   (org-confirm-babel-evaluate nil)
	   ret)
      (when saved-file
	(unless (featurep 'ob-ditaa)
	  (require 'ob-ditaa))
      	(ob:write-file saved-file))
      (let ((html (substring-no-properties
		   ;; `org-export-as-html' arguments has changed on new
		   ;; org-version, then again with the new exporter.
		   ;; First try old function signatures, then on failure
		   ;; use new argument call.
		   (or
		    (ignore-errors (org-export-as-html nil nil nil 'string t))
		    (ignore-errors (org-export-as-html nil nil 'string t))
		    (org-export-as 'html nil nil t nil)))))
	;;(message "Txt: %S" (buffer-substring-no-properties (point-min) (point-max)))
	;;(message ": %S" (buffer-substring-no-properties (point-min) (point-max)))
	(set-slot-value entry 'html
			(ob:org-fix-html self html)))
      (when saved-file
	(delete-file saved-file))
      (ob:get-post-excerpt entry)
      entry)))




;;(defmethod ob:org-fix-org ((self ob:backend:org))
(defun ob:org-fix-org ()
  "Fix some org syntax."
  ;; This is a VERY ugly trick to ensure backward compatibility.
  (let*
      ((compute-bootstrap-grid-args
	(lambda (a1 a2)
	  (let* ((i2c
		  (lambda (x)
		    (let ((x (if x (string-to-int x) 0)))
		      (cond
		       ((= 0 x) "")
		       ((< 0 x) (format "col-sm-%d" x))
		       ((> 0 x) (format "col-sm-offset-%d" (- x))))))))
	    (format "%s %s"
		    (funcall i2c a1)
		    (funcall i2c a2)))))
       (subst-list
	'(
	  ;; alerts
	  ("^#\\+BEGIN_O_BLOG_ALERT:?[ \t]+\\(info\\|success\\|warning\\|error\\)[ \t]*\\(.*\\)"
	   ("<div class=\"alert alert-" (match-string 1) "\">"
	    (when (match-string 2)
	      (mapconcat
	       'identity
	       (list "<p class=\"alert-heading\">"
		     (match-string 2) "</p>")
	       "")))
	   "^#\\+END_O_BLOG_ALERT" "</div>")
	  ;; comment
	  ("^#\\+BEGIN_COMMENT"
	   "<!-- Org comment\n"
	   "^#\\+END_COMMENT"
	   "\nEnd of Org comment-->\n")
	  ;; Hero unit
	  ("^#\\+BEGIN_O_BLOG_HERO_UNIT"
	   "<div class=\"hero-unit\">\n"
	   "^#\\+END_O_BLOG_HERO_UNIT"
	   "</div>\n")
	  ;; Hero unit
	  ("^#\\+BEGIN_O_BLOG_PAGE_HEADER"
	   "<div class=\"page-header\">\n"
	   "^#\\+END_O_BLOG_PAGE_HEADER"
	   "</div>\n")
	  ;; Well
	  ("^#\\+BEGIN_O_BLOG_WELL"
	   "<div class=\"well\">\n"
	   "^#\\+END_O_BLOG_WELL"
	   "</div>\n")
	  ;;grid
	  ("^#\\+BEGIN_O_BLOG_ROW:?[ \t]+\\(.*\\)"
	   (let* ((args (when (stringp (match-string 1))
	   		  (split-string (match-string-no-properties 1)))))
	     (format "<div class=\"row %s\"><div class=\"%s\"><!-- %S -->"
		     (or (nth 2 args) "")
	   	     (funcall compute-bootstrap-grid-args
			      (nth 0 args)
			      (nth 1 args))
		     args))
	   "^#\\+END_O_BLOG_ROW"
	   "</div></div>")
	  ;; grid column
	  ("^#\\+O_BLOG_ROW_COLUMN:?[ \t]+\\(-?[0-9]+\\)\\([ \t]+\\(-?[0-9]+\\)\\)?"
	   (let* ((args (when (stringp (match-string 1))
	   		  (split-string (match-string-no-properties 1)))))
	     (format "</div><div class=\"%s\"><!-- %S -->"
	   	     (funcall compute-bootstrap-grid-args
			      (nth 0 args)
			      (nth 1 args))
		     args)))
	  ;;badge or label
	  ("\\([^,]\\)\\[\\[ob-\\(badge\\|label\\):\\(default\\|success\\|warning\\|important\\|info\\|inverse\\)\\]\\[\\(.+?\\)\\]\\]"
	   (let* ((first (match-string 1))
		  (type  (match-string 2))
		  (style (match-string 3))
		  (data  (match-string 4)))
	     (format "%s<span class=\"%s %s-%s\">%s</span>"
		     first type type style data)))
	  ;; progress
	  ("\\([^,]\\)\\[\\[ob-progress:\\(info\\|success\\|warning\\|danger\\),?\\([^]]+\\)?\\]\\[\\(.+?\\)\\]\\]"
	   (let ((first  (match-string 1))
		 (style  (match-string 2))
		 (value  (match-string 4))
		 (extra (loop for elm in (split-string (or (match-string 3) "") ",")
			      collect (if (string= elm "striped")
					  (format "progress-%s" elm)
					elm)
			      into ret
			      finally return (mapconcat 'identity ret " "))))
	     (format
	      "%s<div class=\"progress progress-%s %s\"><div class=\"bar\" style=\"width: %s%%;\"></div></div>"
	      first
	      style
	      extra
	      value)))
	  ;; modal source
	  ("^#\\+O_BLOG_SOURCE:?[ \t]+\\(.+?\\)\\([ \t]+\\(.+\\)\\)?$"
	   (let* ((src-file (match-string 1))
		  (src-file-name (file-name-nondirectory src-file))
		  (src-file-safe (ob:sanitize-string src-file-name))
		  (mode (match-string 3)))
	     (format "<div class=\"o-blog-source\"><a class=\"btn btn-info\" data-toggle=\"modal\" data-target=\"#%s\" ><i class=\"icon-file icon-white\"></i>&nbsp;%s</a></div><div class=\"modal fade hide\" id=\"%s\"><div class=\"modal-header\"><a class=\"close\" data-dismiss=\"modal\">×</a><h3>%s</h3></div><div class=\"modal-body\">%s</div></div>"
		     src-file-safe src-file-name src-file-safe
		     src-file-name
		     (with-temp-buffer
		       (insert-file-contents src-file)
		       (if mode
			   (setq func (intern (format "%s-mode" mode)))
			 (setq func (assoc-default src-file auto-mode-alist
						   'string-match)))
		       (if (functionp func)
			   (funcall func)
			 (warn (concat "Mode %s not found for %s. "
				       "Consider installing it. "
				       "No syntax highlight would be bone this time.")
			       mode src-file))
		       ;; Unfortunately rainbow-delimiter-mode does not work fine.
		       ;; See https://github.com/jlr/rainbow-delimiters/issues/5
		       (font-lock-fontify-buffer)
		       (htmlize-region-for-paste (point-min) (point-max))))))
	  )))
    (loop for (re_start sub_start re_end sub_end)
	  in subst-list
	  do (save-match-data
	       (save-excursion
		 (goto-char (point-min))
		 (while (re-search-forward re_start nil t)
		   (beginning-of-line)
		   (insert
		    "#+BEGIN_HTML\n"
		    (ob:string-template sub_start)
		    "\n#+END_HTML\n")
		   (delete-region (point) (point-at-eol))
		   (when re_end
		     (save-excursion
		       (unless
			   (re-search-forward re_end nil t)
			 (error "%s not found in %s@%s." re_end
				(buffer-file-name)
				(point)))
		       (beginning-of-line)
		       (insert
			"\n#+BEGIN_HTML\n"
			(ob:string-template sub_end)
			"\n#+END_HTML\n")
		       (delete-region (point) (point-at-eol))))))))))


(defmethod  ob:org:publish-linked-files((self ob:backend:org) entry)
  "Copy files (defined by \"file:\" link prefix) to page related directory."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((htmlfile (ob:get 'htmlfile entry))
	    (page (slot-exists-p entry 'page))
	    (filepath (ob:get 'filepath entry))
	    (title (ob:get 'title entry))
	    ret)
	(while (re-search-forward "\\(\\[file:\\)\\([^]]+\\)\\(\\]\\)" nil t)
	  (let ((prefix (match-string-no-properties 1))
		(file  (match-string-no-properties 2))
		(suffix (match-string-no-properties 3)))

	    (when (file-exists-p file)
	      (replace-match
	       (if page
		   (format "%s%s/%s/%s%s"
			   prefix
			   (ob:get 'path-to-root entry)
			   (or (file-name-directory htmlfile) ".")
			   (file-name-nondirectory file) suffix)

		 (format "%s%s/%s/%s/%s%s"
			 prefix
			 (ob:get 'path-to-root entry)
			 (file-relative-name "." filepath)
			 (file-name-sans-extension htmlfile)
			 (file-name-nondirectory file) suffix ))

	       (add-to-list 'ret file)))))
	
	(when ret
	  (unless page
	    ;; create a redirection page as index.html into files' directory
	    (with-temp-buffer
	      (insert
	       (mapconcat 'identity
			  `(,(format "* Redirect from (%s)" title)
			    ":PROPERTIES:"
			    ,(format ":PAGE: %s/index.html" (file-name-sans-extension htmlfile))
			    ":TEMPLATE: page_redirect.html"
			    ":END:")
			  "\n"))
	      (org-mode)
	      (goto-char (point-min))
	      (let ((redir (ob:parse-entry self (point-marker) 'page)))
		(message "REDIR: %S" redir)
		(set-slot-value redir 'path-to-root
				(concat "../" (ob:get 'path-to-root entry)))
		(set-slot-value self 'pages
				(append (ob:get 'pages self)
				      (list redir))))))

	  ;; copy all files into their target directory.
	  (message "Copy file: %S" ret)
	    
	  (loop for f in ret
		do (let ((target
			  (if page
			      (format "%s/%s"
 				      (ob:blog-publish-dir BLOG)
				      (file-name-nondirectory f))
			    (format "%s/%s/%s"
				    (ob:blog-publish-dir BLOG)
				    ;; file path is nil when exporting static page?
				    ;;(or filepath ".")
				    (file-name-sans-extension htmlfile)
				    (file-name-nondirectory f)))))
		     (mkdir (file-name-directory target) t)
		     (message "COPY %s -> %s" f target)
		     (ob-do-copy f target))))))))


(provide 'o-blog-backend-org)

;; o-blog-backend-org.el ends here
