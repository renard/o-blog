;;; o-blog-backend-org.el --- org-mode backend for o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-12-04
;; Last changed: 2013-02-09 12:29:25
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'eieio nil t)
  (require 'o-blog-backend nil t))


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
  ""
  (ob:with-source-buffer
   self
   (save-excursion
     (save-restriction
       (save-match-data
	 (widen)
	 (goto-char (point-min))
	 (let (values)
	   (while (re-search-forward
		   (format "^#\\+%s:?[ \t]*\\(.*\\)" header) nil t)
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

	  (ob:entry:set-path entry)

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
	  collect (ob:parse-entry self marker type))))

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

(defmethod ob:org-fix-html ((self ob:backend:org) html)
  "Perform some html fixes on org html export."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (ob:org-fix-html-level-numbering self)

    (buffer-substring-no-properties (point-min) (point-max))))

(defmethod ob:convert-entry ((self ob:backend:org) entry)
  "Convert ENTRY to html using `org-mode' syntax."
  (with-temp-buffer
    (insert (oref entry source))
    (org-mode)
    (goto-char (point-min))
    ;; exporting block with ditaa is kinda messy since it requires a real
    ;; file (does not work with a temp-buffer which is not associated to any
    ;; file).
    (let ((saved-file
	   (when
	       (re-search-forward "^#\\+BEGIN_SRC:?[ \t]+\\(ditaa\\)" nil t)
	     (format "/%s/%s/%s.src.org"
		     default-directory
		     (ob:blog-publish-dir BLOG)
		     ;; variable inherited from `ob-parse-entry'
		     htmlfile)))
	  (org-confirm-babel-evaluate nil)
	  ret)
      (when saved-file
	(ob-write-file saved-file))
      (let ((html (substring-no-properties
		   ;; `org-export-as-html' arguments has changed on new
		   ;; org-version, then again with the new exporter.
		   ;; First try old function signatures, then on failure
		   ;; use new argument call.
		   (or
		    (ignore-errors (org-export-as-html nil nil nil 'string t))
		    (ignore-errors (org-export-as-html nil nil 'string t))
		    (org-export-as 'html nil nil t nil)))))
	(set-slot-value entry 'html
			(ob:org-fix-html self html)))
      (when saved-file
	(delete-file saved-file))
      entry)))

(provide 'o-blog-backend-org)

;; o-blog-backend-org.el ends here
