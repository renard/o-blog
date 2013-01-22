;;; o-blog-backend-org.el --- org-mode backend for o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-12-04
;; Last changed: 2013-01-22 14:35:12
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
	      (insert "#+OPTIONS: H:7 num:nil  toc:nil d:nil todo:nil <:nil pri:nil tags:nil\n\n"))
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


(defmethod ob:parse-entry ((self ob:backend:org) marker)
  ""
  (save-excursion
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (when (search-forward-regexp org-complex-heading-regexp
				   (point-at-eol)
				   t)
	(let ((article (ob:article marker)))

	  (set-slot-value
	   article 'title
	   (match-string-no-properties 4))

	  (set-slot-value
	   article 'timestamp
	   (apply 'encode-time
		  (org-parse-time-string
		   (or (org-entry-get (point) "CLOSED")
		       (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S %u")))))

	  (ob:article:compute-dates article)

	  (set-slot-value
	   article 'source
	   (ob:org-get-entry-text self))

	  article)))))


(defmethod ob:parse-entries-1 ((self ob:backend:org) type)
  (let ((markers (org-map-entries
		  'point-marker
		  (slot-value self (intern (format "%s-filter" type)))
		  'file-with-archives)))
    
    (loop for marker in markers
	  collect (ob:parse-entry self marker))))

(defmethod ob:parse-entries ((self ob:backend:org))
  (ob:with-source-buffer
   self
   (loop for type in '(articles pages snippets)
	 do (set-slot-value self type (ob:parse-entries-1 self type)))
   self))

(defmethod ob:convert-article ((self ob:backend:org))
  ""
  )

(provide 'o-blog-backend-org)

;; o-blog-backend-org.el ends here
