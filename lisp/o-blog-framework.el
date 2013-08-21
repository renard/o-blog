;;; o-blog-framework.el --- Base class for o-blog frameworks

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-06-05
;; Last changed: 2013-08-21 12:34:32
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'eieio nil t))


(defclass ob:framework-component nil
  ((backends :initarg :backends
	     :type (or symbol list)
	     :documentation "List of supported backends")
   (start :initarg :start
	  :type (or string list)
	  :documentation "Opening replacement from suitable for
	  `ob:string-template'")
   (end :initarg :end
	:type (or null string list)
	:documentation "Closing replacement from suitable for
	`ob:string-template'. If nil, no closing tag is required"))
  "Component used by ob:compute-framework")


(defvar ob:framework-components
  (list
   (ob:framework-component
    'jumbotron :backends '(org)
    :start "<div class=\"jumbotron\">"
    :end "</div>")
   (ob:framework-component
    'page-header :backends '(org)
    :start '(format
	     "<div class=\"page-header\"><h1>%s%s</h1></div>"
	     title
	     (if (boundp 'subtitle)
		 (format " <small>%s</small>" subtitle)
	       "")))
   (ob:framework-component
    'glyphicon :backends '(org)
    :start '("<span class=\"glyphicon glyphicon-" icon "\"></span>"))
   (ob:framework-component
    'icon :backends '(org)
    :start '("<span class=\"icon-" icon "\"></span>"))
   (ob:framework-component
    'row :backends '(org)
    :start "<div class=\"row\">"
    :end "</div>")
   (ob:framework-component
    'col :backends '(org)
    :start '("<div class=\""
	     (loop for i in '(xs sm md lg o-xs o-sm o-md o-lg)
		   when (boundp i)
		   collect (format
			    "col-%s-%s%s"
			    (car (last (split-string (symbol-name i) "-")))
			    (if (string-match "o-" (symbol-name i))
				"offset-" "")
			    (eval i))
		   into out
		   finally return (mapconcat #'identity out " "))
	     "\">")
    :end "</div>")
   (ob:framework-component
    'panel :backends '(org)
    :start '("<div class=\"panel"
	     (when (boundp 'alt) (format " panel-%s" alt))
	     "\">")
    :end "</div>")
   (ob:framework-component
    'panel-heading :backends '(org)
    :start '("<div class=\"panel-heading\">"
	     (when (boundp 'title)
	       (format "<h3 class=\"panel-title\">%s</h3>" title)))
    :end "</div>")
   (ob:framework-component
    'panel-body :backends '(org)
    :start '("<div class=\"panel-body\">")
    :end "</div>")
   (ob:framework-component
    'panel-footer :backends '(org)
    :start '("<div class=\"panel-footer\">")
    :end "</div>")
   (ob:framework-component
    'label :backends '(org)
    :start '(format "<span class=\"label label-%s\">"
		    (if (boundp 'mod) mod "default"))
    :end "</span>")
   (ob:framework-component
    'badge :backends '(org)
    :start "<span class=\"badge\">"
    :end "</span>")
   (ob:framework-component
    'alert :backends '(org)
    :start '(format "<div class=\"alert alert-%s\">"
		    (if (boundp 'mod) mod "warning"))
    :end "</div>")
   (ob:framework-component
    'well :backends '(org)
    :start '(format "<div class=\"well well-%s\">"
		    (if (boundp 'mod) mod "lg"))
    :end "</div>")
   (ob:framework-component
    'thumbnail :backends '(org)
    :start "<div class=\"thumbnail\">"
    :end "</div>")))

(defun ob:framework-expand (&optional re-start re-end prefix suffix comment)
  ""
  (let ((prefix (or prefix "@@html:"))
	(suffix (or suffix "@@"))
	(re-start (or re-start "^#\\+\\(?:begin_\\)?\\([^ \n\t:]+\\):?\\([^\n>]+\\)?$"))
	(re-end (or re-end "^#\\+end_%s"))
	(items (loop for c in ob:framework-components
		     collect (cons (ob:get-name c) c)))
	(comment (concat "\\s-" (or comment ","))))
    
    (save-match-data
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  ;; Lookup for a XML-like tag.
	  ;; Make sure tag does not start with ":" since "<:nil" is a valid
	  ;; org-mode option
	  (while (re-search-forward re-start nil t)
	    (let* (;; Memorize match as XML string and match positions
		   (xml
		    (format "<%s%s>"
			    (match-string-no-properties 1)
			    (match-string-no-properties 2)))
		   (beg (+ (match-beginning 0)
			   (if (string= "," (match-string 1)) 1 0)))
		   (end (match-end 0))
		   ;; Make sure XML fragment is valid. If so, xml-parsed
		   ;; should be not nil and look like:
		   ;;
		   ;; (tag ((attr1 . "value1") (attrN . "valueN")))
		   (xml-parsed
		    (with-temp-buffer
		      (insert xml)
		      (unless (search-backward "/>" nil t 1)
			(delete-backward-char 1)
			(insert "/>"))
		      (libxml-parse-xml-region (point-min) (point-max))))
		   (tag (car xml-parsed))
		   (attr (cadr xml-parsed))
		   (replacement (cdr (assoc tag items)))
		   ;; , is used to comment out tagging
		   (commented
		    (save-match-data
		      (save-excursion
			(beginning-of-line)
			(when (re-search-forward comment (point-at-eol) t)
			  (delete-backward-char 1) t)))))

	      (when (and replacement (not commented))
		;; Convert the attribute list to variables
		(cl-progv
		    (mapcar #'car attr) (mapcar #'cdr attr)
		  ;; Replace first tag
		  (narrow-to-region beg end)
		  (delete-region (point-min) (point-max))
		  (insert
		   prefix
		   (ob:string-template (ob:get 'start replacement))
		   suffix)
		  (widen)
		  ;; If tag must be closed, look up for closing tag
		  (when (ob:get 'end replacement)
		    (save-match-data
		      (save-excursion
			(unless (re-search-forward (format re-end tag) nil t)
			  (error "Unclosed %s tag (%s)" tag (format re-end tag)))
			(narrow-to-region (match-beginning 0) (match-end 0))
			(delete-region (point-min) (point-max))
			(insert
			 prefix
			 (ob:string-template (ob:get 'end replacement))
			 suffix)
			(widen)))))))))))))




(defclass ob:framework nil
  ()
  "")




;;
;; Methods that must be overridden
;;
(defmethod ob:compute-framework ((self ob:framework))
  "Compute framework specific extentions. This method MUST be
overriden in subclasses."
  (message "Method `%s' is not defined in class `%s'."
	   'ob:compute-framework (object-class self)))





(defvar ob:framework-compents
  


(provide 'o-blog-framework)

;; o-blog-framework.el ends here
