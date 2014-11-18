;;; o-blog-framework.el --- Base class for o-blog frameworks

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-06-05
;; Last changed: 2014-11-18 23:42:36
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'cl)
(require 'htmlize nil t)


(cl-defstruct (ob:framework-component)
  name
  backends
  start
  end
  alias)


(setq ob:framework-components
  (list
   (make-ob:framework-component
    :name 'lead :backends '(org)
    :start "<div class=\"lead\">"
    :end "</div>")

   (make-ob:framework-component
    :name 'excerpt :backends '(org)
    :start "<div class=\"lead\"><!--excerpt-->"
    :end "<!--/excerpt--></div>")

   (make-ob:framework-component
    :name 'mark :backends '(org)
    :start "<mark>"
    :end "</mark>")

   (make-ob:framework-component
    :name 'del :backends '(org)
    :start "<del>"
    :end "</del>")

   (make-ob:framework-component
    :name 'kbd :backends '(org)
    :start "<kbd>"
    :end "</kbd>")

   
   (make-ob:framework-component
    :name 'right :backends '(org)
    :start "<p class=\"text-right\">"
    :end "</p>")

   (make-ob:framework-component
    :name 'left :backends '(org)
    :start "<p class=\"text-left\">"
    :end "</p>")

   (make-ob:framework-component
    :name 'justify :backends '(org)
    :start "<p class=\"text-justify\">"
    :end "</p>")

   (make-ob:framework-component
    :name 'center :backends '(org)
    :start "<p class=\"text-center\">"
    :end "</p>")

   (make-ob:framework-component
    :name 'nowrap :backends '(org)
    :start "<p class=\"text-nowrap\">"
    :end "</p>")

   
   
   
   (make-ob:framework-component
    :name 'jumbotron :backends '(org)
    :start "<div class=\"jumbotron\">"
    :end "</div>")
   (make-ob:framework-component
    :name 'page-header :backends '(org)
    :start '(format
	     "<div class=\"page-header\"><h1>%s%s</h1></div>"
	     title
	     (if (boundp 'subtitle)
		 (format " <small>%s</small>" subtitle)
	       "")))
   (make-ob:framework-component
    :name 'caption :backends '(org)
    :start '(format
	     "<div class=\"caption\">%s"
	     (if (boundp 'title)
		 (format "<h3>%s</h3>" title)
	       ""))
    :end "</div>")
   (make-ob:framework-component
    :name 'thumbnail :backends '(org)
    :start "<div class=\"thumbnail\">"
    :end "</div>")

   (make-ob:framework-component
    :name 'glyphicon :backends '(org)
    :start '("<span class=\"glyphicon glyphicon-" icon "\"></span>"))
   (make-ob:framework-component
    :name 'icon :backends '(org)
    :start '("<i class=\"fa fa-" icon "\"></i>"))
   (make-ob:framework-component
    :name 'row :backends '(org)
    :start (format "<div class=\"row%s\">"
		   (if (boundp 'equal) " equal" ""))
    :end "</div>")
   (make-ob:framework-component
    :name 'col :backends '(org)
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
   (make-ob:framework-component
    :name 'panel :backends '(org)
    :start '("<div class=\"panel"
	     (when (boundp 'alt) (format " panel-%s" alt))
	     "\">")
    :end "</div>")
   (make-ob:framework-component
    :name 'panel-heading :backends '(org)
    :start '("<div class=\"panel-heading\">"
	     (when (boundp 'title)
	       (format "<h3 class=\"panel-title\">%s</h3>" title)))
    :end "</div>")
   (make-ob:framework-component
    :name 'panel-body :backends '(org)
    :start '("<div class=\"panel-body\">")
    :end "</div>")
   (make-ob:framework-component
    :name 'panel-footer :backends '(org)
    :start '("<div class=\"panel-footer\">")
    :end "</div>")
   (make-ob:framework-component
    :name 'label :backends '(org)
    :start '(format "<span class=\"label label-%s\">"
		    (if (boundp 'mod) mod "default"))
    :end "</span>")
   (make-ob:framework-component
    :name 'badge :backends '(org)
    :start "<span class=\"badge\">"
    :end "</span>")
   (make-ob:framework-component
    :name 'alert :backends '(org)
    :start '(format "<div class=\"alert alert-%s\">"
		    (if (boundp 'mod) mod "warning"))
    :end "</div>")
   (make-ob:framework-component
    :name 'well :backends '(org)
    :start '(format "<div class=\"well well-%s\">"
		    (if (boundp 'mod) mod "lg"))
    :end "</div>")
   (make-ob:framework-component
    :name 'thumbnail :backends '(org)
    :start "<div class=\"thumbnail\">"
    :end "</div>")

   (make-ob:framework-component
    :name 'table :backends '(org)
    :start '(format "<table class=\"table %s\">"
		    (loop for v in '(striped bordered hover condensed)
			  with out = '()
			  when (boundp v)
			  collect (format "table-%s" v) into out
			  finally return (mapconcat 'identity out " ")))
			  
    :end "</table>")

   (make-ob:framework-component
    :name 'tr :backends '(org)
    :start '(format "<tr%s>" (if (boundp 'mod) (format " class=\"%s\"" mod) ""))
    :end "</tr>")

   (make-ob:framework-component
    :name 'td :backends '(org)
    :start '(format "<td%s>" (if (boundp 'mod) (format " class=\"%s\"" mod) ""))
    :end "</td>")

   (make-ob:framework-component
    :name 'source :backends '(org)
    :start '(let* ((mode (or (and (boundp 'mode) mode) "fundamental"))
		   (cur-point (point))
		   (end-point (unless (boundp 'src-file)
				(save-match-data
				  (search-forward (format "</%s>" tag)))))
		   (content (when end-point
			      (buffer-substring-no-properties (1+ cur-point)
			       (- end-point (length (format "</%s>" tag))))))
		   (func (setq func (intern (format "%s-mode" mode))))
		   html)

	      (with-temp-buffer
		(if (boundp 'src-file)
		    (progn
		      (insert-file-contents src-file)
		      (unless func
			(setq func (assoc-default src-file auto-mode-alist
						  'string-match))))
		  (insert content))
		(if (functionp func)
		    (funcall func)
		  (warn (concat "Mode %s not found for %s. "
				"Consider installing it. "
				"No syntax highlight would be bone this time.")
			mode (if (boundp 'src-file) src-file "inline")))
		;; Unfortunately rainbow-delimiter-mode does not work fine.
		;; See https://github.com/jlr/rainbow-delimiters/issues/5
		(font-lock-fontify-buffer)
		(if (functionp 'htmlize-region-for-paste)
		    (setf html (htmlize-region-for-paste (point-min) (point-max)))
		  (warn "htmlize not installed try to export simple buffer content")
		  (setf html (format "<pre>%s</pre>"
				     (buffer-substring-no-properties
				      (point-min) (point-max))))))

	      (when end-point
		(delete-region cur-point (- end-point (length (format "</%s>" tag)))))
	      (format "<div class=\"src %s\">%s</div>" (or mode "") html)))
   (make-ob:framework-component :name 'src :backends '(org) :alias 'source)

   (make-ob:framework-component
    :name 'columns :backends '(org)
    :start '(format "<div style=\"%s%s\">"
		    (if (boundp 'width)
			(format "column-width:%s;-webkit-column-width:%s;-moz-column-width:%s" width width width) "")
		    (if (boundp 'count) (format " column-count:%s;-webkit-column-count:%s;-moz-column-count:%s;" count count count) ""))
    :end "</div>")


   (make-ob:framework-component
    :name 'copy :backends '(org)
    :start '(progn
	      (when (and object
			 (boundp 'src))
		(let ((files-to-copy (ob:get 'files-to-copy object)))
		  (add-to-list 'files-to-copy src)
		  (%ob:set object 'files-to-copy files-to-copy)))
	      ""))
   ))

(defun ob:framework-expand (re-start re-end &optional prefix suffix comment object)
  "Expand framework widgets using RE-START and RE-END to delimit notations,
convert widget to their HTML notation. COMMENT is an escape
string to prevent widget expansion.

RE-START is a 2-group regexp. First group is the widget name,
second one is a list of parameters.

RE-END is passed to `format' with widget name as parameter.

OBJECT is the current post object.

"
  (let ((prefix (or prefix "@@html:"))
	(suffix (or suffix "@@"))
	(items (loop for c in ob:framework-components
		     collect (cons (ob:get 'name c) c)))
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
			    (or (match-string-no-properties 2) "")))
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

	      (when (ob:get 'alias replacement)
		(setq replacement
		      (cdr (assoc (ob:get 'alias replacement) items))))

		(when (and replacement (not commented))
		;; Convert the attribute list to variables
		(cl-progv
		    (mapcar #'car attr) (mapcar #'cdr attr)
		  ;; Replace first tag
		  (narrow-to-region beg end)
		  (delete-region (point-min) (point-max))
		  (widen)
		  (insert
		   prefix
		   (ob:string-template (ob:get 'start replacement))
		   suffix)

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




(provide 'o-blog-framework)

;; o-blog-framework.el ends here
