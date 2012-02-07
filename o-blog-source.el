;;; o-blog-source.el --- Publish source in o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-01-23
;; Last changed: 2012-02-07 14:41:11
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'org nil t))


(defcustom o-blog-source-header
  "<div class=\"o-blog-source\"><div class=\"title\">%s</div><div style=\"display:none;\" class=\"content\">"
  "HTML fragment header to be used when publishing an source
using `o-blog-publish-source' using `format' with source
type as parameter. The source should be closed with
`o-blog-source-header'."
  :type 'string
  :group 'o-blog)

(defcustom o-blog-source-footer
  "</div></div>"
  "HTML fragment header to be used when publishing an source
using `o-blog-publish-source' as it. This closes
`o-blog-source-header'."
  :type 'string
  :group 'o-blog)


;;;###autoload
(defun o-blog-publish-source ()
  "Publish an sourced file in HTML mode.

A source file is defined using:

    #+O_BLOG_SOURCE: path/to/file [mode]

and is converted to 

    #+BEGIN_HTML
    <div class=\"o-blog-source\">
    <div class=\"title\">file</div>
    <div style=\"display:none;\" class=\"content\">
    #+END_HTML
    #+BEGIN_SRC mode
    (file content)
    #+END_SRC
    #+BEGIN_HTML
    </div></div>
    #+END_HTML

The default replacement text could be changed using variables
`o-blog-source-header' and `o-blog-source-footer'."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward
		"^#\\+O_BLOG_SOURCE:?[ \t]+\\(.+?\\)\\([ \t]\+\\(.+\\)\\)?$"
		nil t)
	  (let* ((src-file (match-string 1))
		 (mode (match-string 2)))

	    (beginning-of-line)
	    (delete-region (point) (point-at-eol))

	    (insert
	     "#+BEGIN_HTML\n"
	     (format o-blog-source-header src-file)
	     "\n#+END_HTML\n"

	     (format
	      "#+BEGIN_SRC %s\n"
	      (if mode mode
		;; remove "-mode" from major mode
		(substring (symbol-name
			    (with-temp-buffer
			      (insert-file-contents src-file)
			      (set-auto-mode)
			      major-mode))
			   0 -5)))

	     (with-temp-buffer
	       (insert-file-contents src-file)
	       (buffer-string))
	     
	     "#+END_SRC\n"
	     "#+BEGIN_HTML\n"
	     o-blog-source-footer
	     "\n#+END_HTML\n")))))))

(add-to-list
 'org-structure-template-alist
 '("os" "#+o_blog_source ?\n"))


(provide 'o-blog-source)
