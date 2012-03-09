;;; o-blog-source.el --- Publish source in o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-01-23
;; Last changed: 2012-03-09 18:49:11
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'org nil t))


(defcustom o-blog-source-header
  (concat "<div class=\"o-blog-source\"><a class=\"btn btn-info\" data-toggle=\"modal\" data-target=\"#%s\" ><i class=\"icon-file icon-white\"></i>&nbsp;%s</a></div>"
	  "<div class=\"modal fade hide\" id=\"%s\"><div class=\"modal-header\"><a class=\"close\" data-dismiss=\"modal\">×</a><h3>%s</h3></div><div class=\"modal-body\">")
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
		"^#\\+O_BLOG_SOURCE:?[ \t]+\\(.+?\\)\\([ \t]+\\(.+\\)\\)?$"
		nil t)
	  (let* ((src-file (match-string 1))
		 (src-file-name (file-name-nondirectory src-file))
		 (src-file-safe (ob:sanitize-string src-file-name))
		 (mode (match-string 3)))

	    (beginning-of-line)
	    (delete-region (point) (point-at-eol))

	    (insert
	     "#+BEGIN_HTML\n"
	     (format o-blog-source-header
		     src-file-safe src-file-name src-file-safe
		     src-file-name)
	     (with-temp-buffer
	       (insert-file-contents src-file)
	       (if mode
		   (let ((func (intern (format "%s-mode" mode))))
		     (if (functionp func)
			 (funcall func)
		       (warn (concat "Mode %s not found for %s. "
				     "Consider installing it. "
				     "No syntax highlight would be bone this time.")
			     mode src-file)))
		 (set-auto-mode))
	       (font-lock-fontify-buffer)
	       (htmlize-region-for-paste (point-min) (point-max)))
	     o-blog-source-footer
	     "\n#+END_HTML\n")))))))

(add-to-list
 'org-structure-template-alist
 '("os" "#+o_blog_source ?\n"))
(add-hook 'o-blog-html-plugins-hook 'o-blog-publish-source)

(provide 'o-blog-source)
