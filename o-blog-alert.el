;;; o-blog-alert.el --- Publish alert in o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-01-23
;; Last changed: 2012-02-09 10:27:22
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'org nil t))


(defcustom o-blog-alert-header
  "<div class=\"alert alert-%s\">"
  "HTML fragment header to be used when publishing an alert
using `o-blog-publish-alert' using `format' with alert
type as parameter. The alert should be closed with
`o-blog-alert-header'."
  :type 'string
  :group 'o-blog)

(defcustom o-blog-alert-footer
  "</div>"
  "HTML fragment header to be used when publishing an alert
using `o-blog-publish-alert' as it. This closes
`o-blog-alert-header'."
  :type 'string
  :group 'o-blog)

(defcustom o-blog-alert-title
  "<p class=\"alert-heading\">%s</p>"
  "HTML fragment header to be used when publishing an alert
tile \(if defined\) `o-blog-publish-alert' using `format'
with alert tile as parameter."
  :type 'string
  :group 'o-blog)


;;;###autoload
(defun o-blog-publish-alert ()
  "Publish an alert in HTML mode.

Admonitions are specially marked topics that can appear
anywhere an ordinary body element can. They contain arbitrary
body elements. Typically, an alert is rendered as an offset
block in a document, sometimes outlined or shaded, with a title
matching the alert type. For example:

#+BEGIN_O_BLOG_ALERT type title
Some text inside the alert
#+END_O_BLOG_ALERT

Where type can be on of:

  - info
  - success
  - warning
  - error

This directive might be rendered something like this:

+----------------------------+
| Title                      |
|                            |
| Some text inside the alert |
+----------------------------+

In an HTML context, previous directive would be expanded as:

#+BEGIN_HTML
<div class=\"alert alert-type\">
<p class=\"alert-heading\">Title</p>
#+END_HTML
Some text inside the alert
#+BEGIN_HTML
<div>
#+END_HTML

The default replacement text could be changed using variables
`o-blog-alert-header', `o-blog-alert-footer' and
`o-blog-alert-title'."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward "^#\\+BEGIN_O_BLOG_ALERT:?[ \t]+\\(info\\|success\\|warning\\|error\\)[ \t]*\\(.*\\)" nil t)
	  (let* ((admo-type (match-string 1))
		 (admo-title (match-string 2)))
	    (beginning-of-line)
	    (insert
	     "#+BEGIN_HTML\n"
	     (format o-blog-alert-header admo-type)
	     (when admo-title 
	       (format o-blog-alert-title admo-title))
	     "\n#+END_HTML\n")
	    (delete-region (point) (point-at-eol))
	    (unless
		(re-search-forward "^#\\+END_O_BLOG_ALERT" nil t)
	      (error "#+END_ALERT not found in %s@%s." (buffer-file-name)
		     (point)))
	    (beginning-of-line)
	    (insert
	     "\n#+BEGIN_HTML\n"
	     o-blog-alert-footer
	     "\n#+END_HTML\n")
	    (delete-region (point) (point-at-eol))))))))
(add-to-list
 'org-structure-template-alist
 '("oa" "#+begin_o_blog_alert ?\n\n#+end_o_blog_alert"))
(add-hook 'o-blog-html-plugins-hook 'o-blog-publish-source)

(provide 'o-blog-alert)
