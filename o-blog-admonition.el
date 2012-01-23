;;; o-blog-admonition.el --- Publish admonition in o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-01-23
;; Last changed: 2012-01-23 18:10:53
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'org nil t))


(defcustom o-blog-admonition-header
  "<div class=\"alert-message block-message warning %s\">"
  "HTML fragment header to be used when publishing an admonition
using `o-blog-publish-admonition' using `format' with admonition
type as parameter. The admonition should be closed with
`o-blog-admonition-header'."
  :type 'string
  :group 'o-blog)

(defcustom o-blog-admonition-footer
  "</div>"
  "HTML fragment header to be used when publishing an admonition
using `o-blog-publish-admonition' as it. This closes
`o-blog-admonition-header'."
  :type 'string
  :group 'o-blog)


(defcustom o-blog-admonition-title
  "<p class=\"admonition-header\"><strong>%s</strong></p>"
  "HTML fragment header to be used when publishing an admonition
tile \(if defined\) `o-blog-publish-admonition' using `format'
with admonition tile as parameter."
  :type 'string
  :group 'o-blog)



;;;###autoload
(defun o-blog-publish-admonition ()
  "Publish an admonition in HTML mode.

Admonitions are specially marked topics that can appear
anywhere an ordinary body element can. They contain arbitrary
body elements. Typically, an admonition is rendered as an offset
block in a document, sometimes outlined or shaded, with a title
matching the admonition type. For example:

#+BEGIN_O_BLOG_ADMONITION type title
Some text inside the admonition
#+END_O_BLOG_ADMONITION

Where type can be on of:

  - info
  - success
  - warning
  - error

This directive might be rendered something like this:

+---------------------------------+
| Title                           |
|                                 |
| Some text inside the admonition |
+---------------------------------+

In an HTML context, previous directive would be expanded as:

#+BEGIN_HTML
<div class=\"alert-message block-message warning type\">
<p class=\"admonition-header\"><strong>Title</strong></p>
#+END_HTML
Some text inside the admonition
#+BEGIN_HTML
<div>
#+END_HTML

The default replacement text could be changed using variables
`o-blog-admonition-header', `o-blog-admonition-footer' and
`o-blog-admonition-title'."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward "^#\\+BEGIN_O_BLOG_ADMONITION:?[ \t]+\\(info\\|success\\|warning\\|error\\)[ \t]*\\(.*\\)" nil t)
	  (let* ((admo-type (match-string 1))
		 (admo-title (match-string 2)))
	    (beginning-of-line)
	    (insert
	     "#+BEGIN_HTML\n"
	     (format o-blog-admonition-header admo-type)
	     (when admo-title 
	       (format o-blog-admonition-title admo-title))
	     "\n#+END_HTML\n")
	    (delete-region (point) (point-at-eol))
	    (unless
		(re-search-forward "^#\\+END_O_BLOG_ADMONITION" nil t)
	      (error "#+END_ADMONITION not found in %s@%s." (buffer-file-name)
		     (point)))
	    (beginning-of-line)
	    (insert
	     "\n#+BEGIN_HTML\n"
	     o-blog-admonition-footer
	     "\n#+END_HTML\n")
	    (delete-region (point) (point-at-eol))))))))
(add-to-list
 'org-structure-template-alist
 '("oa" "#+begin_o_blog_admonition ?\n\n#+end_o_blog_admonition"))


(provide 'o-blog-admonition)
