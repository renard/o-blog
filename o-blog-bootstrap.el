;;; o-blog-bootstrap.el --- Bootsrap features for o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-05-03
;; Last changed: 2012-05-07 14:12:41
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(defvar ob-bootstrap-label-and-badge-connotations
  '("default" "success"
    "warning" "important" "info"
    "inverse")
  "Bootstrap badge and label connotations.")

(defvar ob-bootstrap-alert-connotations
  '("info" "success" "warning" "danger")
  "Bootstrap alert connotations.")


(defun ob-bootstrap-link-dont-open (path) "Do nothing")

(defun ob-add-link-protocol (protocol export)
  "Add PROTOCOL to `org-linkl-protocols' and use EXPORT for
publication.

See also `org-add-link-type'."
  (when (assoc protocol org-link-protocols)
    (adelete 'org-link-protocols protocol))
  (org-add-link-type protocol 'ob-bootstrap-link-dont-open export))


(defun ob-bootstrap-publish-label-or-badge (path desc format type)
  "Publish badge or label using PATH DESC and FORMAT (see
`org-add-link-type' for more details) using TYPE which can be
either \"label\" org \"badge\"."
  (let ((extra (if (member path ob-bootstrap-label-and-badge-connotations)
		   (format " %s-%s" type path) "")))
    (cond
     ((eq format 'html)
      (format "<span class=\"%s%s\">%s</span>" type extra desc)))))

(defun ob-bootstrap-publish-label (path desc format)
  "Publish bootstrap label. A badge is defined using org link
syntax:

  [[ob-label:TYPE][TEXT]]

Where TEXT is the labeled text and TYPE is the label connotation
type (one of `ob-bootstrap-label-and-badge-connotations').

See also `ob-bootstrap-publish-badge'."
  (ob-bootstrap-publish-label-or-badge path desc format "label"))
(ob-add-link-protocol "ob-label" 'ob-bootstrap-publish-label)

(defun ob-bootstrap-publish-badge (path desc format)
  "Publish bootstrap badge. A label is defined using org link
syntax:

  [[ob-badge:TYPE][TEXT]]

Where TEXT is the badged text and TYPE is the label connotation
type (one of `ob-bootstrap-label-and-badge-connotations').

See also `ob-bootstrap-publish-label'."
  (ob-bootstrap-publish-label-or-badge path desc format "badge"))
(ob-add-link-protocol "ob-badge" 'ob-bootstrap-publish-badge)



(defun ob-bootstrap-publish-progress (path desc format)
  "Publish bootstrap progress bar. A progress is defined using
org link syntax:

  [[ob-progress:TYPE(,OPTS,...)][PERCENT]]

Where PERCENT is the progress percentage, TYPE is the progress
connotation type (one of `ob-bootstrap-alert-connotations') and
OPTS are comma separated options:
  - striped to ass stripes to the progress bar
  - active to activate stripes."
  (let ((extra (loop for elm in (split-string path ",")
		     collect (cond
			      ((or
				(member elm ob-bootstrap-alert-conotations)
				(string= elm "striped"))
			       (format "progress-%s" elm))
			      (t elm))
		     into ret
		     finally return (mapconcat 'identity ret " "))))
    (cond
     ((eq format 'html)
      (format
       "<div class=\"progress %s\"><div class=\"bar\" style=\"width: %s%%;\"></div></div>"
       extra desc)))))
(ob-add-link-protocol "ob-progress" 'ob-bootstrap-publish-progress)



(defun o-blog-publish-well()
  "Publish bootstrap well.

Use the well as a simple effect on an element to give it an inset
effect.

#+BEGIN_O_BLOG_WELL
Look, I'm in a well!
#+END_O_BLOG_WELL

The html result looks like:

#+BEGIN_HTML
<div class=\"well\">
Look, I'm in a well!
</div>
#+END_HTML"

  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward "^#\\+BEGIN_O_BLOG_WELL" nil t)
	  (beginning-of-line)
	  (insert
	   "#+BEGIN_HTML\n"
	   "<div class=\"well\">\n"
	   "\n#+END_HTML\n")
	  (delete-region (point) (point-at-eol))
	  (unless
	      (re-search-forward "^#\\+END_O_BLOG_WELL" nil t)
	    (error "#+END_O_BLOG_WELL not found in %s@%s." (buffer-file-name)
		   (point)))
	  (beginning-of-line)
	  (insert
	   "\n#+BEGIN_HTML\n"
	   "</div>\n"
	   "\n#+END_HTML\n")
	  (delete-region (point) (point-at-eol)))))))
(add-to-list
 'org-structure-template-alist
 '("ow" "#+begin_o_blog_well\n\n#+end_o_blog_well"))
(add-hook 'o-blog-html-plugins-hook 'o-blog-publish-well)



(defun o-blog-publish-hero-unit()
  "Publish bootstrap hero-unit using O_BLOG_HERO_UNIT block in a
hero-unit HTML div.

#+BEGIN_O_BLOG_HERO_UNIT
I am a hero unit!
#+END_O_BLOG_HERO_UNIT

Would be converted to

#+BEGIN_HTML
<div class=\"hero-unit\">
I am a hero unit!
</div>
#+END_HTML"
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward "^#\\+BEGIN_O_BLOG_HERO_UNIT" nil t)
	  (beginning-of-line)
	  (insert
	   "#+BEGIN_HTML\n"
	   "<div class=\"hero-unit\">\n"
	   "\n#+END_HTML\n")
	  (delete-region (point) (point-at-eol))
	  (unless
	      (re-search-forward "^#\\+END_O_BLOG_HERO_UNIT" nil t)
	    (error "#+END_O_BLOG_HERO_UNIT not found in %s@%s." (buffer-file-name)
		   (point)))
	  (beginning-of-line)
	  (insert
	   "\n#+BEGIN_HTML\n"
	   "</div>\n"
	   "\n#+END_HTML\n")
	  (delete-region (point) (point-at-eol)))))))
(add-to-list
 'org-structure-template-alist
 '("ohu" "#+begin_o_blog_hero_unit\n\n#+end_o_blog_hero_unit"))
(add-hook 'o-blog-html-plugins-hook 'o-blog-publish-hero-unit)


(defun o-blog-publish-page-header()
  "Publish bootstrap page-header using O_BLOG_PAGE_HEADER block in a
page-header HTML div.

#+BEGIN_O_BLOG_PAGE_HEADER
I am a page header!
#+END_O_BLOG_PAGE_HEADER

Would be converted to

#+BEGIN_HTML
<div class=\"page-header\">
I am a page header!
</div>
#+END_HTML"
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward "^#\\+BEGIN_O_BLOG_PAGE_HEADER" nil t)
	  (beginning-of-line)
	  (insert
	   "#+BEGIN_HTML\n"
	   "<div class=\"page-header\">\n"
	   "\n#+END_HTML\n")
	  (delete-region (point) (point-at-eol))
	  (unless
	      (re-search-forward "^#\\+END_O_BLOG_PAGE_HEADER" nil t)
	    (error "#+END_O_BLOG_PAGE_HEADER not found in %s@%s." (buffer-file-name)
		   (point)))
	  (beginning-of-line)
	  (insert
	   "\n#+BEGIN_HTML\n"
	   "</div>\n"
	   "\n#+END_HTML\n")
	  (delete-region (point) (point-at-eol)))))))
(add-to-list
 'org-structure-template-alist
 '("oph" "#+begin_o_blog_page_header\n\n#+end_o_blog_page_header"))
(add-hook 'o-blog-html-plugins-hook 'o-blog-publish-page-header)




(provide 'o-blog-bootstrap)
