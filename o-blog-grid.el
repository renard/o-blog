;;; o-blog-grid.el --- Publish grid in o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-01-23
;; Last changed: 2012-02-10 19:46:13
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'org nil t))


(defcustom o-blog-grid-header
  "<div class=\"row %s\"><div class=\"%s\">"
  "HTML fragment header to be used when publishing a grid using
`o-blog-publish-grid' using `format' with grid options as
parameter. The grid should be closed with `o-blog-grid-footer'."
  :type 'string
  :group 'o-blog)

(defcustom o-blog-grid-footer
  "</div></div>"
  "HTML fragment footer to be used when publishing a grid using
`o-blog-publish-grid' as it. This closes `o-blog-grid-header'."
  :type 'string
  :group 'o-blog)

(defcustom o-blog-grid-column
  "</div><div class=\"%s\">"
  "HTML middle fragment to be used when publishing a grid. This
string is used for next column in `o-blog-publish-grid' using
`format' with column configuration as parameter."
  :type 'string
  :group 'o-blog)


(defun o-blog-grig-compute-args (a1 a2)
  "Compute A1 and A2."
  (let* ((i2c (lambda (x)
		(let ((x (if x (string-to-int x) 0)))
		  (cond
		   ((= 0 x) "")
		   ((< 0 x) (format "span%d" x))
		   ((> 0 x) (format "offset%d" (- x))))))))
    (format "%s %s"
	    (funcall i2c a1)
	    (funcall i2c a2))))

;;;###autoload
(defun o-blog-publish-grid ()
  "Publish a grid in HTML mode.

The default replacement text could be changed using variables
`o-blog-grid-header', `o-blog-grid-footer' and
`o-blog-grid-title'."
  (save-match-data
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let ((case-fold-search t))
	  (while (re-search-forward
		  "^#\\+BEGIN_O_BLOG_ROW:?[ \t]+\\(.*\\)"
		  nil t)
	    (org-narrow-to-subtree)
	    (goto-char (point-at-bol))
	    (let* ((beg (point))
		   (args (when (stringp (match-string 1))
			   (split-string (match-string-no-properties 1)))))
		       
	      (insert
	       "#+BEGIN_HTML\n"
	       (format o-blog-grid-header
		       (or (nth 2 args) "")
		       (o-blog-grig-compute-args
			      (nth 0 args)
			      (nth 1 args)))
	       "\n#+END_HTML\n")
	      (delete-region (point) (point-at-eol))

	      (save-excursion
		(goto-char (point-max))
		(unless
		    (re-search-backward "^#\\+END_O_BLOG_ROW" beg t)
		  (error "#+END_O_BLOG_ROW not found in %s@%s." (buffer-file-name)
			 (point)))
		(beginning-of-line)
		(insert
		 "\n#+BEGIN_HTML\n"
		 o-blog-grid-footer
		 "\n#+END_HTML\n")
		(delete-region (point) (point-at-eol))
		(setq end (point)))

	      (save-excursion
		(while (re-search-forward
			"^#\\+O_BLOG_ROW_COLUMN:?[ \t]+\\(-?[0-9]+\\)\\([ \t]+\\(-?[0-9]+\\)\\)?"
			nil t)

		  (goto-char (point-at-bol))

		  (insert
		   "#+BEGIN_HTML\n"
		   (format o-blog-grid-column
			   (o-blog-grig-compute-args
			    (match-string 1)
			    (match-string 2)))
		   "\n#+END_HTML\n")

		  (delete-region (point) (point-at-eol)))))

	    (widen)))))))
	      


(add-to-list
 'org-structure-template-alist
 '("og" "#+begin_o_blog_row ?\n\n#+end_o_blog_row"))
(add-to-list
 'org-structure-template-alist
 '("ogr" "#+o_blog_row_column ?\n\n"))


(add-hook 'o-blog-html-plugins-hook 'o-blog-publish-grid)

(provide 'o-blog-grid)
