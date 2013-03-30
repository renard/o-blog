;;; o-blog-obsolete.el --- Obsolete functions

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-03-30
;; Last changed: 2013-03-30 20:48:07
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile
  (require 'cl))


(defun ob:make-obsolete (structure slot)
  ""
  (let ((func (intern (format "ob:%s-%s" structure slot))))
    (fset func
	  `(lambda (X)
	     "Access to deprecated o-blog structure."
	     (ob:get (quote ,slot) X)))
    (make-obsolete func 'ob:get)))
    

(loop for s in '(file buffer publish-dir template-dir style-dir
  assets-dir posts-filter static-filter snippet-filter title
  description url language post-build-shell default-category
  disqus analytics filename-sanitizer posts-sorter posts-filepath
  posts-htmlfile)
      do (ob:make-obsolete 'blog s))

(loop for s in '(id title timestamp year month day category tags
  template filepath filename htmlfile path-to-root content
  content-html sitemap)
        do (ob:make-obsolete 'post s))


(loop for s in '(name safe count size)
        do (ob:make-obsolete 'tags s))

(loop for s in '(name safe)
        do (ob:make-obsolete 'category s))



(provide 'o-blog-obsolete)

;; o-blog-obsolete.el ends here
