;;; o-blog-tag.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-02-09
;; Last changed: 2014-07-09 01:07:39
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 

;;; Code:


(cl-defstruct
    (ob:category
     (:constructor
      make-ob:category (display &aux (safe (ob:sanitize-string display)))))
  display
  safe)

(cl-defstruct
    (ob:tag
     (:include ob:category)
     (:constructor
      make-ob:tag (display &key (count 0) (size 0)
			   &aux (safe (ob:sanitize-string display)))))
  count
  size)

(provide 'o-blog-tag)

;; o-blog-tag.el ends here
