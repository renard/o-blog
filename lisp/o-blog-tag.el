;;; o-blog-tag.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-02-09
;; Last changed: 2013-02-09 14:39:49
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(defclass ob:tag ()
  ((display :initarg :display
	    :type string
	    :documentation "Displayed tag name string")
   (safe :initarg :safe
	 :type string
	 :documentation "HTML safe tag form")
   (count :initarg :count
	 :type integer
	 :documentation "Tag occurance in all articles")
   (size :initarg :size
	 :type float
	 :documentation "Tag html size")


)
  "")



(provide 'o-blog-tag)

;; o-blog-tag.el ends here
