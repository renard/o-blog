;;; o-blog-utils.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-01-22
;; Last changed: 2013-01-22 01:11:24
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(defun ob:replace-in-string (string replacement-list)
  "Perform a mass `replace-regexp-in-string' against STRING for
all \(regexp rep\) items from REPLACEMENT-LIST and return the
result string."
  (loop for (regexp rep) in replacement-list
	do (setf string (replace-regexp-in-string regexp rep string)))
  string)

(provide 'o-blog-utils)

;; o-blog-utils.el ends here
