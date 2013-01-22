;;; o-blog-article.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-01-21
;; Last changed: 2013-01-22 01:02:58
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile
  (require 'eieio nil t))


(defclass ob:article ()
  ((title :initarg :title
	  :type string
	  :documentation "")
   (timestamp :initarg :timestamp
	      :type list
	      :documentation "")
   (year :initarg :year
	 :type integer
	 :documentation "")

   (month :initarg :month
	 :type integer
	 :documentation "")
   (day :initarg :day
	 :type integer
	 :documentation "")

   (source :initarg :source
	   :type string
	   :documentation "Article raw source")
   )
  "Object type handeling o-blog articles.")


(defmethod ob:article:compute-dates ((self ob:article))
  ""
  (let ((timestamp (oref self timestamp)))
    (when timestamp
      (loop for (p f) in '((year "%Y") (month "%m") (day "%d"))
	    do (set-slot-value
		article p
		(string-to-number
		 (format-time-string f timestamp)))))))




(provide 'o-blog-article)

;; o-blog-article.el ends here
