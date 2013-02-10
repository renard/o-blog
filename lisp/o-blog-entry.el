;;; o-blog-entry.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-01-21
;; Last changed: 2013-02-10 15:56:06
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile
  (require 'eieio nil t))


(defclass ob:entry ()
  ((title :initarg :title
	  :type string
	  :documentation "")
   (source :initarg :source
	   :type string
	   :documentation "Article raw source")
   (html :initarg :html
	 :type string
	 :documentation "Article html result")
   (path :initarg :path
	   :type string
	   :documentation "")
   (file :initarg :file
	   :type string
	   :documentation "")
   (htmlfile :initarg :htmlfile
	   :type string
	   :documentation "")
   )
  "Object type handeling o-blog entries.")




;; Class aliases

(defclass ob:article (ob:entry)
  ((timestamp :initarg :timestamp
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
   (tags :initarg :tag
	 :type list
	 :documentation "List of ob:tag")
   (category :initarg :category
	     ;;	   :type ob:category
	   :documentation "")
)
  "O-blog page article")

(defmethod ob:entry:set-path ((self ob:entry))
  ""
  (set-slot-value
   self 'path (format "%s"
		      (ob:sanitize-string (oref self title))))
  (set-slot-value
   self 'htmlfile (format "%s/%s"
		      (oref self path)
		      (oref self path))))



(defmethod ob:entry:compute-dates ((self ob:article))
  ""
  (let ((timestamp (oref self timestamp)))
    (when timestamp
      (loop for (p f) in '((year "%Y") (month "%m") (day "%d"))
	    do (set-slot-value
		self p
		(string-to-number
		 (format-time-string f timestamp)))))))

(defmethod ob:entry:set-path ((self ob:article))
  ""
  (set-slot-value
   self 'path (format "%s/%.4d/%.2d"
		      (ob:sanitize-string (oref (oref self category) safe))
		      (oref self year)
		      (oref self month)))
  (set-slot-value
   self 'file (format "%.2d_%s.html"
		      (oref self day)
		      (ob:sanitize-string (oref self title))))
  (set-slot-value
   self 'htmlfile (format "%s/%s"
		      (oref self path)
		      (oref self file))))


(defclass ob:page (ob:entry)
  nil
  "O-blog page class")
(defclass ob:snippet (ob:entry)
  nil
  "O-blog snippet class")

(provide 'o-blog-entry)

;; o-blog-entry.el ends here
