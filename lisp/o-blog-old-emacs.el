;;; o-blog-old-emacs.el --- old emacs CL functions

;; Copyright © 2014 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2014-12-30
;; Last changed: 2014-12-30 23:37:41
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 
;; On some old Emacs versions (before April 20th 2014) both
;; `cl-struct-slot-info' and `cl-struct-slot-offset' are not declared.
;
;; This is an extract of these functions to make o-blog work properly.
;;
;; Works for Emacs 24.3.1 through 24.4.1 (maybe other versions as well).
;;

;;; Code:

(require 'cl-macs)

(unless (functionp 'cl-struct-slot-info)
  (defun cl-struct-slot-info (struct-type)
    "Return a list of slot names of struct STRUCT-TYPE.
     Each entry is a list (SLOT-NAME . OPTS), where SLOT-NAME is a
     slot name symbol and OPTS is a list of slot options given to
     `cl-defstruct'.  Dummy slots that represent the struct name
     and slots skipped by :initial-offset may appear in the list."
    (get struct-type 'cl-struct-slots))

  (put 'cl-struct-slot-info 'side-effect-free t))

(unless (functionp 'cl-struct-slot-offset)
  (defun cl-struct-slot-offset (struct-type slot-name)
    "Return the offset of slot SLOT-NAME in STRUCT-TYPE. The
     returned zero-based slot index is relative to the start of the
     structure data type and is adjusted for any structure name
     and :initial-offset slots.  Signal error if struct STRUCT-TYPE
     does not contain SLOT-NAME."
    (or (cl-position slot-name
		     (cl-struct-slot-info struct-type)
		     :key #'car :test #'eq)
	(error "struct %s has no slot %s" struct-type slot-name)))
  
  (put 'cl-struct-slot-offset 'side-effect-free t))


(provide 'o-blog-old-emacs)

;; o-blog-old-emacs.el ends here
