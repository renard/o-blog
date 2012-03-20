;;; sample-init.el --- sample init file to be used to debug o-blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-03-20
;; Last changed: 2012-03-21 00:44:39
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(defun ob-build-sample ()
  (let* ((d default-directory)
	 (sample (format "%s/.emacs.d/o-blog/example/sample.org" d))
	 (debugger
	  (lambda (&rest debugger-args)
	    (let* ((trace (with-temp-buffer
			    (insert (with-output-to-string (backtrace)))
			    (beginning-of-buffer)
			    ;; error begins on line 16
			    ;; line stats with "  signal(error ..."
			    (goto-line 16)
			    (goto-char (point-at-bol))
			    ;; Remove null chars
			    (replace-regexp-in-string
			      (char-to-string 0) "^@"
			      (buffer-substring (point) (point-max))))))
	      (o-blog-bug-report trace))))
	 (debug-on-error t)
	 print-length print-level)
    (add-to-list 'load-path (format "%s/.emacs.d/org-mode/lisp" d))
    (add-to-list 'load-path (format "%s/.emacs.d/org-mode/contrib/lisp" d))
    (add-to-list 'load-path (format "%s/.emacs.d/o-blog" d))
    (require 'o-blog)
    (find-file sample)
    (org-publish-blog sample)
    (kill-emacs)))
