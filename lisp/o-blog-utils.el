;;; o-blog-utils.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-01-22
;; Last changed: 2013-03-25 16:22:23
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

(defun ob:sanitize-string (s)
  "Sanitize string S by:

- converting all charcters ton pure ASCII
- replacing non alphanumerical chars to \"-\"
- down-casing all letters
- trimming leading and tailing \"-\""
  (loop for c across s
	with cd
	with gc
	with ret
	do (progn
	     (setf gc (get-char-code-property c 'general-category))
	     (setf cd (get-char-code-property c 'decomposition)))
	if (or (member gc '(Lu Ll Nd)) (= ?- c))
	collect (downcase
		 (char-to-string (if cd (car cd)  c)))
	into ret
	else if (member gc '(Zs))
	collect "-" into ret
	finally return (ob:replace-in-string
			(mapconcat 'identity ret "")
			'(("--+" "-")
			  ("^-+\\|-+$" "")))))

(defun ob:write-file (file)
  "Write current buffer to FILE. Ensure FILE directories are present."
  (mkdir (file-name-directory file) t)
  (let ((buffer (current-buffer)))
    (with-temp-file file
      (insert (with-current-buffer buffer (buffer-string))))))

(defun ob:eval-lisp()
  "Eval embeded lisp code defined by <lisp> tags in html fragment
when publishing a page."
  (save-excursion
    (save-restriction
      (save-match-data
	;; needed for thing-at-point
	(html-mode)
	(beginning-of-buffer)
	(let ((open-tag "<lisp>\\|{lisp}\\|\\[lisp\\]")
	      (close-tag "</lisp>\\|{/lisp}\\|\\[/lisp\\]")
	      beg end sexp)
	  (while (search-forward-regexp open-tag nil t)
	    (setq beg (- (point) (length  (match-string 0))))
	    (when (search-forward-regexp close-tag nil t)
	      (setq end (point))
	      (backward-char (length (match-string 0)))
	      (backward-sexp)
	      (setq sexp (substring-no-properties (thing-at-point 'sexp)))
	      (narrow-to-region beg end)
	      (delete-region (point-min) (point-max))
	      (insert
	       (save-match-data
		 (condition-case err
		     (let ((object (eval (read sexp))))
		       (cond
			;; result is a string
			((stringp object) object)
			;; a list
			((and (listp object)
			      (not (eq object nil)))
			 (let ((string (pp-to-string object)))
			   (substring string 0 (1- (length string)))))
			;; a number
			((numberp object)
			 (number-to-string object))
			;; nil
			((eq object nil) "")
			;; otherwise
			(t (pp-to-string object))))
		   ;; error handler
		   (error
		    (format "Lisp error in %s: %s" (buffer-file-name) err)))))
	      (goto-char (point-min))
	      (widen))))))))

(provide 'o-blog-utils)

;; o-blog-utils.el ends here
