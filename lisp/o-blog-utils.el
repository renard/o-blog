;;; o-blog-utils.el --- 

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-01-22
;; Last changed: 2013-02-09 12:25:47
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
(provide 'o-blog-utils)

;; o-blog-utils.el ends here
