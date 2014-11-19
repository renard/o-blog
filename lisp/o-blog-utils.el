;;; o-blog-utils.el --- Some generic function used in o-blog.

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-01-22
;; Last changed: 2014-11-19 16:40:30
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'cl)

(defvar *ob:backends* nil
  "Plist of functions to process article with o-blog
  backends. Fill with `ob:register-backend'")


(defun* ob:register-backend (backend &key find-files parse-config
				     parse-entries convert-entry)
  "Register BACKEND as a o-blog backend"
  (let ((backend-def (plist-get *ob:backends* backend)))
    (loop for var in '(find-files parse-config parse-entries convert-entry)
	  do (let ((val (symbol-value var)))
	       (when val
		 (setq backend-def
		       (plist-put backend-def
				  (intern (format ":%s" var))
				  val)))))
    (setf *ob:backends* (plist-put *ob:backends* backend backend-def))))

(defun ob:get-backend-function (backend function)
  ""
  (plist-get (plist-get *ob:backends* backend) function))

(defmacro ob:profile (message &rest body)
  `(let ((start-at (current-time)))
     ,@body
     (message "%s: %.3fs" ,message
	      (time-to-seconds
	       (time-subtract (current-time) start-at)))))


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
	      ;; In some exporters (pandoc) " are replaced with &quot; which
	      ;; breaks lisp interpolation.
	      (with-temp-buffer
		(insert sexp)
		(goto-char (point-min))
		(while (search-forward "&quot;" nil t)
		  (replace-match "\"" nil t))
		(setq sexp (buffer-string)))
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

(defun ob:format-date (date &optional format locale)
  "Format DATE using FORMAT and LOCALE.

DATE can heither be string suitable for `parse-time-string' or a
list of interger using `current-time' format.

FORMAT is a `format-time-string' compatible definition. If not
set ISO8601 \"%Y-%m-%dT%TZ\" format would be used."
  (let* ((date (cond
		((stringp date)
		 (apply 'encode-time
			(parse-time-string date)))
		((listp date)
		 date)))
	 (format (or format "%Y-%m-%dT%TZ"))
	 (system-time-locale locale))
    (format-time-string format date)))

(defun ob:path-to-root ()
  "Return path to site root from `PATH-TO-ROOT' or `POST'
path-to-root slot."
  (cond
   ((boundp 'PATH-TO-ROOT) PATH-TO-ROOT)
   ((boundp 'POST) (ob:get 'path-to-root POST))
   (t ".")))

(defun %ob:get-type (object)
  (when (arrayp object)
    (let* ((type (aref object 0))
	   (prefix "cl-struct-")
	   (prefix-length (length prefix))
	   type-str)
      (when (symbolp type)
	(setf type-str (symbol-name type))
	;; Symbols are prefixed by "cl-struct-" which length is 10.
	(when (and
	       (> (length type-str) prefix-length)
	       (string= (substring type-str 0 prefix-length) prefix))
	  (intern (substring type-str prefix-length)))))))

(defun ob:get-slots (object)
  (loop for s in (cdr (cl-struct-slot-info (%ob:get-type object)))
	collect (car s)))

(defun ob:slot-exists-p (object slot)
  (let* ((type (%ob:get-type object)))
    (assoc slot (cl-struct-slot-info type))))
      
  
(defun %ob:set (object slot value)
  (when (arrayp object)
    (let* ((type (intern (replace-regexp-in-string
			  "^cl-struct-" "" (symbol-name (aref object 0))))))
      (when (assoc slot (cl-struct-slot-info type))
	(aset object (cl-struct-slot-offset type slot) value)))))

(defun ob:get (slot  object)
  "Try to get SLOT from OBJECT."
  (let* ((type (%ob:get-type object))
	 (func (intern (format "%s-%s" type slot))))
    (if (functionp func)
	(funcall func object))))

(defun ob:get-post-by-id (id)
  "Return post which id is ID"
  (let ((POSTS (or (when (boundp 'POSTS) POSTS)
		   (ob:get 'articles BLOG))))
    (when (>= id 0)
      (nth id POSTS))))


(defun ob:get-name (object)
  "Return OBJECT class name."
  (if (boundp 'object-name)
      (aref object object-name)
    (eieio-object-name-string object)))


;; FIXME: do no use ob-bck-end
(defun ob:insert-template (template &optional ob-bck-end)
  "Return the lisp evaluated (see `ob:eval-lisp') content of
TEMPLATE (relative from `ob:backend' `template' slot) as a
string."
  (insert
   (with-temp-buffer
     (insert-file-contents
      (format "%s/%s" (ob:get 'template-dir (or ob-bck-end BLOG)) template))
     (ob:eval-lisp)
     (buffer-string))))

(defun ob:eval-template-to-file (template file)
  "Evaluate TEMPLATE an write it to FILE."
  (with-temp-buffer
    	(ob:insert-template template)
	(ob:write-file file)))

(defun ob:get-next-post (&optional count)
  "Retrieve next post"
  (when (and (boundp 'POST)
	     (boundp 'POSTS))
    (let* ((count (or count 1))
	   (current POST)
	   (category (ob:get 'category POST))
	   (cat-posts (ob:get-posts
		       (lambda (x)
			 (equal category (ob:get 'category x)))))
	   (current-idx (loop for i below (length cat-posts)
			      until (equal (nth i cat-posts) current)
			      finally return i))
	   (wanted-id (+ current-idx count)))
      (when (and (>= wanted-id 0)
		 (< wanted-id (length cat-posts)))
	  (nth wanted-id cat-posts)))))

(defun ob:get-prev-post (&optional count)
  (ob:get-next-post (or count -1)))



(defun ob:file-name-directory-base (file)
  "Return last directory component of FILE.

Ex: \"/path/to/some/file.html\" -> \"some\"

"
  (nth 1 (reverse (split-string file "/"))))


(defun ob:lesser (a b)
  "Emulate `<' in templates."
  (< a b))

(defun ob:lesser-or-equal (a b)
  "Emulate `<=' in templates."
  (<= a b))

(defun ob:greater (a b)
  "Emulate `>' in templates."
  (> a b))

(defun ob:greater-or-equal (a b)
  "Emulate `>=' in templates."
  (>= a b))



(defun ob:string-template (template)
  "Evaluate TEMPLATE and return a string.
- strings are return as it
- symbols are evaluated
- lists are concatenated or evaluated depending on their first element."
  (cond
   ((stringp template) template)
   ((symbolp template)
    (if (boundp template)
	(if (eq nil template)
	    ""
	  (format "%s" (symbol-value template)))
      (symbol-name template)))
   ((listp template)
    (if (and (symbolp (car template))
	     (fboundp (car template)))
	(eval template)
      (mapconcat 'identity
		 (loop for i in template
		       collect (ob:string-template i))
		 "")))
   (t (format "%s" template))))


(defun ob-do-copy (src dst &optional copyf args)
  "Copy SRC into DST. If `dired-do-sync' is found it would be
preferred. Otherwise, `copy-directory' or `copy-files' would be
used.

A copy function COPYF and its arguments ARGS could be specified."
  (let* ((dirp (file-directory-p src))
	 (copyf (cond
		 (copyf copyf)
		 ((functionp 'dired-do-sync) 'dired-do-sync)
		 (dirp 'copy-directory)
		 (t 'copy-file)))
	 (args (or args
		   (when (eq 'copy-file copyf) '(t t t)))))
    (when (file-exists-p src)
      ;;(message "Copying %s -> %s using %s" src dst copyf)
      (apply copyf src dst args))))

(defun ob:publish-style (object)
  "Publish OBJECT styles such as CSS, JavaScript and Fonts."
  (ob-do-copy
   (ob:get 'style-dir object)
   (ob:get 'publish-dir object)))


(defun ob:get-html-links (html &optional wanted-tag wanted-attribute)
  "Return a list of all WANTED-ATTRIBUTE of WANTED-TAG from HTML.
If not specified return all link references from HTML
fragment (tag: 'a attribute 'href)"
  (when (listp html)
    (let ((tag (pop html))
	  (attributes (pop html))
	  (children html)
	  (wanted-tag (or wanted-tag 'a))
	  (wanted-attribute (or wanted-attribute 'href)))
      (cond
       ((eq tag wanted-tag)
	(cons (cdr (assoc wanted-attribute attributes))
	      (loop for child in children
		    nconc (ob:get-html-links child wanted-tag wanted-attribute))))
       ((listp children)
	(loop for child in children
	      nconc (ob:get-html-links child wanted-tag wanted-attribute)))
       (t nil)))))



(provide 'o-blog-utils)

;; o-blog-utils.el ends here
