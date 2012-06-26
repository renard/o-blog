;;; oblog-asciidoc.el --- 

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-06-01
;; Last changed: 2012-06-26 18:06:36
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'o-blog)

(defun ob-asciidoc-parse-items (dir)
  ""
  (loop for f in (ob-find-project-files dir t "\\.txt$") 
	collect
	(with-temp-buffer
	  (insert-file-contents f)
	  (let* ((content (buffer-string))
		 (category (or (car (ob-asciidoc-get-header "category")) "blog"))
		 (tags (loop for tag in (ob-asciidoc-get-header "tags")
			     collect (make-ob:tags :name tag))))
	    (make-ob:post :content content
			  :tags tags
			  :category category
			  :source-file f)))))


(defun ob-asciidoc-export-process (post)
  ""
  (let ((buffer-name
	 (get-buffer-create
	  (format "*asciidoc-output %s*"
		  (ob:post-source-file post)))))

    (with-temp-buffer
      (insert (ob:post-content post))
      (when (>
	     (call-process-region
	      (point-min) (point-max)
	      "asciidoc" nil buffer-name
	      nil "-v" "-s" "-o" "-" "-")
	     0)
	(switch-to-buffer buffer-name)
	(error "Asciidoc export error")))

    (with-current-buffer
	buffer-name
      (goto-char (point-min))
      ;; verbose mode is useful to figure out what's wrong
      (when
	  (search-forward "<div " nil t)
	(delete-region (point-min) (point-at-bol)))
      (setf (ob:post-content-html post) (buffer-string))
      (write-file (concat (ob:post-source-file post) ".html") nil)
      (kill-buffer buffer-name))))


(defun ob-asciidoc-export-to-html (items)
  ""
  (loop for i in items
	collect (ob-asciidoc-export-process i) into ret
	finally return (notany 'null ret)))


(defun ob-asciidoc-get-header (header)
  "Search all HEADER attribute entries in current document.

Return a list of matched attributes."
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
	(loop while
	      (search-forward-regexp
	       ;; Regexp based upon `adoc-re-attribute-entry' found in adoc-mode.el
	       (format "^\\(:%s[^.\n]*?\\(?:\\..*?\\)?:[ \t]*\\)\\(.*?\\)$" header)
	       nil t)
	      collect (match-string-no-properties 2))))))

(provide 'oblog-asciidoc)

;; oblog-asciidoc.el ends here
