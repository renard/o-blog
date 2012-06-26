;;; oblog-asciidoc.el --- 

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-06-01
;; Last changed: 2012-06-26 16:07:05
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'o-blog)

(defun ob-asciidoc-parse-items (dir)
  ""
  (loop for f in (ob-find-project-files dir t "\\.txt$") 
	collect (make-ob:post :content (with-temp-buffer
					 (insert-file-contents f)
					 (buffer-string))
			      :source-file f)))


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
	finally return ret))


(defun ob-asciidoc-get-header (header)
  "Search HEADER attribute entry in current document.
If a header is repeated several times only the first match is returned.
"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward-regexp
	     ;; Regexp based upon `adoc-re-attribute-entry' found in adoc-mode.el
	     (format "^\\(:%s[^.\n]*?\\(?:\\..*?\\)?:[ \t]*\\)\\(.*?\\)$" header)
	     nil t)
	(match-string-no-properties 2)))))


(provide 'oblog-asciidoc)

;; oblog-asciidoc.el ends here
