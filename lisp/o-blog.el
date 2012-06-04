;;; o-blog.el --- Publish markdown as blog

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-06-01
;; Last changed: Mon Jun  4 08:55:58 2012 (cest)
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(defconst o-blog-version "1.99" "o-blog version number")


(defstruct (ob:blog :named)
  "Blog structure

 - file: the blog source file (read-only).

 - buffer: buffer visiting the blog file (read-only).

 - publish-dir: where to publish the blog defined by the
   \"#+PUBLISH_DIR:\" header directive or out in the same
   directory as the blog source file.

 - template-dir: location of the template directory defined by
   the \"#+TEMPLATE_DIR:\" header directive or the templates
   directory of the o-blog library.

 - style-dir: path of the \"css\" files defined by the
   \"#STYLE_DIR:\" header directive or style. This directory is
   relative to \"template-dir\".

 - posts-filter: default filter for post defined by the
   \"#POSTS_FILTER:\" header directive or \"+TODO=\\\"DONE\\\".

 - static-filter: default filter for static pages defined by the
   \"#STATIC_FILTER:\" header directive or \"+PAGES={.*}\.

 - snippet-filter default filter for snippets defined by the
   \"#SNIPPET_FILTER:\" header directive or \"+SNIPPET={.*}\".

 - title: Blog title defined by the \"#+TITLE:\" header
   directive.

 - description: blog description defined by the
   \"#+DESCRIPTION:\" header directive.

 - url: Blog base URL defined by the \"#+URL:\" header.

 - language: Blog language defined by the \"#+LANGUAGE:\" header
   or \"en\".

 - default-category: default category for posts defined by the
   \"#DEFAULT_CATEGORY:\" header or \"Blog\".

 - disqus: disqus account (called a forum on Disqus) this system
   belongs to. Defined by the \"#DISQUS\" header.

 - filename-sanitizer: 1-argument function to be used to sanitize
   post filenames. Defined by \#+FILENAME_SANITIZER:\" or
   \"ob-sanitize-string\".
"
  (file nil :read-only)
  (buffer nil :read-only)
  publish-dir
  template-dir
  style-dir
  posts-filter
  static-filter
  snippet-filter
  title
  description
  url
  language
  post-build-shell
  default-category
  disqus
  filename-sanitizer)


(defstruct (ob:post :named)
  "Post structure

 - id: the post numerical id. Posts are sort by reversed
   chronological order. The most recent post get the id 0.

 - title: the post title read from the entry title.

 - timestamp: the post timestamp given by the \"CLOSED\" property
   or the current time.

 - year: numerical year computed from \"timestamp\".

 - month: numerical month computed from \"timestamp\".

 - day: numerical day computed from \"timestamp\".

 - category: category read from \"CATEGORY\" property org
   \"blog\".

 - tags: list of ob:tags.

 - template: template to use for current post read from
   \"TEMPLATE\" property or \"blog_post.html\".

 - filepath: relative path from the blog root directory to the
   post directory (directory only).

 - filename: sanitized filename generated from \"title\".

 - htmlfile: full relative path to the post html file (file and
   directory).

 - path-to-root: relative path from the post html file to the
   blog root.

 - source-file: path to post original source file.

 - content: raw content of the post (org-mode format).

 - content-html: HTML export of the post."
  id
  title
  timestamp
  year
  month
  day
  category
  tags
  template
  filepath
  filename
  htmlfile
  path-to-root
  source-file
  content
  content-html)


(defstruct (ob:tags :named)
  "Tag structure with following slots:

 - name: string defying the tag name.
 - safe: web safe tag name for URL.
 - count: how many time the tag is used.
 - size: the font size in percent."
  name safe count size)



(defun ob-find-project-files (dir &optional full match nosort ignore)
  "Find files under DIR like `directory-files'. See
`directory-files' for FULL MATCH NOSORT options.

IGNORE is a list of names to ignore (basically \".git\" \"CVS\" etc.
Note: both \".\" and \"..\" are otomaticaly ignored."
  (loop for f in (directory-files dir full nil nosort)
	for full-path = (if full f (concat (file-name-as-directory dir) f))
	for item-name = (if full (file-name-nondirectory f) f)
	unless (or
		(member item-name '("." ".."))
		(member item-name ignore))
	if (file-directory-p full-path)
	append (ob-find-project-files full-path full match nosort ignore)
	else
	when (save-match-data (string-match match item-name))
	collect f))



(provide 'o-blog)
;; o-blog.el ends here
