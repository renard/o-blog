;;; o-blog-i18n.el --- Internationalization for o-blog.

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-04-10
;; Last changed: 2012-04-10 16:28:42
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(defvar o-blog-i18n
  '(("en"
     :posted-on "Posted on"
     :post-timestamp "%A %B, %d %Y at %H:%M:%S"
     :related-tags "related tags"
     :home "Home"
     :tags "Tags"
     :archives "Archives"
     :rss "RSS"
     :about "About"
     :links "Links"
     :powered-by "Powered by"
     :debug-blog "Dump information for blog"
     :debug-post "Dump information for post"
     :debug-tag "Dump information for tag"
     :debug-property "Property"
     :debug-value "Value"
     :debug-blog-header "Blog"
     :debug-posts-header "Posts"
     :debug-static-pages-header "Static pages"
     :debug-snippet-header "Snippets"
     :debug-tags-header "Tags"))

  "Translation ALIST used by `ob:gettext'. Each item consists of
language as `car' and translation items as `cdr'.

The translation items are used in following templates:

  - :posted-on blog_post: blog_post.html
  - :post-timestamp: blog_post.html
  - :home: nav_links.html, nav_breadcrumb.html
  - :tags: nav_links.html, page_footer.html
  - :archives: nav_links.html, page_footer.html
  - :rss: nav_links.html
  - :about: page_footer.html
  - :links: page_footer.html
  - :powered-by: page_footer.html
  - :debug-blog: debug_blog.html
  - :debug-post: debug_post.html
  - :debug-tag: debug_tag.html
  - :debug-blog-header: debug.html
  - :debug-posts-header: debug.html
  - :debug-static-pages-header: debug.html
  - :debug-snippet-header: debug.html
  - :debug-tags-header: debug.html
  - :debug-property: debug_blog.html, debug_post.html, debug_tag.html
  - :debug-value: debug_blog.html, debug_post.html, debug_tag.html")




(provide 'o-blog-i18n)
