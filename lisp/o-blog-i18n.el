;;; o-blog-i18n.el --- Internationalization for o-blog.

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-04-10
;; Last changed: 2013-03-28 23:15:46
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
     :redirect-header "Redirect"
     :redirect-text "Oups! you shouldn't be here. Please wait for redirection."
     :redirect-link "Click here for homepage."
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
     :debug-tags-header "Tags")
    ("fr"
     :posted-on "Posté le"
     :post-timestamp "%A %d %B %Y à %H:%M:%S"
     :related-tags "Tags associés"
     :home "Accueil"
     :tags "Tags"
     :archives "Archives"
     :rss "RSS"
     :about "À propos"
     :links "Liens"
     :redirect-header "Redirection"
     :redirect-text "Oups! Vous ne devriez pas être ici. Merci de patienter pour une redirection."
     :redirect-link "Cliquez ici pour l'accueil."
     :powered-by "Réalisé avec"
     :debug-blog "Contenu du blog"
     :debug-post "Contenu des articles"
     :debug-tag "Contenu des tags"
     :debug-property "Propriété"
     :debug-value "Valeur"
     :debug-blog-header "Blog"
     :debug-posts-header "Articles"
     :debug-static-pages-header "Pages statiques"
     :debug-snippet-header "Fragments"
     :debug-tags-header "Tags")
    ("de"
     :posted-on "Veröffentlicht am"
     :post-timestamp "%d.%m.%Y um %H:%M"
     :related-tags "Zugewiesene Schlagworte"
     :home "Start"
     :tags "Schlagworte"
     :archives "Archiv"
     :rss "RSS"
     :about "Über"
     :links "Links"
     :redirect-header "Umleitung"
     :redirect-text "O! Du solltest nicht hier sein. Bitte warte auf die Umleitung."
     :redirect-link "Hier klicken, um zum Start zu kommen."
     :powered-by "Bereitgestellt mit Hilfe von"
     :debug-blog "Beispielinformation für den Blog"
     :debug-post "Beispielinformation für einen Beitrag"
     :debug-tag "Beispielinformation für ein Schlagwort"
     :debug-property "Eigenschaft"
     :debug-value "Wert"
     :debug-blog-header "Blog"
     :debug-posts-header "Artikel"
     :debug-static-pages-header "Statische Seiten"
     :debug-snippet-header "Schnipsel"
     :debug-tags-header "Schlagworte")
    ("zh-CN"  ;; simplified Chinese, traditional Chinese may use zh-TW or zh-HK
     :posted-on "发布于"
     :post-timestamp "%Y-%m-%d %H:%M:%S"
     :related-tags "相关标签"
     :home "首页"
     :tags "标签"
     :archives "归档"
     :rss "RSS"
     :about "关于"
     :links "链接"
     :powered-by "功能取自"
     :debug-blog "导出博客信息"
     :debug-post "导出博文信息"
     :debug-tag "导出标签信息"
     :debug-property "属性"
     :debug-value "值"
     :debug-blog-header "博客"
     :debug-posts-header "博文"
     :debug-static-pages-header "固定页面"
     :debug-snippet-header "网页片断"
     :debug-tags-header "标签" ))

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

;;;###autoload
(defun ob:gettext (text &optional lang)
  "Return part of `o-blog-i18n' that matches TEXT in LANG.

If LANG is not defined, the blog \"#+LANGUAGE:\" header would be
used. If not found, English (en) is used as a fall-back."
  (let* ((lang (or lang
		   ;;(when (boundp 'BLOG) (ob:blog-language BLOG))
		   "en"))
	 (default-text-list (cdr (assoc "en" o-blog-i18n)))
	 (text-list (or
		     (cdr (assoc lang o-blog-i18n))
		     default-text-list)))
    (or (plist-get text-list text)
	(plist-get text-list default-text-list))))



(provide 'o-blog-i18n)
