;;; o-blog-framework.el --- Base class for o-blog frameworks

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-06-05
;; Last changed: 2014-11-20 15:26:22
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'cl)
(require 'htmlize nil t)


(defun ob:function-to-markdown(function)
  "Convert FUNCTION docstring to markdown. Return a list which
`car' is the function signature and `cadr' the markdown converted
docstring."
  (let* ((doc (documentation function t))
	 (args (help-function-arglist function)))
    (loop for arg in args
	  do (setq
	      doc
	      ;; taken from help-do-arg-highlight
	      (replace-regexp-in-string
	       (concat "\\<"                   ; beginning of word
		       "\\(?:[a-z-]*-\\)?"     ; for xxx-ARG
		       "\\("
		       (regexp-quote (format "%s" arg))
		       "\\)"
		       "\\(?:es\\|s\\|th\\)?"  ; for ARGth, ARGs
		       "\\(?:-[a-z0-9-]+\\)?"  ; for ARG-xxx, ARG-n
		       "\\(?:-[{([<`\"].*?\\)?"; for ARG-{x}, (x), <x>, [x], `x'
		       "\\>")                  ; end of word
	       ;;"`\\2`"
	       (upcase (format "`%s`" arg))
	       doc t t 1)))
    (list (format "`%s`" (help-make-usage function args)) doc)))



(cl-defstruct (ob:framework-component)
  name
  backends
  documentation
  example
  start
  end
  alias)


(setq ob:framework-components
  (list
   (make-ob:framework-component
    :name 'lead :backends '(org)
    :documentation "Make a paragraph stand out by adding `.lead`."
    :example "<lead>
foo
</lead>"
    :start "<div class=\"lead\">"
    :end "</div>")

   (make-ob:framework-component
    :name 'excerpt :backends '(org)
    :start "<div class=\"lead\"><!--excerpt-->"
    :end "<!--/excerpt--></div>")

   (make-ob:framework-component
    :name 'mark :backends '(org)
    :documentation "For highlighting a run of text due to its
    relevance in another context, use the `<mark>` tag."
    :example "You can use the mark tag to <mark>highlight</mark> text."
    :start "<mark>"
    :end "</mark>")

   (make-ob:framework-component
    :name 'del :backends '(org)
    :documentation "For indicating blocks of text that have been
    deleted use the `<del>` tag."
    :example "<del>This line of text is meant to be treated as deleted text.</del>"
    :start "<del>"
    :end "</del>")

   (make-ob:framework-component
    :name 'kbd :backends '(org)
    :documentation "Use the `<kbd>` to indicate input that is
    typically entered via keyboard."
    :example "To switch directories, type <kbd>cd</kbd> followed by the name of the directory.

To edit settings, press <kbd><kbd>ctrl</kbd> + <kbd>,</kbd></kbd>"
    :start "<kbd>"
    :end "</kbd>")

   ;;; FIXME: http://stackoverflow.com/questions/19933445/using-css-not-selector-in-less-nested-rules
   (make-ob:framework-component
    :name 'right :backends '(org)
    :documentation "Easily realign text to the right."
    :example "<right>
Right aligned text.
</right>"
    :start "<p class=\"text-right\">"    
    :end "</p>")

   (make-ob:framework-component
    :name 'left :backends '(org)
    :documentation "Easily realign text to the left."
    :example "<left>
Left aligned text.
</left>"
    :start "<p class=\"text-left\">"
    :end "</p>")

   (make-ob:framework-component
    :name 'justify :backends '(org)
    :documentation "Easily justify text."
    :example "<justify>
Justified text.
</justify>"
    :start "<p class=\"text-justify\">"
    :end "</p>")

   (make-ob:framework-component
    :name 'center :backends '(org)
    :documentation "Easily center text."
    :example "<center>Centered text.</center>"
    :start "<p class=\"text-center\">"
    :end "</p>")

   (make-ob:framework-component
    :name 'nowrap :backends '(org)
    :documentation "Easily prevent text wrapping."
    :example "<nowrap>
No wrapped text.
</nowrap>"
    :start "<p class=\"text-nowrap\">"
    :end "</p>")

   (make-ob:framework-component
    :name 'jumbotron :backends '(org)
    :documentation "A lightweight, flexible component that can
    optionally extend the entire viewport to showcase key content
    on your site."
    :example "<jumbotron>
<h1>Hello, world!</h1>

This is a simple hero unit, a simple jumbotron-style component
for calling extra attention to featured content or information.

</jumbotron>"
    :start "<div class=\"jumbotron\">"
    :end "</div>")

   (make-ob:framework-component
    :name 'page-header :backends '(org)
    :documentation "A simple shell for an `h1` to appropriately
    space out and segment sections of content on a page. A
    mandatory `title` argument holds the page header's title. A
    subtitle can be provided using the `subtitle` element."
    :example "<page-header title=\"Example page header\" subtitle=\"Subtext for header\">"
    :start '(format
	     "<div class=\"page-header\"><h1>%s%s</h1></div>"
	     title
	     (if (boundp 'subtitle)
		 (format " <small>%s</small>" subtitle)
	       "")))

   (make-ob:framework-component
    :name 'caption :backends '(org)
    :documentation "Small content like headings, paragraphs, or
    buttons to be added into `thumbnail`s. An optional `title`
    argument is supported."
    :example "<caption title=\"Thumbnail label\">

Cras justo odio, dapibus ac facilisis in, egestas eget
quam. Donec id elit non mi porta gravida at eget metus. Nullam id
dolor id nibh ultricies vehicula ut id elit.

</caption>
"
    :start '(format
	     "<div class=\"caption\">%s"
	     (if (boundp 'title)
		 (format "<h3>%s</h3>" title)
	       ""))
    :end "</div>")


   (make-ob:framework-component
    :name 'thumbnail :backends '(org)
    :documentation "Easily display grids of images, videos, text,
    and more. Can be used with the `caption` widget."
    :example "<row>
<col sm=\"4\" md=\"4\">
<thumbnail>
<caption title=\"Thumbnail label\">

Cras justo odio, dapibus ac facilisis in, egestas eget
quam. Donec id elit non mi porta gravida at eget metus. Nullam id
dolor id nibh ultricies vehicula ut id elit.

</caption>
</thumbnail>
</col>

<col sm=\"4\" md=\"4\">
<thumbnail>
<caption title=\"Thumbnail label\">

Cras justo odio, dapibus ac facilisis in, egestas eget
quam. Donec id elit non mi porta gravida at eget metus. Nullam id
dolor id nibh ultricies vehicula ut id elit.

</caption>
</thumbnail>
</col>

<col sm=\"4\" md=\"4\">
<thumbnail>
<caption title=\"Thumbnail label\">

Cras justo odio, dapibus ac facilisis in, egestas eget
quam. Donec id elit non mi porta gravida at eget metus. Nullam id
dolor id nibh ultricies vehicula ut id elit.

</caption>
</thumbnail>
</col>
</row>"
    :start "<div class=\"thumbnail\">"
    :end "</div>")

   
   (make-ob:framework-component
    :name 'glyphicon :backends '(org)
    :documentation "Display an icon using
    [Glyphicon](http://glyphicons.com/). You can also have a look
    at [bootstrap Glyphicon
    documentation](http://getbootstrap.com/components/#glyphicons). Please
    note that the `glyphicons-` part must be dropped in the
    `icon` parameter."
    :example "<glyphicon icon=\"thumbs-up\">"
    :start '("<span class=\"glyphicon glyphicon-" icon "\"></span>"))
   (make-ob:framework-component    
    :name 'icon :backends '(org)
    :documentation "Display an icon using [Font
    Awesome](http://fortawesome.github.io/Font-Awesome/icons/). Please
    note that the `icons-` part of icon name must be dropped in the
    `icon` parameter but not for other options."
    :example "
- <icon icon=\"thumbs-up\">
- <icon icon=\"thumbs-up fa-fw\">: Fixed width.
- <icon icon=\"thumbs-up fa-2x\">: 2x Magnified icon.
- <icon icon=\"thumbs-up fa-border\">: Bordered icon.
- <icon icon=\"thumbs-up fa-spin\">: Spinning icon.
- <icon icon=\"thumbs-up fa-rotate-90\">: 90˚ rotated icon.
- <icon icon=\"thumbs-up fa-rotate-180\">: 180˚ rotated icon.
- <icon icon=\"thumbs-up fa-rotate-270\">: 270˚ rotated icon.
- <icon icon=\"thumbs-up fa-flip-horizontal\">: Horizontal flipped icon.
- <icon icon=\"thumbs-up fa-flip-vertical\">: Vertical flipped icon.
"
    :start '("<i class=\"fa fa-" icon "\"></i>"))

   (make-ob:framework-component
    :name 'row :backends '(org)
    :documentation "Create a 12-columns responsive grid. See
    [Bootstrap grid system](http://getbootstrap.com/css/#grid)
    for further information."
    :example "<row>
<col xs=\"6\">`XS6`</col><col xs=\"3\">`XS3`</col>
</row>
<row>
<col xs=\"6\">`XS6`</col><col xs=\"3\">`XS3`</col><col xs=\"3\">`XS3`</col>
</row>
<row grid=\"y\">
<col o-xs=\"6\" xs=\"3\">`O-XS6` `XS3`</col><col xs=\"3\">`XS3`</col>
</row>
"
    :start (format "<div class=\"row%s%s\">"
		   (if (boundp 'equal) " equal" "")
		   (if (boundp 'grid) " show-grid" ""))
    :end "</div>")

   (make-ob:framework-component
    :name 'col :backends '(org)
    :documentation "Column of a `row` widget. Use `xs-X`, `sm-X`,
    `md-X`, `lg-X` to create a length `X` column. Use a `o-`
    prefixed parameter to inset a column offset."
    :example "<row>
<col xs=\"6\">`XS6`</col><col xs=\"3\">`XS3`</col>
</row>
<row>
<col xs=\"6\">`XS6`</col><col xs=\"3\">`XS3`</col><col xs=\"3\">`XS3`</col>
</row>
<row grid=\"y\">
<col o-xs=\"6\" xs=\"3\">`O-XS6` `XS3`</col><col xs=\"3\">`XS3`</col>
</row>
"    
    :start '("<div class=\""
	     (loop for i in '(xs sm md lg o-xs o-sm o-md o-lg)
		   when (boundp i)
		   collect (format
			    "col-%s-%s%s"
			    (car (last (split-string (symbol-name i) "-")))
			    (if (string-match "o-" (symbol-name i))
				"offset-" "")
			    (eval i))
		   into out
		   finally return (mapconcat #'identity out " "))
	     "\">")
    :end "</div>")

   (make-ob:framework-component
    :name 'panel :backends '(org)
    :documentation "Simple `panel` class. Apply some basic border
    and padding to contain some content. An alternative display
    can be specified using the `alt` optional argument (valid
    values are: `success`, `info`, `warning` and `danger`). A
    `panel` widget should contain a `panel-body` for better
    results."
    :example "<panel alt=\"primary\">
<panel-body>

Basic panel example

</panel-body>
</panel>"
    :start '("<div class=\"panel"
	     (when (boundp 'alt) (format " panel-%s" alt))
	     "\">")
    :end "</div>")

   (make-ob:framework-component
    :name 'panel-heading :backends '(org)
    :documentation "Easily add a heading container to your
    `panel` with `panel-heading`. You may also include a `title`
    optional argument to add a pre-styled heading title."
    :example "<panel alt=\"success\">
<panel-heading title=\"Panel title\">

I am the panel heading.
</panel-heading>

<panel-body>

I am the panel body

</panel-body>
</panel>"
    :start '("<div class=\"panel-heading\">"
	     (when (boundp 'title)
	       (format "<h3 class=\"panel-title\">%s</h3>" title)))
    :end "</div>")

   (make-ob:framework-component
    :name 'panel-body :backends '(org)
    :documentation "Body of a `panel` widget."
    :example "<panel alt=\"info\">
<panel-body>

Basic panel example

</panel-body>
</panel>"
    :start '("<div class=\"panel-body\">")
    :end "</div>")
   
   (make-ob:framework-component
    :name 'panel-footer :backends '(org)
    :documentation "Add a footer to a `panel` widget. Note that
    panel footers do not inherit colors and borders when using
    contextual variations as they are not meant to be in the
    foreground."
    :example  "<panel alt=\"warning\">
<panel-heading title=\"Panel title\">

I am the panel heading.
</panel-heading>

<panel-body>

I am the panel body

</panel-body>
<panel-footer>

I am the panel footer.
</panel-footer>
</panel>"
    :start '("<div class=\"panel-footer\">")
    :end "</div>")
   
   (make-ob:framework-component
    :name 'label :backends '(org)
    :documentation "Add a label. Variation can be specified using
    the `mod` optional argument."
    :example "
- <label>Label</label>
- <label mod=\"primary\">Primary</label>
- <label mod=\"success\">Success</label>
- <label mod=\"info\">Info</span></label>
- <label mod=\"warning\">Warning</label>
- <label mod=\"danger\">Danger</label>
"
    :start '(format "<span class=\"label label-%s\">"
		    (if (boundp 'mod) mod "default"))
    :end "</span>")
   
   (make-ob:framework-component
    :name 'badge :backends '(org)
    :documentation "Easily highlight new or unread items by
    adding a `badge` class to links, Bootstrap navs, and more."
    :example "Inbox <badge>42</badge>"
    :start "<span class=\"badge\">"
    :end "</span>")
   (make-ob:framework-component
    :name 'alert :backends '(org)
    :start '(format "<div class=\"alert alert-%s\">"
		    (if (boundp 'mod) mod "warning"))
    :end "</div>")
   (make-ob:framework-component
    :name 'well :backends '(org)
    :documentation "Use the well as a simple effect on an element
    to give it an inset effect. Use the `mod` parameter to
    configure the well look (valid values: `lg` and `sm`)."
    :example "<well>Look, I'm in a well!</well>

<well mod=\"sm\">Look, I'm in a well!</well>
"
    :start '(format "<div class=\"well well-%s\">"
		    (if (boundp 'mod) mod "lg"))
    :end "</div>")

   (make-ob:framework-component
    :name 'table :backends '(org)
    :documentation "Create a bootstrap styled table. I also
    supports `striped` `bordered` `hover` `condensed` style by
    the use of optional parameters."
    :example "<table bordered=\"y\" striped=\"y\" >
<tr><th>`mod`</th><th>description</th></tr>
<tr><td>`.active`</td><td>Applies the hover color to a particular row or cell</td></tr>
<tr><td>`.success`</td><td>Indicates a successful or positive action</td></tr>
<tr><td>`.info`</td><td>Indicates a neutral informative change or action</td></tr>
<tr><td>`.warning`</td><td>Indicates a warning that might need attention</td></tr>
<tr><td>`.danger`</td><td>Indicates a dangerous or potentially negative action</td></tr>
</table>"
    :start '(format "<table class=\"table %s\">"
		    (loop for v in '(striped bordered hover condensed)
			  with out = '()
			  when (boundp v)
			  collect (format "table-%s" v) into out
			  finally return (mapconcat 'identity out " ")))
			  
    :end "</table>")

   (make-ob:framework-component
    :name 'tr :backends '(org)
    :documentation "Row element of a `table`. An optional `mod` argument can specify the row background color class."
    :example "<table condensed=\"y\">
<tr><th>`mod`</th><th>description</th></tr>
<tr mod=\"active\"><td>`.active`</td><td>Applies the hover color to a particular row or cell</td></tr>
<tr><td mod=\"success\">`.success`</td><td>Indicates a successful or positive action</td></tr>
<tr mod=\"info\"><td>`.info`</td><td>Indicates a neutral informative change or action</td></tr>
<tr mod=\"warning\"><td>`.warning`</td><td>Indicates a warning that might need attention</td></tr>
<tr mod=\"danger\"><td>`.danger`</td><td>Indicates a dangerous or potentially negative action</td></tr>
</table>"
    :start '(format "<tr%s>" (if (boundp 'mod) (format " class=\"%s\"" mod) ""))
    :end "</tr>")

   (make-ob:framework-component
    :documentation "Table cell element of a `table` `tr`. An optional `mod` argument can specify the row background color class."
    :example "<table hover=\"y\">
<tr><th>`mod`</th><th>description</th></tr>
<tr mod=\"active\"><td>`.active`</td><td>Applies the hover color to a particular row or cell</td></tr>
<tr><td mod=\"success\">`.success`</td><td>Indicates a successful or positive action</td></tr>
<tr mod=\"info\"><td>`.info`</td><td>Indicates a neutral informative change or action</td></tr>
<tr mod=\"warning\"><td>`.warning`</td><td>Indicates a warning that might need attention</td></tr>
<tr mod=\"danger\"><td>`.danger`</td><td>Indicates a dangerous or potentially negative action</td></tr>
</table>"
    :name 'td :backends '(org)
    :start '(format "<td%s>" (if (boundp 'mod) (format " class=\"%s\"" mod) ""))
    :end "</td>")

   (make-ob:framework-component
    :name 'source :backends '(org)
    :documentation "Insert a syntax highlighted source code using
    Emacs `htmlize` library. A specific Emacs mode can be
    specified in the `mode` parameter (by default `fundamental`
    is used). If `src-file` is defined, its content will be read
    from file instead of being inlined."
    :example "<source mode=\"sh\">
#!/bin/sh

#Clone o-blog repository

rm -rf ~/dev/.emacs.d
mkdir -p ~/dev/.emacs.d
cd ~/dev/.emacs.d
git clone \"git://github.com/renard/o-blog.git\"
</source>"
    :start '(let* ((mode (or (and (boundp 'mode) mode) "fundamental"))
		   (cur-point (point))
		   (end-point (unless (boundp 'src-file)
				(save-match-data
				  (search-forward (format "</%s>" tag)))))
		   (content (when end-point
			      (buffer-substring-no-properties (1+ cur-point)
			       (- end-point (length (format "</%s>" tag))))))
		   (func (setq func (intern (format "%s-mode" mode))))
		   html)

	      (with-temp-buffer
		(if (boundp 'src-file)
		    (progn
		      (insert-file-contents src-file)
		      (unless func
			(setq func (assoc-default src-file auto-mode-alist
						  'string-match))))
		  (insert content))
		(if (functionp func)
		    (funcall func)
		  (warn (concat "Mode %s not found for %s. "
				"Consider installing it. "
				"No syntax highlight would be bone this time.")
			mode (if (boundp 'src-file) src-file "inline")))
		;; Unfortunately rainbow-delimiter-mode does not work fine.
		;; See https://github.com/jlr/rainbow-delimiters/issues/5
		(font-lock-fontify-buffer)
		(if (functionp 'htmlize-region-for-paste)
		    (setf html (htmlize-region-for-paste (point-min) (point-max)))
		  (warn "htmlize not installed try to export simple buffer content")
		  (setf html (format "<pre>%s</pre>"
				     (buffer-substring-no-properties
				      (point-min) (point-max))))))

	      (when end-point
		(delete-region cur-point (- end-point (length (format "</%s>" tag)))))
	      (format "<div class=\"src %s\">%s</div>" (or mode "") html)))
   (make-ob:framework-component :name 'src :backends '(org) :alias 'source)

   (make-ob:framework-component
    :name 'columns :backends '(org)
    :start '(format "<div style=\"%s%s\">"
		    (if (boundp 'width)
			(format "column-width:%s;-webkit-column-width:%s;-moz-column-width:%s" width width width) "")
		    (if (boundp 'count) (format " column-count:%s;-webkit-column-count:%s;-moz-column-count:%s;" count count count) ""))
    :end "</div>")


   (make-ob:framework-component
    :name 'copy :backends '(org)
    :documentation "Simply copy file as specified by the `src`
    parameter to the page output directory. `src` is relative to
    the page location. This component does not display anything
    on the page."
    :example "<copy src=\"path/to/file\">"
    :start '(progn
	      (when (and object
			 (boundp 'src))
		(let ((files-to-copy (ob:get 'files-to-copy object)))
		  (add-to-list 'files-to-copy src)
		  (%ob:set object 'files-to-copy files-to-copy)))
	      ""))

   (make-ob:framework-component
    :name 'function-documentation :backends '(org)
    :start '(progn
	      (when (boundp 'name)
		(let* ((name (intern name))
		       (fn-markdown (ob:function-to-markdown name)))
		  (with-temp-buffer
		    (insert
		     (format "
<panel alt=\"default\">
<panel-heading title=\"%s\">
</panel-heading>
<panel-body>

%s

</panel-body>
</panel>
"
			     (nth 0 fn-markdown)
			     (nth 1 fn-markdown)))
		    (ob:framework-expand
		     "<\\([a-z][a-z0-9-]*\\)\\([^>]+\\)?>"
		     "</%s>" "" "" "#")
		    (buffer-substring-no-properties
		     (point-min) (point-max)))))))


   
   (make-ob:framework-component
    :name 'framework-documentation :backends '(org)
    :start '(progn
	      (when (boundp 'name)
		(let* ((name (intern name))
		       (fw (loop for c in ob:framework-components
				 when (eq name (ob:get 'name c))
				 return c)))

		  (with-temp-buffer
		    (insert
		     (format "
<div class=\"panel\">
<panel-heading title=\"%s\">
</panel-heading>
<panel-body>

%s

</panel-body>
<div class=\"panel-footer\">
<thumbnail>
%s
</thumbnail>

<source mode=\"markdown\">
%s
</source>
</div>
</div>
"
					   name (or (ob:get 'documentation fw) "")
					   (or (ob:get 'example fw) "")
					   (or (ob:get 'example fw) "")))
				  (ob:framework-expand
				   "<\\([a-z][a-z0-9-]*\\)\\([^>]+\\)?>"
				   "</%s>" "" "" "#")
				  (buffer-substring-no-properties
				   (point-min) (point-max)))))))


   
   ))

(defun ob:framework-expand (re-start re-end &optional prefix suffix comment object)
  "Expand framework widgets using RE-START and RE-END to delimit notations,
convert widget to their HTML notation. COMMENT is an escape
string to prevent widget expansion.

RE-START is a 2-group regexp. First group is the widget name,
second one is a list of parameters.

RE-END is passed to `format' with widget name as parameter.

OBJECT is the current post object.

"
  (let ((prefix (or prefix "@@html:"))
	(suffix (or suffix "@@"))
	(items (loop for c in ob:framework-components
		     collect (cons (ob:get 'name c) c)))
	(comment (concat "\\s-" (or comment ","))))
    
    (save-match-data
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  ;; Lookup for a XML-like tag.
	  ;; Make sure tag does not start with ":" since "<:nil" is a valid
	  ;; org-mode option
	  (while (re-search-forward re-start nil t)
	    (let* (;; Memorize match as XML string and match positions
		   (xml
		    (format "<%s%s>"
			    (match-string-no-properties 1)
			    (or (match-string-no-properties 2) "")))
		   (beg (+ (match-beginning 0)
			   (if (string= "," (match-string 1)) 1 0)))
		   (end (match-end 0))
		   ;; Make sure XML fragment is valid. If so, xml-parsed
		   ;; should be not nil and look like:
		   ;;
		   ;; (tag ((attr1 . "value1") (attrN . "valueN")))
		   (xml-parsed
		    (with-temp-buffer
		      (insert xml)
		      (unless (search-backward "/>" nil t 1)
			(delete-backward-char 1)
			(insert "/>"))
		      (libxml-parse-xml-region (point-min) (point-max))))
		   (tag (car xml-parsed))
		   (attr (cadr xml-parsed))
		   (replacement (cdr (assoc tag items)))
		   ;; , is used to comment out tagging
		   (commented
		    (save-match-data
		      (save-excursion
			(beginning-of-line)
			(when (re-search-forward comment (point-at-eol) t)
			  (delete-backward-char 1) t)))))

	      (when (ob:get 'alias replacement)
		(setq replacement
		      (cdr (assoc (ob:get 'alias replacement) items))))

		(when (and replacement (not commented))
		;; Convert the attribute list to variables
		(cl-progv
		    (mapcar #'car attr) (mapcar #'cdr attr)
		  ;; Replace first tag
		  (narrow-to-region beg end)
		  (delete-region (point-min) (point-max))
		  (widen)
		  (insert
		   prefix
		   (ob:string-template (ob:get 'start replacement))
		   suffix)

		  ;; If tag must be closed, look up for closing tag
		  (when (ob:get 'end replacement)
		    (save-match-data
		      (save-excursion
			(unless (re-search-forward (format re-end tag) nil t)
			  (error "Unclosed %s tag (%s)" tag (format re-end tag)))
			(narrow-to-region (match-beginning 0) (match-end 0))
			(delete-region (point-min) (point-max))
			(insert
			 prefix
			 (ob:string-template (ob:get 'end replacement))
			 suffix)
			(widen)))))))))))))




(provide 'o-blog-framework)

;; o-blog-framework.el ends here
