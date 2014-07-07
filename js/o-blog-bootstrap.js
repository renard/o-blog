function equalHeight(group) {    
    tallest = 0;    
    group.each(function() {       
        thisHeight = $(this).height();       
        if(thisHeight > tallest) {          
            tallest = thisHeight;       
        }    
    });    
    group.each(function() { $(this).height(tallest); });
} 

function ob_load_tags() {
    /*
     * Load all tag information into each '.ob-tagcloud' classes.
     *
     * Define a tagcloud as:
     *  <span class="ob-tag-cloud" data-source="tags.js" data-path-to-root="."/>
     *
     * Where:
     *  - `source` is the path to the JSON structure
     *  - `path-to-root` is the path to the site root directory.
     *
     * Json structure is like:
     *    { "tags" : [
     *       { "size" : "220.00", "path" : "tags/admin.html", "tag" : "Admin" },
     *       { ... }
     *     ] }
     */
    $.each($('.ob-tagcloud'), function(i, element) {
	    var source = $(element).data('source') || "tags.js";
	    var path_to_root = $(element).data('path-to-root') || ".";
	    $.getJSON(source, function(json) {
		var tags_list = [];
		$.each(json.tags, function(i, tag) {
		    tags_list.push('<li style="font-size: ' + tag.size
				   + '%;"><a href="' + path_to_root
				   + '/' + tag.path + '">'
				   + tag.tag + '</a></li>');
		});
		$(element).replaceWith('<ul>' + tags_list.join(' ') + '</ul>');
	    });
	});
}


function ob_load_articles(callback) {
    /*
     * Load all articles information into each '.ob-articles' classes.
     *
     * Define a article as:
     *  <span class="ob-articles" data-source="articlestags.js"
     *        data-path-to-root="." data-category="category"
     *        data-excerpt="true" data=limit="5"/>
     *
     * Where:
     *  - `source` is the path to the JSON structure
     *  - `path-to-root` is the path to the site root directory.
     *  - `category` is the article category to handle
     *  - `excerpt` if true add article excerpt, insert only titles otherwise
     *  - `limit` is the article limit count
     *
     * Json structure is like:
     *   {
     *     "articles": {
     *       "cat1": [
     *         {
     *           "excerpt": "article excerpt",
     *           "path": "cat1/2014/07/03_article1.html",
     *           "title": "Article 1 in category 1 title"
     *         }, ...
     *       ],
     *       "cat2": [
     *         {
     *           "excerpt": "article excerpt",
     *           "path": "cat2/2014/07/03_article1.html",
     *           "title": "Article 1 in category 2 title"
     *         }, ...
     *       ], ...
     *     }
     *   }
     */
    $.each($('.ob-articles'), function(i, element) {
	var source = $(element).data('source') || "articles.js";
	var path_to_root = $(element).data('path-to-root') || ".";
	var limit  = $(element).data('limit') || 10;
	var excerpt  = $(element).data('excerpt') || false;
	var category  = $(element).data('category');
	
	$.getJSON(source).done(function(json) {
	    var articles_list = [];
	    $.each(json.articles[category], function(i, art_data) {
		if (i >= limit) return false;
		if (excerpt) {
		    articles_list.push('<div><h3><a href="' + path_to_root
				       + '/' + art_data.path + '">'
				       + art_data.title + '</a></h3><p>'
				       + art_data.excerpt + '</p></div>');
		} else {
		    articles_list.push('<li><a href="' + path_to_root
				       + '/' + art_data.path + '">' +
				       art_data.title + '</a></li>');
		}
	    });
	    if (excerpt) {
		$(element).replaceWith('<div class="ob-excerpts">'
				       + articles_list.join(' ') + '</div>');
	    } else {
		$(element).replaceWith('<ul>' + articles_list.join(' ') + '</ul>');
	    }
	    if ($.isFunction(callback)) {
		callback.call();
	    }
	});
    });
}


function init_menu_dropdown() {

    /* find active tab */
    setTimeout(function() {
	$('.navbar .navbar-collapse > ul li a[href="' +
	  path_to_root + '/' + ob_this_page + '"]')
	    .parent().addClass('active')
	    .parent().parent().addClass('active');},
	       1);
    
    // Build nav bar
    var navbarUl = $('.navbar .navbar-collapse > ul');
    navbarUl.addClass('nav').addClass('navbar-nav');
    
    /* create the top menu bar */
    var dropdown = navbarUl.find('li ul');
    dropdown.parent().addClass('dropdown');
    /* find sub menu items */
    //dropdown.parent().findaddClass("dropdown-menu");

    var dropdown_link = dropdown.parent().find('> a');
    dropdown_link.addClass('dropdown-toggle')
	.attr("data-toggle", "dropdown")
	.find('b.caret').remove();
    dropdown_link.append(' <b class="caret"></b>');
    
    dropdown.addClass("dropdown-menu");
    
    /* Add divider class if menu item is empty */
    dropdown.parent().find('.dropdown-menu li').each(function() {
	if ( $(this).text() == '') $(this).addClass('divider')
    });
}



$(document).ready(
    function() {
	setTimeout(init_menu_dropdown, 1);
	//Load articles
	ob_load_articles();
	
	/* Compute page min height */
	$('div#page').css('min-height', $(window).innerHeight() -
			  $('#footer').outerHeight() -
			  $('div.navbar-fixed-top.navbar').outerHeight() -
			  parseInt($('div#page').css('padding-top')) -
			  parseInt($('div#page').css('padding-bottom')));


	/* Make sure each .thumbnail is the same height for each row */
	setTimeout(function() {
	    $(".row").each(function() {
		equalHeight($(this).find(".thumbnail"));
		equalHeight($(this).find(".src"));
	    });
	}, 300);
	    

	ob_load_tags();
    });

