// function ob_load_css(url) {
//     /*
//      * Load CSS file given by URL
//      */
//     if (typeof(url) !== 'undefined') {
// 	var l = document.createElement('link'); l.rel = 'stylesheet';
// 	l.href = url;
// 	l.id='ob-css';
// 	var h = document.getElementsByTagName('head')[0];
// 	h.appendChild(l, h);
//     }
// };


// function run_on_predicate(predicate, func, args, timeout, maxwait) {
//     /*
//      * Call FUNC with ARGS if PREDICATE is true, otherwise wait for TIMEOUT
//      * milliseconds before retrying the PREDICATE for a maximum of MAXWAIT
//      * ms.
//      *
//      * If TIMEOUT is not provided, 10ms is used. If MAXWAIT is not provided
//      * 1000ms is used.
//      *
//      * For example to run a function when jQuery is loaded you can use:
//      *
//      *   run_on_predicate(function(){return window.jQuery}, func, args);
//      *
//      */
//     if (typeof(timeout)=='undefined') timeout=10;
//     if (typeof(maxwait)=='undefined') maxwait=1000;
//     if (predicate()) { 
// 	func(args)
//     } else {
// 	if (maxwait>=0) {
// 	    setTimeout(function(){
// 		run_on_predicate(predicate, func, args,
// 				 timeout, maxwait-timeout)
// 	    }, timeout);
// 	}
//     }
// }


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
    dropdown_link.append('<b class="caret"></b>');
    
    dropdown.addClass("dropdown-menu");
    
    /* Add divider class if menu item is empty */
    dropdown.parent().find('.dropdown-menu li').each(function() {
	if ( $(this).text() == '') $(this).addClass('divider')
    });
}


function init_tabs () {
    $('#category-tabs a:first').tab('show');
    // $('#category-tabs a').click(function (e) {
    // 	e.preventDefault();
    // 	$(this).tab('show');
    // });
    
}


// $(window).load(function(){
//     // $(".row").each(function() {
//     // 	$($(this).find(".thumbnail")).equalizer();
//     // 	$($(this).find(".src")).equalizer();
//     // })
//     $('.row').equalizer({columns:'.thumbnail'});
//     $('.row').equalizer({columns:'.src'});

// });


$(document).ready(    function() {
    // var raf = requestAnimationFrame || mozRequestAnimationFrame ||
    // 	webkitRequestAnimationFrame || msRequestAnimationFrame;
    // if (typeof(ob_css) !== 'undefined') {
    // 	var do_load_css = function(){ob_load_css(ob_css)};
    // 	if (raf) raf(do_load_css);
    // 	else
    // 	    window.addEventListener('load', do_load_css);
    // 	//ob_load_css(ob_css);
    // }

    $('.prettySocial').prettySocial();

    if (typeof ob_spinner !== 'undefined') {
	setTimeout(function() {
	    ob_spinner.stop();
	    $('#spinner').remove();
	}, 50);
    }

    setTimeout(function() {
	$("#gallery").jsFlickrGallery({
	    'modal': { 'generate' : false},
	    'thumbnailSize' :  's'
	})
    },  50);

    //function() {
//};

});



function ob_on_load() {
    if (typeof(ob_background) !== 'undefined') {
	$('.subnav .container').remove();
	$.backstretch(ob_background);
	$('.backstretch').fullwindow();
    }
    //Load articles
    // ob_load_articles(init_menu_dropdown);
//    setTimeout(init_menu_dropdown, 50);

	
    /* Compute page min height */
    $('div#page').css('min-height', $(window).innerHeight() -
		      //$('#footer').outerHeight() -
		      $('div.navbar-fixed-top.navbar').outerHeight() -
		      //parseInt($('div#page').css('padding-top')) -
		      //parseInt($('div#page').css('padding-bottom'))
		      0
		     );


    // /* Make sure each .thumbnail is the same height for each row */
    // setTimeout(function() {
    //     $(".row").each(function() {
    // 	equalHeight($(this).find(".thumbnail"));
    // 	equalHeight($(this).find(".src"));
    //     });
    // }, 300);

    
    // ob_load_tags();

    // init_tabs();

    $('[data-toggle="tooltip"]').tooltip();

    // setTimeout(function(){
    // 	$('.row').equalizer({columns:'.thumbnail'});
    // 	$('.row').equalizer({columns:'.src'});
    // }, 20);

    
}


// RunIf(
//     function(){return(window.Sid)},
//     function(){
// 	Sid.js('<lisp>(ob:path-to-root)</lisp>/style/js/spin.min.js',
// 	       function(){
// 		   ob_spinner = new Spinner({color: '#bbb'})
// 		       .spin(document.getElementById('ob-spinner'));
// 	       });
// 	Sid.js('<lisp>(ob:path-to-root)</lisp>/style/js/o-blog.min.js');
//     });



$(window).load(function(){ob_on_load();});

// if (typeof(ob_css) !== 'undefined') {
//     Sid.css(ob_css, function() {
//     	$('.row').equalizer({columns:'.thumbnail'});
//     	$('.row').equalizer({columns:'.src'});
// //	myModule.init();
//     });
// }



// // Load CSS if defined
// if (typeof(ob_css) !== 'undefined') {
//     var raf = requestAnimationFrame || mozRequestAnimationFrame ||
//         webkitRequestAnimationFrame || msRequestAnimationFrame;
//     var do_load_css = function(){ob_load_css(ob_css)};
//     if (raf) raf(do_load_css);
//     else
// 	window.addEventListener('load', do_load_css);
//     do_load_css();
// }

// ob_on_load();

function init_page() {
    ob_load_articles(init_menu_dropdown);
    ob_load_tags();
    init_tabs();
}

var raf = requestAnimationFrame || mozRequestAnimationFrame ||
    webkitRequestAnimationFrame || msRequestAnimationFrame;

if (raf) {
    raf(function(){
	init_page();
    });
} else {
    window.addEventListener('load', init_page);
}
window.onpopstate = function() {
    init_menu_dropdown();
    init_tabs();
}



var ob_js_loaded=true;
