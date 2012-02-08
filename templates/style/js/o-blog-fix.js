/*
 * Fix some org-mode html export
 */

$(document).ready(
    function() {

	/* create the top menu bar */
	var menu=$('.navbar-fixed-top .nav-collapse ul:first-child')
	menu.addClass("nav");

	/* find sub menu items */
	menu.find('li ul').parent().addClass("dropdown");

	/* and add dropdown features */
	menu.find('li.dropdown > a').addClass('dropdown-toggle')
	    .attr("data-toggle", "dropdown")
	    .append(' <b class="caret"></b>')
	    .parent().find('ul').addClass("dropdown-menu");

	/* Add divider class if menu item is empty */
	menu.find('.dropdown-menu li').each(function() {
	    if ( $(this).text() == '\n') $(this).addClass('divider')
	});

	$('div#page').css('min-height', $(window).innerHeight() -
			  $('.footer').outerHeight() -
			  $('div.navbar-fixed-top.navbar').outerHeight() -
			  parseInt($('div#page').css('padding-top')) -
			  parseInt($('div#page').css('padding-bottom')));


    }
)
