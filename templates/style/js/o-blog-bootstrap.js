$(document).ready(
    function() {

	/* create the top menu bar */
	var dropdown = $('.navbar-inner .nav-collapse > ul li ul').parent();
	dropdown.parent().addClass("nav");

	/* find sub menu items */
	dropdown.addClass("dropdown");

	/* and add dropdown features */
	dropdown.find('> a').addClass('dropdown-toggle')
	    .attr("data-toggle", "dropdown")
	    .append(' <b class="caret"></b>');

	dropdown.find('> ul').addClass("dropdown-menu");

	/* Add divider class if menu item is empty */
	dropdown.parent().find('.dropdown-menu li').each(function() {
	    if ( $(this).text() == '\n') $(this).addClass('divider')
	});

	/* Compute page min height */
	$('div#page').css('min-height', $(window).innerHeight() -
			  $('#footer').outerHeight() -
			  $('div.navbar-fixed-top.navbar').outerHeight() -
			  parseInt($('div#page').css('padding-top')) -
			  parseInt($('div#page').css('padding-bottom')));
})
