$(document).ready(
    function() {
	// Build nav bar
	var navbarUl = $('.navbar .navbar-collapse > ul');
	navbarUl.addClass('nav').addClass('navbar-nav');

	/* create the top menu bar */
	var dropdown = navbarUl.find('li ul')
	dropdown.parent().addClass('dropdown')
	/* find sub menu items */
	//dropdown.parent().findaddClass("dropdown-menu");

	/* and add dropdown features */
	dropdown.parent().find('> a').addClass('dropdown-toggle')
	    .attr("data-toggle", "dropdown")
	    .append(' <b class="caret"></b>');

	dropdown.addClass("dropdown-menu");

	/* Add divider class if menu item is empty */
	dropdown.parent().find('.dropdown-menu li').each(function() {
	    if ( $(this).text() == '') $(this).addClass('divider')
	});

	/* Compute page min height */
	$('div#page').css('min-height', $(window).innerHeight() -
			  $('#footer').outerHeight() -
			  $('div.navbar-fixed-top.navbar').outerHeight() -
			  parseInt($('div#page').css('padding-top')) -
			  parseInt($('div#page').css('padding-bottom')));


    }
);
