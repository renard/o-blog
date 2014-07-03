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

function loadTags(element, url, root) {
    /*
     * Lookup for file at ROOT/URL and parse JSON to put into ELEMENT as a
     * list items.
     *
     * Json structure is like:
     *    { "tags" : [
     *       { "size" : "220.00", "path" : "tags/admin.html", "tag" : "Admin" },
     *     ] }
     */
    $(element).append('<ul></ul>');
    var ul = element + ' ul';
    var items = [];
    $.getJSON(root + '/' + url, function(data) {
	$.each(data.tags, function(i, data) {
	    var div_data =
		'<li style="font-size: ' + data.size + '%;"><a href="' +
		root + '/' + data.path + '">' + data.tag + '</a></li>';
	    items.push(div_data);
	});
	$(ul).append(items.join(' '));
    });

}

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


	/* find active tab */
	var this_page_path = path_to_root + '/' + ob_this_page;
	$('.navbar .navbar-collapse > ul li a').each(function(){
	    if($(this).attr('href') == this_page_path) {
		$(this).parent().addClass('active');
		$(this).parent().parent().parent().addClass('active');

	    }
	});
	
	/* Compute page min height */
	$('div#page').css('min-height', $(window).innerHeight() -
			  $('#footer').outerHeight() -
			  $('div.navbar-fixed-top.navbar').outerHeight() -
			  parseInt($('div#page').css('padding-top')) -
			  parseInt($('div#page').css('padding-bottom')));


	/* Make sure each .thumbnail is the same height for each row */
	$(".row").each(function() {equalHeight($(this).find(".thumbnail"))}); 
	$(".row").each(function() {equalHeight($(this).find(".src"))}); 

	loadTags('nav.tags', 'tags.js', path_to_root);
	
    });

