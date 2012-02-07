// Enable modal feature on document load.
$(document).ready(function() {
    $('.o-blog-source').click(function(e) {
	$(this).find('.content').modal({
	    zIndex:10000000,
	    autoResize: true,
	    minWidth: 950,
	    overlayClose: true,
	    escClose: true,
	    appendTo: '.article-content'
	})
    })
})
