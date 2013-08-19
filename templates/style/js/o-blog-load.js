(function($){
    $.getUrlVar = function(key){
	var result = new RegExp(key + "=([^&]*)", "i").exec(window.location.search);
	return result && unescape(result[1]) || "";
    };
})(jQuery);

$(document).ready(function () {
    var p = $.getUrlVar("p");
    $('#page article .article-content').load(p);
});
