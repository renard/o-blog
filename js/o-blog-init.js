/**

   Copyright © 2015 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

   Created: 2015-04-15
   Last changed: 2015-04-15 12:53:10

   This program is free software. It comes without any warranty, to the
   extent permitted by applicable law. You can redistribute it and/or modify
   it under the terms of the Do What The Fuck You Want To Public License,
   Version 2, as published by Sam Hocevar. See
   http://sam.zoy.org/wtfpl/COPYING for more details.

*/


var path_to_root = '<lisp>(ob:path-to-root)</lisp>';
var ob_this_page = '<lisp>FILE</lisp>';

// Things to load after Sid JS is loaded.
RunIf.runif(
    function(){
	return(window.Sid)
    },
    function(){
	var protocol = /^http:/.test(document.location)?'http:':'https:';
	// First load CSS, then adapt dimensionning.
        Sid.css('<lisp>(ob:path-to-root)</lisp>/style/css/o-blog.min.css',
		function(){RunIf.runif(
		    function(){return window.jQuery},
		    function() {
			$('.row').equalizer({columns:'.thumbnail'});
			$('.row').equalizer({columns:'.src'});
		    }, undefined,
		    100, 60000)},
		'head');
	// Load the spinner and run it.
	Sid.js('<lisp>(ob:path-to-root)</lisp>/style/js/spin.min.js',
	       function(){
		   ob_spinner = new Spinner({color: '#bbb'})
		       .spin(document.getElementById('ob-spinner'));
	       },
	       'head');
	// Load remaining o-blog JS.
	Sid.js('<lisp>(ob:path-to-root)</lisp>/style/js/o-blog.min.js',
	       undefined, 'head');

	var ob_linkedin = "<lisp>(ob:get 'share-linkedin BLOG)</lisp>";
	if (ob_linkedin !== '') {
	    Sid.js(protocol + '//platform.linkedin.com/in.js', undefined, 'head');
	}
	
	var ob_twitter_account = "<lisp>(ob:get 'twitter-account BLOG)</lisp>";
	if (ob_twitter_account !== '') {
	    Sid.js(protocol + '//platform.twitter.com/widgets.js', undefined, 'head');
	}

	var ob_google_plus = "<lisp>(ob:get 'google-plus BLOG)</lisp>";
	if (ob_google_plus !== '') {
	    Sid.js(protocol + '//apis.google.com/js/platform.js', undefined, 'head');
	}

	var ob_google_plus = "<lisp>(ob:get 'google-analytics BLOG)</lisp>";
	if (ob_google_plus !== '') {
	    Sid.js(protocol + '//www.google-analytics.com/analytics.js',
		   function(){
		       ga('create', ob_google_plus, 'auto');
		       ga('send', 'pageview');
		   }, 'head');
	}
    });




