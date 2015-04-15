/**
   Copyright © 2015 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

   Created: 2015-04-15
   Last changed: 2015-04-15 12:46:59

   This program is free software. It comes without any warranty, to
   the extent permitted by applicable law. You can redistribute it
   and/or modify it under the terms of the Do What The Fuck You Want
   To Public License, Version 2, as published by Sam Hocevar. See
   http://sam.zoy.org/wtfpl/COPYING for more details.

*/


(function() {

    var runif = function (test, func, args, timeout, maxwait) {
	/*
	 * Call FUNC with ARGS if TEST is true, otherwise wait for TIMEOUT
	 * milliseconds before retrying the PREDICATE for a maximum of MAXWAIT
	 * ms.
	 *
	 * If TIMEOUT is not provided, 10ms is used. If MAXWAIT is not provided
	 * 1000ms is used.
	 *
	 * For example to run a function when jQuery is loaded you can use:
	 *
	 *   run_on_predicate(function(){return window.jQuery}, func, args);
	 *
	 */
	// console.log('runif(' + test + ', ' + timeout + ', ' + maxwait + ')');
	if (typeof(timeout) == 'undefined') timeout = 10;
	if (typeof(maxwait) == 'undefined') maxwait = 1000;
	// console.log('  -->(' + test + ', ' + timeout + ', ' + maxwait + ')');
	if ( test() ) {
	    // console.log('Running func' + func);
	    return func(args);
	} else {
	    if (maxwait >= 0) {
		// console.log('Maxwait: ' + maxwait);
		setTimeout(function() {
		    // console.log('Timeout! to run: ' + (maxwait - timeout));
		    runif(test, func, args, timeout, (maxwait - timeout));
		}, timeout);
	    }
	}
    }

    window.RunIf = {
	runif: function(test, func, args, timeout, maxwait) {
	    return runif(test, func, args, timeout, maxwait);
	},
    }
})();
