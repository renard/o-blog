// https://www.erianna.com/using-grunt-to-minify-and-compress-assets


module.exports = function(grunt) {
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),

	// 'curl-dir': {
	//     'contrib': [
	// 	'https://github.com/twbs/bootstrap/releases/download/v3.2.0/bootstrap-3.2.0-dist.zip'
	//     ]
	// },

	// unzip: {
	//     contrib: [
	// 	'contrib/bootstrap-3.2.0-dist.zip'
	//     ]
	// },
	'curl-dir': {
	    contrib: [
		'https://github.com/gidole/sans/raw/master/gidole.zip',
		'http://sidjs.googlecode.com/files/sidjs-0.1.tar.bz2',
		
	    ]
	},
	unzip: {
	    contrib: [
		'contrib/gidole.zip',
	    ]
	},
	exec: {
	    sidjs: {
		command: 'tar -C contrib -xjf contrib/sidjs-0.1.tar.bz2',
	    }
	},

	gitclone: {
	    'backstretch': {
		options: {
		    repository: 'git://github.com/srobbin/jquery-backstretch.git',
		    directory: 'contrib/backstretch'
		}
	    },
	    'blurr': {
		options: {
		    repository: 'https://github.com/tomhallam/Blurr.git',
		    directory: 'contrib/blurr',
		}
	    },
	    'equalizer': {
		options: {
		    repository: 'https://github.com/CSS-Tricks/Equalizer.git',
		    directory: 'contrib/equalizer'
		}
	    },
	    'js-flickr-gallery': {
		options: {
		    repository: 'https://github.com/petejank/js-flickr-gallery.git',
		    directory: 'contrib/js-flickr-gallery'
		}
	    },
	    'pretty-social': {
		options: {
		    repository: 'https://github.com/sonnyt/prettySocial.git',
		    directory: 'contrib/prettySocial'
		}
	    },
	    'spin': {
		options: {
		    repository: 'https://github.com/fgnass/spin.js.git',
		    directory: 'contrib/spin'
		}
	    },
	    // 'gidole-sans': {
	    // 	options: {
	    // 	    repository: 'https://github.com/gidole/sans.git',
	    // 	    directory: 'contrib/gidole-sans'
	    // 	}
	    // }

	},
	
	concat: {
            css: {
		src: [
                    // 'node_modules/bootstrap/dist/css/bootstrap.css',
		    '_tmp/o-blog.css',
		    'node_modules/font-awesome/css/font-awesome.css',
		    // 'contrib/js-flickr-gallery/css/js-flickr-gallery.css',
                ],
		dest: '_tmp/combined.css'
            },
            js : {
		src : [
		    // 'contrib/sidjs-0.1/sidjs-0.1.js',
		    'node_modules/jquery/dist/jquery.min.js',
		    'node_modules/bootstrap/dist/js/bootstrap.min.js',
		    'contrib/backstretch/jquery.backstretch.min.js',
		    'contrib/equalizer/js/jquery.equalizer.js',
		    'contrib/blurr/src/jquery.blurr.js',
		    'contrib/prettySocial/jquery.prettySocial.js',
		    'contrib/js-flickr-gallery/js/js-flickr-gallery.js',
		    'js/jquery.overlay.js',
		    'js/o-blog-bootstrap.js',
		],
		dest : '_tmp/combined.js'
            },
	},

	copy: {
	    'bootstrap': {
		cwd: 'node_modules/bootstrap/fonts',
		src: '*',
		dest: 'style/fonts/',
		expand: true
	    },

	    'fonts-awesome': {
		cwd: 'node_modules/font-awesome/fonts',
		src: '*',
		dest: 'style/fonts/',
		expand: true
	    },
	    'overlay': {
		cwd: 'js',
		src: 'overlays/*',
		dest: 'style/img/',
		expand: true
	    },

	    'css-images': {
		cwd: 'less',
		src: 'img/*',
		dest: 'style',
		expand: true
	    },
	    'favicon': {
		src: 'favicon.png',
		dest: 'style/img/',
	    },
	    'gidole-sans': {
	    	cwd: 'contrib/GidoleFont/',
	    	src: [ '*.otf', '*.ttf' ],
	    	dest: 'style/fonts/',
	    	expand: true,
	    }
	    
	},

	less: {
	    'o-blog': {
		options: {
		    paths: [ 'node_modules/bootstrap/less' ],
		    compress: true,
		    cleancss: true,
		},
		files: {
		    '_tmp/o-blog.css': 'less/o-blog.less',
		}
	    },
	},

	cssmin : {
            css:{
                src: '_tmp/combined.css',
                dest: 'style/css/o-blog.min.css'
            }
        },
	
	uglify : {
            js: {
		src: '_tmp/combined.js',
		dest : 'style/js/o-blog.min.js'
            },
	    'js-spin' : {
		src : 'contrib/spin/spin.js',
		dest: 'style/js/spin.min.js'
	    },
	    runif : {
		src: 'js/runif.js',
		dest: 'style/js/runif.min.js'
	    },
	    sidjs : {
		src: 'contrib/sidjs-0.1/sidjs-0.1.js',
		dest: 'style/js/sidjs.min.js'
	    },
	    'o-blog-init' : {
		src: 'js/o-blog-init.js',
		dest: 'style/js/o-blog-init.min.js'
	    },
	    'ps' : {
		src : 'contrib/prettySocial/jquery.prettySocial.js',
		dest: 'contrib/prettySocial/jquery.prettySocial.min.js'
	    }

	},
	csslint : {
	    strict: {
		options: {
		    import: false
		},
		src: ['style/css/*.css']
	    }

	}
	
    });

    grunt.loadNpmTasks('grunt-curl');
    grunt.loadNpmTasks('grunt-zip');
    grunt.loadNpmTasks('grunt-git');
    grunt.loadNpmTasks('grunt-untar');
    grunt.loadNpmTasks('grunt-exec');
    grunt.loadNpmTasks('grunt-contrib-copy');
    grunt.loadNpmTasks('grunt-contrib-less');
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-contrib-uglify');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-contrib-cssmin');
    grunt.loadNpmTasks('grunt-contrib-csslint');
    // grunt.registerTask('init', [
    // 	// 'curl-dir',
    // 	'unzip',
    // ]);
    grunt.registerTask('init', [
	'gitclone',
	'curl-dir',
	'unzip',
	'untar',
    ]);

    grunt.registerTask('default', [
	'less',
	'concat',
	'cssmin',
	'concat',
	'uglify',
	'copy',
    ]);
};
