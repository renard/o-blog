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


	gitclone: {
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
		    'node_modules/jquery/dist/jquery.min.js',
		    'node_modules/bootstrap/dist/js/bootstrap.min.js',
		    'contrib/equalizer/js/jquery.equalizer.js',
		    'contrib/prettySocial/jquery.prettySocial.js',
		    'contrib/js-flickr-gallery/js/js-flickr-gallery.js',
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

	    'css-images': {
		cwd: 'less',
		src: 'img/*',
		dest: 'style',
		expand: true
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
	    'ps' : {
		src : 'contrib/prettySocial/jquery.prettySocial.js',
		dest: 'contrib/prettySocial/jquery.prettySocial.min.js'
	    }

	},
	
    });

    grunt.loadNpmTasks('grunt-curl');
    grunt.loadNpmTasks('grunt-zip');
    grunt.loadNpmTasks('grunt-git');
    grunt.loadNpmTasks('grunt-contrib-copy');
    grunt.loadNpmTasks('grunt-contrib-less');
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-contrib-uglify');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-contrib-cssmin');
    // grunt.registerTask('init', [
    // 	// 'curl-dir',
    // 	'unzip',
    // ]);
    grunt.registerTask('init', [
	'gitclone',
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
