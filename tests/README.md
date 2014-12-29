<!-- -*- markdown -*- -->

# Test `o-blog`

These scripts allows you to test `o-blog` from both `markdown` and
`org-mode` sources.

## Requirements

The test configuration assumes that you have installed `o-blog` using
`el-get`. Thus:

- `htmlize` is installed in `~/.emacs.d/el-get/htmlize/htmlize.el`
- `o-blog` is installed in `~/.emacs.d/el-get/o-blog/lisp`
- `markdown-mode` is installed in `~/.emacs.d/el-get/markdown-mode/markdown-mode.el`

Please change the `config` file if you have something different.

## Sources

The `markdown` source file is `~/.emacs.d/el-get/o-blog/site/src/index.txt`
(provided by `o-blog` in `site/src/index.txt`).

The `org-mode` source file is `~/.emacs.d/el-get/o-blog/example/sample.org`
(provided by `o-blog` in `example/sample.org`).

## Run the tests

Simply run:

	./tests/test-org
	./test/test-markdown

The results are located in:

- `site/out-org` for `org-mode` sources.
- `site/out2` for `markdown` sources.
