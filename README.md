# o-blog 快速入门

- 如何使用 [o-blog](https://github.com/renard/o-blog).
- [英文演示站点](http://renard.github.com/o-blog).
- [中文演示站点](http://exaos.github.com/o-blog).


## 快速入门指南
开始之前，你需要有：

- [emacs](http:www.gnu.org/s/emacs) (假设你已经有了)
- [org-mode](http://orgmode.org/) (同上)

如果没有这些程序，你应该先安装好它们。

### 设置

#### 使用 `el-get`

如果你使用 [el-get](https://github.com/dimitri/el-get), 安装很容易。你只需要将
`o-blog` 添加到 `el-get-sources` 中，然后执行 `M-x el-get-install o-blog`.

如果你的 `el-get` 版本为 4 或以上，你只需要执行: `M-x el-get-install o-blog`.

#### 手动设置
首先从 [Github](http://github.com) 克隆这个 *git* 源码仓库。

```cd ~/.emacs.d
git clone https://github.com/renard/o-blog.git```

将后面的代码添加到你的 `~/.emacs.d/init.el` 文件中：

```(add-to-list 'load-path "~/.emacs.d/o-blog")
(require 'o-blog)```

### 发布

打开文件 `~/.emacs.d/o-blog/example/sample.org` 然后执行 `M-x org-publish-blog`.
生成的站点将会自动发布在 `~/.emacs.d/o-blog/out` 目录下。

然后，详情参见 `example/out/blog/index.html` 及 `example/out/todo.html`.

