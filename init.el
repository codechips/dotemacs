(push "/usr/local/bin" exec-path)
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(set-frame-font "Menlo-12")
(delete-selection-mode t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(require 'package)
(setq package-archives (cons '("tromey" . "http://tromey.com/elpa/") package-archives))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(load "custom/el-get-init")
(load "custom/global")
(load "custom/bindings")
(load "custom/unicode")
(load "custom/ido")
(load "custom/hippie")
(load "custom/smart-tab")
(load "custom/color-theme")



