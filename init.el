;; read in PATH from .bashrc
;;(if (not (getenv "TERM_PROGRAM"))
;; (setenv "PATH"
;;   (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
      (replace-regexp-in-string "[[:space:]\n]*$" ""
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

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

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
                      coffee-mode
                      clojure-test-mode
                      marmalade
                      org
                      paredit
                      color-theme
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load "custom/global")
(load "custom/bindings")
(load "custom/unicode")
(load "custom/ido")
(load "custom/hippie")
(load "custom/smart-tab")
