;; Fix path issue on OSX
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
      (replace-regexp-in-string "[[:space:]\n]*$" ""
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when
  (equal system-type 'darwin)
  (set-exec-path-from-shell-PATH))

(add-to-list 'load-path "~/.emacs.d")

(delete-selection-mode t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-hl-line-mode 1)
(setq standard-indent 2)
(setq scroll-step 1)
(setq-default indent-tabs-mode nil)


(require 'package)
(setq package-archives (cons '("tromey" . "http://tromey.com/elpa/") package-archives))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-eshell
                      starter-kit-bindings
                      scpaste
                      css-mode
                      less-css-mode
                      org
                      clojure-mode
                      rainbow-delimiters
                      paredit
                      coffee-mode
                      ruby-mode
                      rinari
                      inf-ruby
                      clojure-test-mode
                      marmalade
                      color-theme-solarized
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Set font and rebind Command to Meta on OSX
(cond
  ((eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil)
    (set-frame-font "Menlo-12"))
  ((eq system-type 'windows-nt)
    (set-frame-font "Consolas-10")))

(load "custom/global")
(load "custom/bindings")
(load "custom/unicode")
(load "custom/ido")
(load "custom/hippie")
(load "custom/smart-tab")
(put 'narrow-to-region 'disabled nil)

(add-hook 'css-mode-hook '(lambda ()
                            (setq css-indent-level 2)
                            (setq css-indent-offset 2)))

(load-theme 'tango t)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(load "vendor/journal")
