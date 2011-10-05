;; Use command as the meta key
;; (setq ns-command-modifier (quote meta))

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Visially show region
(setq transient-mark-mode t)

;; Everything should use fonts
(setq global-font-lock-mode 1)

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;; Turn off scrollbar
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; Turn off toolbar
(tool-bar-mode -1)
;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)

;; Line-wrapping
(set-default 'fill-column 80)

;; Prevent the annoying beep on errors
(setq visible-bell t)

;; Make sure all backup files only live in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Gotta see matching parens
(show-paren-mode t)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; For emacsclient
(server-start)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Trash can support
(setq delete-by-moving-to-trash t)

