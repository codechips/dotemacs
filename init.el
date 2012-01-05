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


(setq search-highlight t            ;; highlight when searching...
  query-replace-highlight t)        ;; ...and replacing

;; Don't start the server unless we can verify that it isn't running.
(require 'server)
(when (and (functionp 'server-running-p) (not (server-running-p)))
(server-start))

;; Make M-z stop minimizing frames
(defun iconify-or-deiconify-frame nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'animate)

(blink-cursor-mode 0)

;; disable C-z on X11 sessions
(when window-system
(global-unset-key "\C-z"))

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

(setq org-archive-location "~/Dropbox/Org/archive.org::From %s")
(setq org-directory "~/Dropbox/Org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-agenda-files (quote ("~/Dropbox/Org")))
(setq org-mobile-inbox-for-pull "~/Dropbox/Org/from-mobile.org")

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(load "vendor/journal")

