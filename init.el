;; set correct path on osx
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

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Visially show region
(setq transient-mark-mode t)

;; Everything should use fonts
(setq global-font-lock-mode 1)

;; Replace text selection
(delete-selection-mode t)

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq standard-indent 2)

;; Make sure all backup files only live in one place
(setq make-backup-files nil
      auto-save-default nil
      backup-directory-alist '("~/.emacs.d/backups"))

;; (setq scroll-step 1)
;; (global-hl-line-mode 1)

(setq search-highlight t            ;; highlight when searching
      query-replace-highlight t)    ;; and replacing

;; Start server if it's not running
(require 'server)
(when (and (functionp 'server-running-p) (not (server-running-p)))
  (server-start))

;; Make M-z stop minimizing frames
(defun iconify-or-deiconify-frame nil)

;; Make buffers with same name unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "|"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'animate)

(setq redisplay-dont-pause t)

;; Set cursor defaults
(blink-cursor-mode 1)
(set 'cursor-type 'bar)
(set-cursor-color "#ffff00")

;; Make gutter smaller
(fringe-mode 2)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ac-nrepl 
   ack-and-a-half 
   auto-complete 
   autopair 
   birds-of-paradise-plus-theme 
   blank-mode 
   buffer-move 
   clojure-test-mode 
   clojurescript-mode 
   deft 
   helm-projectile 
   helm 
   highlight-80+ 
   ir-black-theme 
   javascript 
   json 
   less-css-mode 
   magit 
   markdown-mode 
   molokai-theme 
   move-text 
   multiple-cursors 
   mustache-mode 
   nrepl 
   clojure-mode 
   paredit 
   projectile 
   rainbow-delimiters 
   smart-tab 
   smex 
   tango-2-theme 
   underwater-theme 
   undo-tree 
   web-mode 
   yasnippet 
   zen-and-art-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; OS specific stuff
(cond
  ((eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil)
    (set-frame-font "MonospaceTypewriter-12"))
  ((eq system-type 'windows-nt)
    (set-frame-font "Consolas-10")))

;; smoother mouse scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Display line and column numbers
(setq line-number-mode   t
      column-number-mode t)

;; Turn off scrollbar
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; Turn off toolbar
(tool-bar-mode -1)

;; Explicitly show the end of a buffer
;; (set-default 'indicate-empty-lines t)

;; Line-wrapping
(set-default 'fill-column 80)

;; Prevent the annoying beep on errors
(setq ring-bell-function (lambda () (message "*beep*")))

;; smart pairing for all
(electric-pair-mode t)

;; Gotta see matching parens
(show-paren-mode t)
(global-rainbow-delimiters-mode t)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Show file path as frame title

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Trailing whitespace is unnecessary
;; (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Trash can support
(setq delete-by-moving-to-trash t)

;; Reload file when changed on disk
(global-auto-revert-mode t)

;; Delete selected text after some key is pressed
(delete-selection-mode +1)

;; Unicode stuff
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default default-buffer-file-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Interactive do settings
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote
                       ("\n-> "
                        ""
                        "\n   "
                        "\n   ..."
                        "["
                        "]"
                        " [No match]"
                        " [Matched]"
                        " [Not readable]"
                        " [Too big]"
                        " [Confirm]")))

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(setq cua-enable-cua-keys nil)
(cua-mode)
(projectile-global-mode 1)
(winner-mode 1)

(defun duplicate-line ()
  (interactive)
  (let* ((cursor-column (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)
    (move-to-column cursor-column)))

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [\M-\S-up] 'move-text-up)
(global-set-key [\M-\S-down] 'move-text-down)

;; Rotate windows stuff
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(global-set-key (kbd "C-c r") 'rotate-windows)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Shift + direction to switch windows
(windmove-default-keybindings)

(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Custom Meta shortcuts
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "C-x C-z") 'magit-status)

(global-set-key (kbd "C-c j") (lambda () (interactive) (join-line -1)))

(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c f") 'projectile-find-file)

(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)


(add-hook 'css-mode-hook '(lambda ()
                            (setq css-indent-level 2)
                            (setq css-indent-offset 2)))

;; Org mode stuff

(setq org-directory "~/Dropbox/Org")
(setq org-archive-location "~/Dropbox/Org/archive.org::From %s")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (quote ("~/Dropbox/Org")))

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-ca" 'org-agenda)

(require 'smart-tab)
(global-smart-tab-mode 1)

(load "vendor/journal")
(load "vendor/buffer-move")

;; Enable theme
(add-to-list 'custom-theme-load-path "/Users/ilia/.emacs.d/themes")
(load-theme 'deeper-blue t)

;; dired - reuse current buffer by pressing 'a'

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; clojure stuff
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

(add-hook 'clojure-mode-hook 'paredit-mode)

;; nrepl config
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; Lost and found ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keybinds
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)

;; mac switch meta key
(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'hyper))
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta))))

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes (quote ("6615e5aefae7d222a0c252c81aac52c4efb2218d35dfbb93c023c4b94d3fa0db" "38c4fb6c8b2625f6307f3dde763d5c61d774d854ecee9c5eb9c5433350bc0bef" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "fca8ce385e5424064320d2790297f735ecfde494674193b061b9ac371526d059" "02414c4cfbbe9805b89a5ec66d3d3deb4ae1e4795ed2092eab240ca0cb79ea96" "d24e10524bb50385f7631400950ba488fa45560afcadd21e6e03c2f5d0fad194" "f38dd27d6462c0dac285aa95ae28aeb7df7e545f8930688c18960aeaf4e807ed" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" "303488aa27ce49f658a7ba4035e93380421e394ec2799ae8fd952d08808c7235" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "47d2a01f2cbd853ccd1eddcb0e9e4fdfdabcc97ddad1d1a5218304294889f731" "06f5145c01ec774a0abb49eeffa3980743ce2f997112b537effeb188b7c51caf" "8f6537eb6f9d66b060c736f5f680f5c661e0a6b311b86defa293bc5ba104a030" "3bd9497fb8f39c28ab58a9e957152ba2dc41223c23c5520ef10fc7bd6b222384" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" default)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
