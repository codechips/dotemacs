(require 'cl)
;; el-get packages

(unless (require 'el-get nil t)
  (message "We need to install el-get")
  (url-retrieve
    "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
    (lambda (s)
      (end-of-buffer)
      (eval-print-last-sexp))))

(setq el-get-sources
      '((:name css-mode :type elpa)
        (:name slime :type elpa)
        (:name slime-repl :type elpa)
        (:name slime-clj :type elpa)
        (:name elein :type elpa)
        (:name swank-clojure :type elpa)
        (:name full-ack
               :type git
               :url "https://github.com/nschum/full-ack.git")
        (:name highlight-parentheses
               :type git
               :url "git://github.com/nschum/highlight-parentheses.el.git"
               :features highlight-parentheses)
        (:name idle-highlight
               :type git
               :url "https://github.com/emacsmirror/idle-highlight.git")))

(setq el-get-packages
      '(el-get
         clojure-mode
         magit
         deft
         paredit
         coffee-mode
         csharp-mode
         color-theme
         color-theme-inkpot
         color-theme-ir-black
         color-theme-zen-and-art
         color-theme-solarized
         color-theme-empty-void
         color-theme-almost-monokai
         color-theme-tango
         org-mode
         ruby-mode
         rinari
         ruby-electric
         rvm
         yasnippet))

(setq el-get-packages
      (append el-get-packages
          (loop for src in el-get-sources collect (el-get-source-name src))))

(el-get 'sync el-get-packages)


