;; Settings for hippie-expand

(setq hippie-expand-try-functions-list
       '(try-expand-dabbrev
         try-expand-dabbrev-from-kill
         try-expand-dabbrev-all-buffers
         try-expand-line
         try-complete-lisp-symbol
         try-complete-lisp-symbol-partially
         try-complete-file-name-partially
         try-complete-file-name))
