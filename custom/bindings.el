;;; Key bindings

; Rebind command to meta on Mac
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)

;; Custom Meta shortcuts
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-x\C-z" 'magit-status)
(global-set-key (kbd "TAB") 'smart-tab)
