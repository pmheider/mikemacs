;; ===== Keymappings =====

;; Global keymappings
(global-set-key "\M-Q" 'unfill-paragraph)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-h" 'backward-delete-char)
;; (global-set-key "\C-x\C-b" 'switch-to-buffer)
;; (global-set-key "\C-xb" 'list-buffers)
(global-set-key [?\C-%] 'match-paren)

;; text-mode keymappings
(define-key text-mode-map (kbd "TAB") 'self-insert-command);

(provide 'mikeys)
