;; ===== Keymappings =====

;; Global keymappings
(global-set-key "\M-Q" 'unfill-paragraph)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "%" 'match-paren)

;; text-mode keymappings
(define-key text-mode-map (kbd "TAB") 'self-insert-command);

(provide 'mikeys)
