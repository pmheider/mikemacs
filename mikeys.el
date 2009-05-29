;; ===== Keymappings =====

;; Global keymappings
(global-set-key "\M-Q" 'unfill-paragraph)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "%" 'match-paren)

;; mode-specific keymappings
(define-key text-mode-map (kbd "TAB") 'self-insert-command);
(when (featurep 'inf-ruby)
  (define-key inferior-ruby-mode-map [(control ?c) (control ?l)]
    'g0-ruby-load-buffer))

(provide 'mikeys)
