;; ===== Modes =====

;; autoload modes
(autoload 'html-helper-mode "html-helper-mode" "Load html-helper-mode" t)
(autoload 'htmlize-file "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-buffer "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-region "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-many-files "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-many-files-dired "htmlize" "Syntax highlighting to HTML" t)
(autoload 'matlab-mode "matlab" "Load matlab-mode" t)
(autoload 'pstxt-x-unfill-region "pstxt" "Unfill region" t)
(autoload 'pstxt-x-unfill-buffer "pstxt" "Unfill buffer" t)
(when (eq (emacs-variant) 'xemacs)
  (autoload 'run-acl "init-for-acl" "Initialize Allegro Lisp" t))

(add-to-list 'auto-mode-alist '("\\.l$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.y$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.ypp$" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.js$" . c-mode))
;; (add-to-list 'auto-mode-alist '("\\.html$" . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.php$" . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.css$" . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . inferior-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(add-to-list 'auto-mode-alist '("\\.snepslog$" . lisp-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . inferior-ruby-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Set global modes
(setq-default auto-fill-function 'do-auto-fill)
(setq-default font-lock-maximum-decoration t)
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(when (eq (emacs-variant) 'emacs) (global-font-lock-mode t))

;; Enable visual feedback on selections
(setq transient-mark-mode t)

;; Mode hooks
(add-hook 'text-mode-hook (lambda ()
                            (setq indent-tabs-mode t)
                            (setq tab-width 8)
                            ))
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)
(add-hook 'sql-interactive-mode-hook (lambda () (setq tab-width 8)))

(when (featurep 'slime)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t))))

;; Make #! scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Always indent with spaces
(setq-default indent-tabs-mode nil)

(add-hook 'shell-mode-hook (lambda () (turn-off-auto-fill)))

(provide 'mikemodes)
