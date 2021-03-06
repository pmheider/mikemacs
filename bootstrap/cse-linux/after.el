;; after.el
;; To be loaded on CSE Linux machines by bootstrap-init

;; Load libraries
(if (eq (emacs-variant) 'xemacs)
    (require 'tex-site))

;; Change pdf viewer to acroread
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (add-to-list 'TeX-output-view-style
                           '("^pdf$" "." "acroread %o")))
          t)

;; Fix broken yank to yank from clipboard
;; Yank from Emacs: C-y
;; Yank from system clipboard: C-M-y
(global-set-key "\C-\M-y" 'clipboard-yank)

;; Shortcut since nothing seems to use it
;; Save buffer: M-s
(global-set-key "\M-s" 'save-buffer)

;; ipython broken on some cse machines, why??
(setq py-python-command-args '())
