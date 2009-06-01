;; mikemacs.el
;; 
;; Machine-independent customizations and required libraries for Mikey's Emacs
;; happiness.
;; 
;; Copyright (C) 2009 Mike Prentice
;; Author: Mike Prentice (mjp44@buffalo.edu)
;; License: GPL version 2 or (at your option) any later version

;; ===== Required =====

(require 'emacs-variants)


;; ===== Modes =====

;; autoload modes
(autoload 'html-helper-mode "html-helper-mode" "Enhanced HTML editing" t)
(autoload 'htmlize-file "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-buffer "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-region "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-many-files "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-many-files-dired "htmlize" "Syntax highlighting to HTML" t)
(autoload 'matlab-mode "matlab" "Major mode for editing Matlab files" t)
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
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(add-to-list 'auto-mode-alist '("\\.snepslog$" . lisp-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . inferior-ruby-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Set global modes
(setq-default auto-fill-function 'do-auto-fill)
(setq-default line-number-mode t)       ; display line
(setq-default column-number-mode t)     ; display column
(setq-default font-lock-maximum-decoration t) ; font locking
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t)           ; GNU Emacs font locking
  (setq font-lock-auto-fontify t))      ; XEmacs font locking
(setq-default indent-tabs-mode nil)     ; Always indent with spaces

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

;; Make #! scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Turn off auto-filling in shells
(add-hook 'shell-mode-hook (lambda () (turn-off-auto-fill)))


;; ===== Library-specific initializations =====

;; Slime: Superior Lisp Interaction Mode for Emacs
;; Requires setting machine-specific executable paths, e.g.
;; (setq inferior-lisp-program "/path/to/lisp-exe")
;; (setq slime-lisp-implementations
;;       '((sbcl ("/path/to/sbcl"))
;;         (ccl ("/path/to/ccl"))
;;         (clisp ("/path/to/clisp"))))
(when (and (eq (emacs-variant) 'emacs) (locate-library "slime-autoloads"))
  (require 'slime-autoloads)
  (slime-setup))

;; Ido: interactive do
(when (locate-library "ido")
  (require 'ido)
  (ido-mode t)
  (add-to-list 'ido-ignore-files "\\`\\.git/")
  )

;; Yasnippet: Yet another snippet library
;; There is some problem with function remove-if that prevents loading.
;; (when (locate-library "yasnippet")
;;   (require 'yasnippet)
;;   (yas/initialize)
;;   (yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets"))

(setq py-python-command-args '( "-colors" "NoColor")) ; for py-shell

;; vc-git and git-emacs for emacs
(when (locate-library "vc-git")
  (require 'vc-git)
  (add-to-list 'vc-handled-backends 'git))
(cond ((and window-system (locate-library "git-emacs"))
       (require 'git-emacs)
       (setq git--completing-read #'completing-read)) ; ido not working, why?
      ((locate-library "git") (require 'git)))

(when (locate-library "ipython") (require 'ipython))
(when (locate-library "graphviz-dot-mode")
  (load-library "graphviz-dot-mode"))
(when (locate-library "sml-mode-startup")
  (load-library "sml-mode-startup"))

;; Requires machine-specific erlang executable path, e.g.
;; (setq erlang-root-dir "/path/to/erlang")
;; (add-to-list 'exec-path "/path/to/erlang/bin")
(when (locate-library "erlang-start") (require 'erlang-start))


;; ===== Function definitions =====

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;; Takes a multi-line paragraph and makes it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (point) (mark))))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t (backward-char 1)
           (cond ((looking-at "\\s\)")
                  (forward-char 1) (backward-list 1))
                 (t (forward-char 1) (self-insert-command (or arg 1)))))))

(defun fixssh ()
  "Run fixssh script for use in GNU screen with X forwarding"
  (interactive)
  (save-excursion
    (let ((buffer (find-file-noselect "~/bin/fixssh")))
      (set-buffer buffer)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (while (re-search-forward
              "\\([A-Z_][A-Z0-9_]*\\) *= *\"\\([^\"]*\\)\"" nil t)
        (let ((key (match-string 1))
              (val (match-string 2)))
          (setenv key val)))
      (kill-buffer buffer))))

;; http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-talk/47275
(defun g0-ruby-load-buffer ()
  "Save current buffer, and load it into an inferior ruby process.

If there is no inferior ruby process running, start one.  Otherwise,
load the current buffer into the currently running process.  Switch to
`ruby-buffer'."
  (interactive)
  (save-buffer)
  (let ((bufname (buffer-name))
        (pop-up-windows t))
    (run-ruby ruby-program-name)
    (ruby-load-file bufname)))


;; ===== Keymappings =====

;; Global keymappings
(global-set-key "\M-Q" 'unfill-paragraph)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "%" 'match-paren)

;; mode-specific keymappings

(define-key text-mode-map (kbd "TAB") 'self-insert-command)
(add-hook 'inferior-ruby-mode-hook
          (lambda ()
            (when (featurep 'inf-ruby)
              (define-key inferior-ruby-mode-map [(control ?c) (control ?l)]
                'g0-ruby-load-buffer))))


;; ===== Miscellaneous =====

;; Set coding system to UTF-8
(when (eq (emacs-variant) 'emacs)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

;; Make sure text files end in newline
(setq-default require-final-newline t)

;; Set tab width and margins
(setq-default fill-column 79)
(setq-default tab-width 8)
(setq-default standard-indent 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; Make delete work properly for OS X
(when (eq (emacs-variant) 'emacs)
  (normal-erase-is-backspace-mode 0))

;; Enable wheelmouse support by default
(when window-system
  (mwheel-install))

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(when (eq (emacs-variant) 'xemacs)
  (setq default-frame-plist '(width 80 height 40)))

(add-to-list 'completion-ignored-extensions ".elc")

(provide 'mikemacs)
