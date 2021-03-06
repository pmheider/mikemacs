;; mikemacs.el
;; 
;; Machine-independent customizations and required libraries for Mikey's Emacs
;; happiness.
;; 
;; Copyright (C) 2009 Mike Prentice
;; Author: Mike Prentice (mjp44@buffalo.edu)
;; License: GPL version 3 or (at your option) any later version

;; ===== Variables =====
(defvar *my-emacs-lib-dir* (file-name-directory (or load-file-name
                                                    (buffer-file-name)))
  "mikemacs directory.  Set in .emacs (default ~/.emacs.d/mikemacs)")

;; Mike-specific!
(setq user-mail-address "mjp44@buffalo.edu")

;; eshell aliases
(setq eshell-aliases-file (concat *my-emacs-lib-dir* "eshell-aliases"))


;; ===== Required =====

;; try not to require anything, rely on autoloads


;; ===== Modes =====

;; autoload modes
(autoload 'html-helper-mode "html-helper-mode" "Enhanced HTML editing" t)
(autoload 'htmlize-file "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-buffer "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-region "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-many-files "htmlize" "Syntax highlighting to HTML" t)
(autoload 'htmlize-many-files-dired "htmlize" "Syntax highlighting to HTML" t)
(autoload 'matlab-mode "matlab" "Major mode for editing Matlab files" t)
(when (eq (emacs-variant) 'xemacs)
  (autoload 'run-acl "init-for-acl" "Initialize Allegro Lisp" t))
(autoload 'sml-mode "sml-mode-startup" "Major mode for editing SML files" t)
(autoload 'run-sml "sml-mode-startup" "Inferior SML shell" t)
(autoload 'erlang-mode "erlang-start" "Major mode for editing Erlang files" t)
(autoload 'run-erlang "erlang-start" "Inferior Erlang shell" t)
(autoload 'graphviz-dot-mode "graphviz-dot-mode"
  "Major mode for editing Graphviz dot files" t)
(autoload 'run-ruby "inf-ruby" "Inferior Ruby shell (irb)" t)
(autoload 'ses-mode "ses" "Simple Emacs Spreadsheet mode" t)
(autoload 'bee-mode "bmacs" "Bigloo Scheme mode for Emacs" t)
(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
(autoload 'clojure-mode "clojure-mode" "Major mode for editing Clojure files" t)

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
(add-to-list 'auto-mode-alist '("\\.ml$" . sml-mode))
(add-to-list 'auto-mode-alist '("\\.sml$" . sml-mode))
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.ses$" . ses-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
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

;; XEmacs font lock hook
(when (eq (emacs-variant) 'xemacs)
  (add-hook 'find-file-hooks (lambda () (font-lock-mode 1))))

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
(if (eq (emacs-variant) 'emacs)
  (add-hook 'comint-mode-hook (lambda () (turn-off-auto-fill)))
  (add-hook 'comint-mode-hook (lambda () (auto-fill-mode))))

(add-hook 'graphviz-dot-mode-hook
          (lambda () (setq graphviz-dot-auto-indent-on-semi nil)))

;; Load inf-ruby if exists
(add-hook 'ruby-mode-hook
          (lambda ()
            (when (locate-library "inf-ruby")
              (require 'inf-ruby))))


;; ===== Library-specific initializations =====

;; Slime: Superior Lisp Interaction Mode for Emacs
;; Requires setting machine-specific executable paths, e.g.
;; (setq inferior-lisp-program "/path/to/lisp-exe")
;; (setq slime-lisp-implementations
;;       '((sbcl ("/path/to/sbcl"))
;;         (ccl ("/path/to/ccl"))
;;         (clisp ("/path/to/clisp"))))
(when (locate-library "slime-autoloads")
  (require 'slime-autoloads)
  (slime-setup '(inferior-slime slime-scheme)))
;; Add Clojure support
(when (locate-library "swank-clojure-autoload")
  (setq swank-clojure-binary "clojure")
  (require 'swank-clojure-autoload))

;; CEDET: Collection of Emacs Development Environment Tools
;; TODO: figure out why cedet isn't working on cse
;(when (locate-library "cedet")
;  (require 'cedet)
;  (semantic-load-enable-minimum-features)
;  (require 'semantic-ia)
;  (require 'semantic-gcc)
;  (semantic-load-enable-code-helpers)
;  (global-ede-mode t))

;; ECB: Emacs Code Browser
(when (locate-library "ecb")
  (require 'ecb-autoloads))

;; Ido: interactive do
;; Annoying behavior in XEmacs
(when (and (eq (emacs-variant) 'emacs) (locate-library "ido"))
  (require 'ido)
  (ido-mode t)
  (add-to-list 'ido-ignore-files "\\`\\.git/")
  (add-to-list 'ido-ignore-files "\\`\\.svn/")
  (setq ido-everywhere t
        ido-case-fold t
        ido-use-filename-at-point t
        ido-use-url-at-point t
        ido-confirm-unique-completion t
        ido-auto-merge-work-directories-length -1
        ido-enable-flex-matching t)
  )

;; Yasnippet: Yet another snippet library
;; There is some problem with function remove-if that prevents loading.
(let ((snippet-lib (locate-library "yasnippet")))
  (when (and (eq (emacs-variant) 'emacs) snippet-lib)
    (require 'yasnippet)
    (yas/initialize)
    (yas/load-directory (concat (file-name-directory
                                 snippet-lib)
                                "snippets"))))

(when (locate-library "ipython")
  (autoload 'py-shell "ipython" "Interactive python shell with ipython" t)
  (setq py-python-command-args '( "-colors" "NoColor")))

;; Bigloo (Scheme) mode for Emacs
(when (locate-library "bmacs")
  (add-to-list 'auto-mode-alist '("\\.scm$" . bee-mode))
  (add-to-list 'auto-mode-alist '("\\.sch$" . bee-mode)))

;; vc-git and git-emacs for emacs
(when (and (eq (emacs-variant) 'emacs) (locate-library "vc-git"))
  (require 'vc-git)
  (add-to-list 'vc-handled-backends 'git))
(when (and (eq (emacs-variant) 'emacs) (locate-library "git"))
  (require 'git)
  (when (and window-system (locate-library "git-emacs"))
    (require 'git-emacs)
    ;; ido not working, why?
    (setq git--completing-read #'completing-read)))

;; Abbrev mode settings
(setq abbrev-file-name (concat *my-emacs-lib-dir* "abbrev-defs.el"))
(read-abbrev-file abbrev-file-name t)
(setq dabbrev-case-replace nil)
(setq abbrev-mode t)

(when (locate-library "highlight-parentheses")
  (require 'highlight-parentheses))
(setq show-paren-mode t)

;;; I don't like quack, adds a global menu item
;; (when (locate-library "quack") (require 'quack))

;;; Add fontification to allout-mode (an outline-mode)
;;; From http://www.emacswiki.org/emacs/AllOut
(defun dto/allout-highlight-headings ()
  (interactive)
  (font-lock-add-keywords nil '(("^\\(\\* \\|\\(\\. *\\* \\)\\)\\(.*\\)$" 3
                                 font-lock-keyword-face t))))
(add-hook 'allout-mode-hook 'dto/allout-highlight-headings)


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

;; Eshell definitions
(defun eshell/eshell () (rename-uniquely) (eshell))

;; Build tags for a project
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (eshell-command
;;    (format "find %s -type f -name \"*.[ch]\" | ctags -e -L -" dir-name)))


;; ===== Keymappings =====

;; Global keymappings
(global-set-key "\M-Q" 'unfill-paragraph)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "%" 'match-paren)
(global-set-key "\C-ce" 'eshell)
(global-set-key "\C-cr" 'revert-buffer)

;; mode-specific keymappings

;; (define-key text-mode-map (kbd "TAB") 'self-insert-command)
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map [(control ?a)] 'eshell-bol)
            (define-key eshell-mode-map [(control ?c) (control ?a)]
              'move-beginning-of-line)))
(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\C-m"
              'ruby-reindent-then-newline-and-indent)
            (define-key ruby-mode-map [(control ?c) (control ?l)]
              'g0-ruby-load-buffer)))
(add-hook 'erlang-mode-hook
          (lambda ()
            (define-key erlang-mode-map "\C-m"
              'newline-and-indent)))

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

;; Save bookmarks whenever created or deleted
(setq bookmark-save-flag 1)
