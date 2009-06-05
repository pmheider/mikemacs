;; before.el
;; To be loaded on Victoria by bootstrap-init

;; ===== Machine-specific customizations =====

;; Set load paths
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/slime")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(add-to-list 'load-path "~/.emacs.d/mikemacs")
(add-to-list 'load-path "~/src/git/contrib/emacs")
(add-to-list 'load-path "/opt/local/lib/erlang/lib/tools-2.6.2/emacs")
(add-to-list 'load-path "~/.emacs.d/plugins/git-emacs")

;; Machine-specific variables
(setq inferior-lisp-program "/Applications/AllegroCL/mlisp")
(setq slime-lisp-implementations
      '((mlisp ("/Applications/AllegroCL/mlisp"))
        (alisp ("/Applications/AllegroCL/alisp"))
        (sbcl ("/opt/local/bin/sbcl"))
        (ccl ("/Users/mprentice/bin/ccl64"))
        (clisp ("/opt/local/bin/clisp"))))
(setq erlang-root-dir "/opt/local/lib/erlang")
(add-to-list 'exec-path "/opt/local/lib/erlang/bin")
(setq scheme-program-name "mzscheme")
