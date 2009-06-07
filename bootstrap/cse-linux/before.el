;; before.el
;; To be loaded on CSE Linux machines by bootstrap-init

;; ===== Machine-specific customizations =====

;; Set load paths
(add-to-list 'load-path "~/.emacs.d/mikemacs")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/slime")
(add-to-list 'load-path "/util/erlang/lib/erlang/lib/tools-2.6.2/emacs")
(add-to-list 'load-path "~/.emacs.d/plugins/sml-mode")

;; Machine-specific variables

(setq inferior-lisp-program "/util/bin/mlisp")
(setq slime-lisp-implementations
      '((mlisp ("/util/bin/mlisp"))
        (alisp ("/util/bin/alisp"))))
(setq erlang-root-dir "/util/erlang/lib/erlang")
(add-to-list 'exec-path "/util/erlang/lib/erlang/bin")
(setq scheme-program-name "mzscheme")

(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))
(setq shell-file-name "/bin/bash")
