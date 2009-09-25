;; before.el
;; To be loaded on Victoria by bootstrap-init

;; ===== Machine-specific customizations =====

;; Set load paths
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/slime")
(add-to-list 'load-path "~/.emacs.d/plugins/slime/contrib")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(add-to-list 'load-path "~/.emacs.d/plugins/ses21-031130")
(add-to-list 'load-path "~/.emacs.d/plugins/cedet-1.0pre6/common")
(add-to-list 'load-path "~/.emacs.d/plugins/jde-2.3.5.1/lisp")
(add-to-list 'load-path "~/.emacs.d/plugins/ecb-2.40")
(add-to-list 'load-path "~/.emacs.d/mikemacs")
(add-to-list 'load-path "~/src/git/contrib/emacs")
(add-to-list 'load-path "/opt/local/lib/erlang/lib/tools-2.6.4/emacs")
(add-to-list 'load-path "~/.emacs.d/plugins/git-emacs")
(add-to-list 'load-path "/usr/local/share/emacs/bigloo/")
(add-to-list 'load-path "~/.emacs.d/plugins/clojure-mode")
(add-to-list 'load-path "~/.emacs.d/plugins/swank-clojure")

;; Machine-specific variables
(setq inferior-lisp-program "/Applications/AllegroCL/mlisp")
;(setq inferior-lisp-program "/Applications/ccl/dx86cl64")
(setq slime-lisp-implementations
      '((mlisp ("/Applications/AllegroCL/mlisp"))
        (alisp ("/Applications/AllegroCL/alisp"))
        (sbcl ("/opt/local/bin/sbcl"))
        (ccl ("/Applications/ccl/dx86cl64"))
        (clisp ("/opt/local/bin/clisp"))))
(setq erlang-root-dir "/opt/local/lib/erlang")
(add-to-list 'exec-path "/opt/local/lib/erlang/bin")
(setq scheme-program-name "mzscheme")
(setq prolog-program-name "gprolog")
