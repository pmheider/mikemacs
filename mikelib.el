;; Library-specific initializations

(when (featurep 'slime)
  (setq inferior-lisp-program "/Applications/AllegroCL/mlisp")
  (setq slime-lisp-implementations
        '((mlisp ("/Applications/AllegroCL/mlisp"))
          (alisp ("/Applications/AllegroCL/alisp"))
          (sbcl ("/opt/local/bin/sbcl"))
          (ccl ("/Users/mprentice/bin/ccl64"))
          (clisp ("/opt/local/bin/clisp"))))
  (slime-setup))
(when (featurep 'ido) (ido-mode t))
(when (featurep 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets"))
(setq py-python-command-args '( "-colors" "NoColor"))
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))

(provide 'mikelib)
