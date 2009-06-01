;; Library-specific initializations

(when (featurep 'slime)
  (slime-setup))
(when (featurep 'ido)
  (ido-mode t)
  (add-to-list 'ido-ignore-files "\\`\\.git/")
  )
(when (featurep 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets"))
(setq py-python-command-args '( "-colors" "NoColor"))
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(when (featurep 'git-emacs)
  (setq git--completing-read #'completing-read)) ; ido not working, why?

(provide 'mikelib)
