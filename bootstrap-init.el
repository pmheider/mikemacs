;;; From http://www.io.com/~jimm/emacs_tips.html
;;; Modified by Mike Prentice

(defun bootstrap-file (machine file-name)
  (concat *my-emacs-lib-dir* "bootstrap/" machine "/" file-name))

(defun load-init-if-exists (machine file)
  (let ((f (bootstrap-file machine file)))
    (if (file-exists-p (concat f ".el"))
	(load f))))

(defun bootstrap-init (machine)
  (load (concat *my-emacs-lib-dir* "emacs-variants")) ; allows versioning
  (load-init-if-exists machine "before")
  (load (concat *my-emacs-lib-dir* "mikemacs"))
  (load-init-if-exists machine "after")
  (setq bookmark-default-file
	(bootstrap-file machine "emacs.bmk")))
