;; mikemacs.el
;; 
;; Machine-independent customizations and required libraries for Mikey's Emacs
;; happiness.
;; 
;; Copyright (C) 2009 Mike Prentice
;; Author: Mike Prentice (mjp44@buffalo.edu)
;; License: GPL version 2 or (at your option) any later version

(require 'emacs-variants)
(require 'mikemodes)
(require 'mikelib)
(require 'mikefun)
(require 'mikeys)

;; ===== Miscellaneous =====

;; Set coding system to UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Make sure text files end in newline
(setq-default require-final-newline t)

;; Set tab width and margins
(setq-default fill-column 79)
(setq-default tab-width 8)
(setq-default standard-indent 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; Enable wheelmouse support by default
(cond (window-system
       (mwheel-install)))

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(provide 'mikemacs)
