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
