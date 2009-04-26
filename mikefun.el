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
  (when (not (looking-at "\\s\(")) (backward-char 1))
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (forward-char 1) (self-insert-command (or arg 1)))))

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

(provide 'mikefun)
