(require 's)
(require 'evil)
(require 'evil-cleverparens)

(defun al/rename-buffer (name)
  (interactive "Bnew buffer name: " )
  (if (member major-mode '(shell-mode))
      (rename-buffer
       (if (and
            (s-ends-with? "*" name)
            (s-starts-with? "*" name))
           name
         (s-concat "*" (s-trim name) " (shell)*"))
       t)
    (rename-buffer name t)))

(setq al/evil-right-delimeters '("(" "[" "{"))

(defun al/evil-eval-sexp (fn)
  (interactive)
  (save-excursion
    (let* ((c (char-to-string (char-after)))
           (is-right (member
                      c al/evil-right-delimeters)))
      (when is-right
        (evil-jump-item))
      (evil-forward-char)
      (call-interactively fn))))

(defun al/delete-sexp-or-symbol ()
  (interactive)
  (let ((is-right (member (char-to-string (char-after)) al/evil-right-delimeters)))
    (if is-right (call-interactively 'evil-cp-delete-sexp) (call-interactively 'sp-delete-symbol))))

(provide 'al-utils)
