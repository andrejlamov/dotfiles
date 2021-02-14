(require 's)

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

(defun al/evil-eval-sexp (fn)
  (interactive)
  (save-excursion
    (let* ((c (char-to-string (char-after)))
           (is-right (member
                      c '("(" "[" "{")))
           (is-left (member
                     c '(")" "]" "}"))))
      (when is-right
        (evil-jump-item))
      (evil-forward-char)
      (call-interactively fn))))

(provide 'al-utils)
