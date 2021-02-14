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

(provide 'al-utils)
