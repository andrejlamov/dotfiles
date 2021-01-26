(require 's)

(defun almacs/rename-buffer (name)
  (interactive "B: " )
  (if (member major-mode '(shell-mode))
      (rename-buffer
       (if (and
            (s-ends-with? "*" name)
            (s-starts-with? "*" name))
           name
         (s-concat "*" (s-trim name) " (shell)*"))
       t)
    (rename-buffer name t)))

(provide 'almacs-utils)
