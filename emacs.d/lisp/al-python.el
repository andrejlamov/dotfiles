(require 'python)

(defun al/python-run-stm-in-python-shell ()
    (interactive)
    (save-window-excursion
      (let ((stm (if (eq evil-state 'normal)
                     (buffer-substring
                      (save-excursion (python-nav-beginning-of-statement))
                      (save-excursion (python-nav-end-of-statement)))
                   (buffer-substring (region-beginning) (region-end)))))
        (python-shell-switch-to-shell)
        (insert stm)
        (comint-send-input))))

(provide 'al-python)
