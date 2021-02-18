(require 'python)

(defun al/python-run-stm-in-python-shell ()
  (interactive)
  (al/python-run-string-in-python-shell
   (buffer-substring
    (save-excursion (python-nav-beginning-of-statement))
    (save-excursion (python-nav-end-of-statement)))))

(defun al/python-run-region-in-python-shell ()
  (interactive)
  (al/python-run-string-in-python-shell
   (buffer-substring (region-beginning) (region-end))))

(defun al/python-run-string-in-python-shell (string)
  (interactive)
  (save-window-excursion
   (python-shell-switch-to-shell)
   (insert string)
   (comint-send-input)))

(provide 'al-python)
