(require 'dash)

(setq evalka-target-buffer nil)

(defun evalka-set-target-buffer (buffer)
  (interactive "b")
  (setq evalka-target-buffer buffer))


(defun evalka-send-snippet (&optional snippet target-buffer)
  (interactive)
  (setq target-buffer (or target-buffer evalka-target-buffer))
  (let ((snippet  (buffer-substring-no-properties (region-beginning) (region-end))))
    (when target-buffer
      (with-current-buffer target-buffer
        (end-of-buffer)
        (comint-send-string target-buffer (concat "\n" snippet "\n\n"))
        (end-of-buffer)))))


(defun evalka-py-eval-thing-between-spaces ()
  (interactive)
  (let ((end (save-excursion (re-search-forward "\s\\|$")))
        (start (save-excursion (re-search-backward "\s\\|$"))))
    (evalka-send-snippet (-> (buffer-substring start end)
                             (string-trim)))))

(defun evalka-py-eval-dwim ()
  (interactive)
  (let ((snippet (buffer-substring
                  (if (region-active-p)
                      (region-beginning)
                    (save-excursion
                      (python-nav-beginning-of-statement)
                      (point)))
                  (if (region-active-p)
                      (region-end)
                    (save-excursion
                      (python-nav-end-of-statement)
                      (point))))))
    (evalka-send-snippet snippet)))

(defun evalka-py-eval-defun ()
  (interactive)
  (let ((snippet (buffer-substring
                  (save-excursion
                    (python-nav-beginning-of-defun)
                    (point))
                  (save-excursion
                    (python-nav-end-of-defun)
                    (point)))))
    (evalka-send-snippet snippet)))

(defun evalka-init ()
  (interactive)
  (require 'python)
  (keymap-set python-mode-map "C-x C-S-e" #'evalka-py-eval-thing-between-spaces)
  (keymap-set python-mode-map "C-x C-e" #'evalka-py-eval-dwim)
  (keymap-set python-mode-map "C-M-x" #'evalka-py-eval-defun))

(provide 'evalka)
