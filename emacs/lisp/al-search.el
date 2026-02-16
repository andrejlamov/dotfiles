(require 'cl-lib)
(require 'dired-x)
(require 'magit)

(cl-defun al/live (&key (title "")
                     (fun (lambda ()))
                     (initial-input nil)
                     (on-complete (lambda ())))
  (let* ((timer nil)
         (my-buffer (window-buffer (minibuffer-selected-window)))
         (on-keypress (lambda (beg end length)
                        (when (timerp timer)
                          (cancel-timer timer))
                        (setq timer (run-at-time 1 nil (lambda ()
                                                      (let ((contents (minibuffer-contents)))
                                                        (with-current-buffer my-buffer
                                                          (funcall fun contents)))))))))
    (minibuffer-with-setup-hook
        (lambda () (add-hook 'after-change-functions on-keypress nil t))
      (let ((res (read-string title initial-input)))
        (when res
          (funcall on-complete))))))

;;;###autoload
(defun al/live-occur ()
  (interactive)
  (al/live :title "Occur:"
           :fun (lambda (contents)
                  (occur contents))
           :on-complete (lambda ()
                          (split-window-vertically nil (selected-window))
                          (other-window 1)
                          (switch-to-buffer "*Occur*"))))

;;;###autoload
(defun al/live-grep ()
  (interactive)
  (al/live :title "Grep:"
           :initial-input  "grep --color=auto -nH --null -e "
           :fun (lambda (contents)
             (grep contents))
           :on-complete (lambda ()
             (split-window-vertically nil (selected-window))
             (other-window 1)
             (switch-to-buffer "*grep*"))))

;;;###autoload
(defun al/live-git-ls-files ()
  (interactive)
  (al/live :title ""
           :initial-input  (concat "cd " (magit-toplevel) " && git ls-files  | grep  | xargs ls -lah")
           :fun (lambda (contents)
                  (shell-command contents "*git-ls-files*"))
           :on-complete (lambda ()
                          (split-window-vertically nil (selected-window))
                          (other-window 1)
                          (switch-to-buffer "*git-ls-files*")
                          (dired-virtual (magit-toplevel)))))


(provide 'al-search)
