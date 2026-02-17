

(require 'cl-lib)
(require 'dired-x)
(require 'magit)
(require 's)

(cl-defun al-live (&key (title "")
                        (fun (lambda ()))
                        (initial-input nil)
                        (on-complete (lambda ()))
                        (on-init (lambda ())))
  (let* ((timer nil)
         (my-buffer (window-buffer (minibuffer-selected-window)))
         (on-keypress (lambda (beg end length)
                        (when (timerp timer)
                          (cancel-timer timer))
                        (setq timer (run-at-time 0.2 nil (lambda ()
                                                         (let ((contents (minibuffer-contents)))
                                                           (with-current-buffer my-buffer
                                                             (funcall fun contents)))))))))
    (minibuffer-with-setup-hook
        (lambda ()
          (funcall on-init)
          (add-hook 'after-change-functions on-keypress nil t))
      (let ((res (read-string title initial-input)))
        (when res
          (funcall on-complete))))))

;;;###autoload
(defun al-live/occur ()
  (interactive)
  (al-live :title "Occur: "
           :on-init (lambda ()
                      (local-set-key (kbd "C-n") (lambda () (interactive)
                                                   (with-selected-window (get-buffer-window "*Occur*")
                                                     (next-line 1)
                                                     (occur-mode-goto-occurrence) )))
                      (local-set-key (kbd "C-p") (lambda () (interactive)
                                                   (with-selected-window  (get-buffer-window "*Occur*")
                                                     (previous-line 1)
                                                     (occur-mode-goto-occurrence)))))
           :fun (lambda (contents)
                  (occur contents))))

;;;###autoload
(defun al-live/grep ()
  (interactive)
  (let ((confirm-kill-processes nil))
    (al-live :title "Grep: "
             :on-init (lambda ()
                        (add-hook 'grep-mode-hook (lambda () (setq-local compilation-always-kill t)))
                        (local-set-key (kbd "C-n") (lambda () (interactive)
                                                     (with-selected-window (get-buffer-window "*grep*")
                                                       (next-line 1)
                                                       (compile-goto-error))))
                        (local-set-key (kbd "C-p") (lambda () (interactive)
                                                     (with-selected-window  (get-buffer-window "*grep*")
                                                       (previous-line 1)
                                                       (compile-goto-error)))))
             :initial-input  "grep --color=auto -nH --null -e "
             :fun (lambda (contents)
                    (grep contents)))))

;; TODO: C-return got to the buffer, while only return goes to the current line if any
;;;###autoload
(defun al-live/grep-marked-dired-files ()
  (interactive)
  (let ((confirm-kill-processes nil)
        (marked-files (s-join " " (dired-get-marked-files))))
    (al-live :title "Grep: "
             :on-init (lambda ()
                        (add-hook 'grep-mode-hook (lambda () (setq-local compilation-always-kill t)))
                        (local-set-key (kbd "C-n") (lambda () (interactive)
                                                     (with-selected-window (get-buffer-window "*grep*")
                                                       (next-line 1)
                                                       (compile-goto-error))))
                        (local-set-key (kbd "C-p") (lambda () (interactive)
                                                     (with-selected-window  (get-buffer-window "*grep*")
                                                       (previous-line 1)
                                                       (compile-goto-error)))))
             :initial-input  (concat "grep --color=auto -nH --null -e " marked-files)
             :fun (lambda (contents)
                    (grep contents)))))

;;;###autoload
(defun al-live/git-ls-files ()
  (interactive)
  (al-live :title "Command: "
           :on-init (lambda ())
           ;; TODO: put cursor in the correct position to "continue" with chosen command
           :initial-input  (concat "cd " (magit-toplevel) " && git ls-files  | grep  | xargs ls -lah ;")
           :fun (lambda (contents)
                  (async-shell-command contents "*git-ls-files*"))
           :on-complete (lambda ()
                          (switch-to-buffer-other-window "*git-ls-files*")
                          (dired-virtual (magit-toplevel)))))

(provide 'al-live)
