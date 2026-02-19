;; TODO: do something with find-grep-dired (make it use git ls-files)
(require 'cl-lib)
(require 'dired-x)
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

(defun al-live/command-with-cursor-position (title command-prefix command-postfix)
  (list
   :position (+ (length title) (length command-prefix) 1)
   :title title
   :command (concat command-prefix command-postfix)))

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

;;;###autoload
(defun al-live/git-grep ()
  (interactive)
  (let ((confirm-kill-processes nil))
    (al-live :title "Gitgrep: "
             :on-init (lambda ()
                        (goto-char (point-max))
                        (add-hook 'grep-mode-hook (lambda () (setq-local compilation-always-kill t)))
                        (local-set-key (kbd "C-n") (lambda () (interactive)
                                                     (with-selected-window (get-buffer-window "*grep*")
                                                       (next-line 1)
                                                       (compile-goto-error))))
                        (local-set-key (kbd "C-p") (lambda () (interactive)
                                                     (with-selected-window  (get-buffer-window "*grep*")
                                                       (previous-line 1)
                                                       (compile-goto-error)))))
             :initial-input "git grep -n "
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
  (let ((command (al-live/command-with-cursor-position
                  "Command:"
                  (concat "cd " (magit-toplevel) " && git ls-files  | grep -i ")
                  " | xargs ls -1"))
        (result nil))
    (al-live :title (plist-get command :title)
             :on-init (lambda ()
                        (goto-char (plist-get command :position))
                        (local-set-key (kbd "C-n") (lambda () (interactive)
                                                     (with-selected-window (get-buffer-window "*git-ls-files*")
                                                       (next-line 1)
                                                       (find-file-other-window (ffap-guess-file-name-at-point))
                                                       (setq result (current-buffer)))))
                        (local-set-key (kbd "C-p") (lambda () (interactive)
                                                     (with-selected-window  (get-buffer-window "*git-ls-files*")
                                                       (previous-line 1)
                                                       (find-file-other-window (ffap-guess-file-name-at-point))
                                                       (setq result (current-buffer))))))
             :initial-input  (plist-get command :command)
             :fun (lambda (contents)
                    (async-shell-command contents "*git-ls-files*"))
             :on-complete (lambda ()
                            (when result
                                (switch-to-buffer result))))))

(provide 'al-live)
