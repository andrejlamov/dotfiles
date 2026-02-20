;; TODO: do something with find-grep-dired (make it use git ls-files)
(require 'cl-lib)
(require 'dired-x)
(require 's)

(cl-defun al-live/construct(&key (title "")
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

(cl-defun al-live/construct-find-command (&key (command nil) (buffer-name nil))
  (let ((result nil))
    (al-live/construct :title (plist-get command :title)
             :on-init(lambda ()
                       (goto-char (plist-get command :position))
                       (local-set-key (kbd "C-n") (lambda ()
                                                    (interactive)
                                                    (with-selected-window (get-buffer-window buffer-name)
                                                      (next-line 1)
                                                      (find-file-other-window (ffap-guess-file-name-at-point))
                                                      (setq result (current-buffer)))))
                       (local-set-key (kbd "C-p") (lambda ()
                                                    (interactive)
                                                    (with-selected-window (get-buffer-window buffer-name)
                                                      (next-line -1)
                                                      (find-file-other-window (ffap-guess-file-name-at-point))
                                                      (setq result (current-buffer))))))
             :initial-input  (plist-get command :command)
             :fun (lambda (contents)
                    (async-shell-command contents buffer-name))
             :on-complete (lambda ()
                            (when result
                              (switch-to-buffer result))))))

;;;###autoload
(defun al-live/find ()
  (interactive)
  (al-live/construct-find-command
   :command (al-live/command-with-cursor-position "Commmand:" "find . | grep -i " " | xargs ls -1")
   :buffer-name "*find*"))

;;;###autoload
(defun al-live/git-ls-files ()
  (interactive)
  (al-live/construct-find-command
   :command (al-live/command-with-cursor-position "Commmand:"
                                                  (concat  "cd " (vc-root-dir)  " && git ls-files | grep -i ")
                                                  " | xargs ls -1")
   :buffer-name "*git-ls-files*"))

;;;###autoload
(defun al-live/occur ()
  (interactive)
  (al-live/construct :title "Occur: "
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
    (al-live/construct :title "Grep: "
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
    (al-live/construct :title "Gitgrep: "
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
    (al-live/construct :title "Grep: "
             :on-init  (lambda ()
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



(provide 'al-live)

