;; Goal of this module:
;; - live update "output" buffer triggered by al-live wrapped command
;; - for some outputs, do somethhing in the line change
;; - easy jump back to continue any search
;; - for ls/find output do a custom miniro mode that previews file on line change

;; TODO: do something with find-grep-dired (make it use git ls-files)
(require 'cl-lib)
(require 'dired-x)
(require 'subr-x)
(require 's)
(require 'al-debouncer)

(cl-defun al-live/construct(&key (title "")
                                 (fun (lambda ()))
                                 (initial-input nil)
                                 (on-complete (lambda ()))
                                 (on-init (lambda ())))
  (let* ((my-buffer (window-buffer (minibuffer-selected-window)))
         (on-keypress (lambda (beg end length)
                        (al-debouncer/create :name 'al-live
                                             :time 0.2
                                             :fn (lambda ()
                                                   (let ((contents (minibuffer-contents)))
                                                     (with-current-buffer my-buffer
                                                       (funcall fun contents))))))))
    (minibuffer-with-setup-hook
        (lambda ()
          (funcall on-init)
          (add-hook 'after-change-functions on-keypress nil t))
      (let ((res (read-string title initial-input)))
        (when res
          (funcall on-complete))))))

(defun al-live/virtual-dired ()
  "Try to parse the first line as root-dir, if its nil then virtual-dired will prompt for it."
  (interactive)
  (let* ((first-line (save-excursion
                       (goto-line 0)
                       (buffer-substring-no-properties
                        (progn (beginning-of-line) (point))
                        (progn (end-of-line) (point)))))
         (path (progn
                 (string-match "^\\s-*\\(.+\\):" first-line)
                 (match-string 1 first-line))))
    (if (and path (file-directory-p path))
        (virtual-dired path)
      (call-interactively 'virtual-dired))))

(defun al-live/command-with-cursor-position (title command-prefix command-postfix)
  (list
   :position (+ (length title) (length command-prefix) 1)
   :title title
   :command (concat command-prefix command-postfix)))


;; (defun al-live/enable-find-mode ())
;; (defun al-live/disable-find-mode ())

;; (define-minor-mode al-live/find-mode
;;   "al-live/find-mode"
;;   :lighter " al-live/find"
;;   (if al-live-find-mode
;;       (progn
;; 	(add-hook 'post-command-hook #'al-live/enable-find-mode nil t))
;;     (remove-hook 'post-command-hook #'al-live/disable-find-mode t)))


(cl-defun al-live/construct-find-command (&key (command nil) (buffer-name nil))
  (let ((path nil))
    (al-live/construct :title (plist-get command :title)
                       :on-init (lambda ()
                                  (goto-char (plist-get command :position)))
                       :initial-input (plist-get command :command)
                       :fun (lambda (contents)
                              (async-shell-command contents buffer-name))
                       :on-complete (lambda ()
                                      (switch-to-buffer-other-window buffer-name)
                                      (al-live/virtual-dired)))))

(cl-defun al-live/construct-grep-command (&key (command nil) (buffer-name nil))
  (let ((path nil))
    (al-live/construct :title (plist-get command :title)
                       :on-init (lambda ()
                                  (goto-char (plist-get command :position)))
                       :initial-input (plist-get command :command)
                       :fun (lambda (contents)
                              (async-shell-command contents buffer-name))
                       :on-complete (lambda ()
                                      (switch-to-buffer-other-window buffer-name)))))


;;;###autoload
(defun al-live/find ()
  (interactive)
  (al-live/construct-find-command
   :command (al-live/command-with-cursor-position
             "Commmand:"
             (concat
              "echo " (string-trim (shell-command-to-string "pwd")) ": ;"
              " find . | grep -i ")
             " | xargs ls -lR")
   :buffer-name "*find*"))

;;;###autoload
(defun al-live/git-ls-files ()
  (interactive)
  (ignore-errors (kill-buffer "*git-ls-files*"))
  (al-live/construct-find-command
   :command (al-live/command-with-cursor-position
             "Command: "
             (concat
              "cd " (or (string-trim (shell-command-to-string "git rev-parse --show-toplevel")) ".")
              " &&  echo $(pwd): && git ls-files | grep -i ")
             " | xargs ls -lR")
   :buffer-name "*git-ls-files*"))

;;;###autoload
(defun al-live/occur ()
  (interactive)
  (al-live/construct :title "Occur: "
                     :on-init (lambda ()
                                (ignore-errors
                                  (kill-buffer "*Occur")))
                     :fun (lambda (contents)
                            (occur contents))
                     :on-complete (lambda ()
                                    (switch-to-buffer-other-window "*Occur*")
                                    (occur-next))))

;;;###autoload
(defun al-live/grep ()
  (interactive)
  (let ((confirm-kill-processes nil))
    (al-live/construct :title "Grep: "
                       :on-init (lambda ()
                                  (add-hook 'grep-mode-hook (lambda () (setq-local compilation-always-kill t))))
                       :on-complete (lambda ()
                                      (switch-to-buffer-other-window "*grep*")
                                      (next-error-no-select))
                       :initial-input "grep --color=auto -nHi --null -e "
                       :fun (lambda (contents)
                              (grep contents)))))

;;;###autoload
(defun al-live/git-grep ()
  (interactive)
  (let ((confirm-kill-processes nil))
    (al-live/construct :title "Grep: "
             :on-init (lambda ()
                        (goto-char (point-max)))
             :initial-input "git grep -in "
             :on-complete (lambda ()
                            (switch-to-buffer-other-window "*grep*")
                            (next-error-no-select))
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
                        (add-hook 'grep-mode-hook (lambda () (setq-local compilation-always-kill t))))
             :initial-input  (concat "grep --color=auto -nH --null -e " marked-files)
             :fun (lambda (contents)
                    (grep contents)))))



(provide 'al-live)



