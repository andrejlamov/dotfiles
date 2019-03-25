(defun almacs/delete-current-buffer-file ()
  (interactive)
  (delete-file (buffer-file-name))
  (kill-buffer))

(defun almacs/rename-current-file (new-file-name)
  (interactive "Fnew name:")
  (let* ((old-file-name (buffer-file-name))
         (old-buffer-name (current-buffer)))
    (rename-file old-file-name new-file-name t)
    (kill-buffer old-buffer-name)
    (find-file new-file-name)))

(defun almacs/named-shell (buffer-name)
  (interactive "Bname: ")
  (shell (concat "*" buffer-name "*")))

(defun almacs/load-el-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun almacs/go-to-funs-el ()
  (interactive)
  (find-file "~/.emacs.d/funs.el"))

(defun almacs/go-to-packages-el ()
  (interactive)
  (find-file "~/.emacs.d/packages.el"))

(defun almacs/go-to-keys-el ()
  (interactive)
  (find-file "~/.emacs.d/keys.el"))

(defun almacs/set-font-global-size (size)
  (interactive "sSet font size: ")
  (set-face-attribute 'default nil :height (string-to-number size)))

(defun almacs/eval-sexp ()
  "Eval expression on point in normal mode."
  (interactive)
  (let ((eval-fun (lambda ()
                    (evil-append 0)
                    (pcase major-mode
                               ('emacs-lisp-mode (call-interactively 'eval-last-sexp))
                               ('clojurec-mode (call-interactively 'cider-pprint-eval-last-sexp))
                               ('clojurescript-mode (call-interactively 'cider-pprint-eval-last-sexp))
                               ('clojure-mode (call-interactively 'cider-pprint-eval-last-sexp))
                               (mode (message "Not supported lisp mode %S" mode)))
                    (evil-force-normal-state))))
    (if (member (string (char-after)) '("(" "[" "{"))
        (progn
          (evil-jump-item)
          (eval (list eval-fun))
          (evil-jump-item))
      (eval (list eval-fun)))))

(defun almacs/helm-ls-git-word-at-point ()
  (interactive)
  (helm :sources helm-ls-git-default-sources
        :ff-transformer-show-only-basename nil
        :buffer "*helm lsgit*"
        :input (thing-at-point 'word 'no-properties)))

(defun almacs/bash-term ()
  (interactive)
  (ansi-term "/bin/bash")
  (rename-buffer "term" t))

(defun almacs/x-screensaver ()
  (interactive)
  (async-shell-command "xscreensaver-command -lock" nil nil))

(defun almacs/non-asterisk-other-hidden-buffers ()
  (let ((other-visible-buffers (->> (window-list)
                                   (-map (-compose 'buffer-name 'window-buffer))
                                   (-remove-item (buffer-name)))))
    (->> (buffer-list)
         (-map 'buffer-name)
         (-filter (lambda (b) (not (-contains? other-visible-buffers b))))
         (-filter (lambda (b) (not (s-matches? "\*.*\*" (s-trim b)))))
         (-sort (lambda (a b) (s-less? (s-lower-camel-case a)
                                       (s-lower-camel-case b)))))))

(defun almacs/-switch-buffer (next-idx-fn)
  (let* ((buffers (almacs/non-asterisk-other-hidden-buffers))
         (max-idx (1- (length buffers)))
         (current-idx (-elem-index (buffer-name) buffers)))
    (if current-idx
        (let ((next-idx (funcall next-idx-fn current-idx max-idx)))
          (switch-to-buffer (nth next-idx buffers) t t))
      (switch-to-buffer (car buffers) t t))))

(defun almacs/prev-buffer ()
  (interactive)
  (almacs/-switch-buffer (lambda (current-idx max-idx)
                          (cond
                           ((eq current-idx 0) max-idx)
                           (t (1- current-idx))))))


(defun almacs/next-buffer ()
  (interactive)
  (almacs/-switch-buffer (lambda (current-idx max-id)
                           (cond
                            ((eq current-idx max-idx) 0)
                            (t (1+ current-idx))))))

(defun almacs/switch-buffer-by-first-char (char)
  (interactive "c")
  (when-let ((b (->> (buffer-list)
                     (-map 'buffer-name)
                     (-filter (lambda (b) (not (s-equals? (buffer-name) (s-trim b)))))
                     (-sort 's-less?)
                     (-find (lambda (b) (s-starts-with? (char-to-string char) (s-trim b) t))))))
    (switch-to-buffer b t t)))
