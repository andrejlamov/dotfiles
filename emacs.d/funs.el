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

(defun almacs/go-to-base-packages-el ()
  (interactive)
  (find-file "~/.emacs.d/base-packages.el"))

(defun almacs/go-to-keys-el ()
  (interactive)
  (find-file "~/.emacs.d/keys.el"))

(defun almacs/go-to-modules-el ()
  (interactive)
  (helm-find-files-1 "~/.emacs.d/modules/"))

(defun almacs/set-font-global-size (size)
  (interactive "sSet font size: ")
  (set-face-attribute 'default nil :height (string-to-number size)))

(defun almacs/eval-sexp ()
  "Eval expression on point in normal mode."
  (interactive)
  (let ((eval-fun (lambda ()
                    (save-excursion
                      (goto-char (1+ (point)))
                      (pcase major-mode
                        ('hy-mode (call-interactively 'hy-shell-eval-last-sexp))
                        ('emacs-lisp-mode (call-interactively 'eval-last-sexp))
                        ('clojurec-mode (call-interactively 'cider-pprint-eval-last-sexp))
                        ('clojurescript-mode (call-interactively 'cider-pprint-eval-last-sexp))
                        ('clojure-mode (call-interactively 'cider-pprint-eval-last-sexp))
                        (mode (message "Not supported lisp mode %S" mode)))))))
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

(defun almacs/helm-occur ()
  (interactive)
  (let  ((c (face-attribute 'region :background)))
    (unwind-protect
        (set-face-attribute 'region nil :background nil)
      (helm-occur)
      (set-face-attribute 'region nil :background c))))

(defun almacs/revert-buffer ()
  (interactive)
  (revert-buffer nil t t))
(defun spacecat (&rest terms)
  (s-join " " terms))
(defun almacs/winner-undo ()
  (interactive)
  (writeroom--disable)
  (call-interactively 'winner-undo))

(defun almacs/writeroom-enable ()
  (interactive)
  (writeroom--enable))

(defun almacs/reload-dir-locals-for-current-buffer ()
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun almacs/reload-dir-locals-for-all-buffer-in-this-directory ()
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (almacs/reload-dir-locals-for-current-buffer)))))
