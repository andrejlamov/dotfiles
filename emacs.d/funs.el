;; -*- lexical-binding: t -*-

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
  (helm-find-files-1 "~/.emacs.d/lisp/"))

(defun almacs/set-font-global-size (size)
  (interactive "sSet font size: ")
  (set-face-attribute 'default nil :height (string-to-number size)))

(defun almacs/eval-enclosed-sexp (eval-fn)
  "Eval enclosed sexp with EVAL-FN when in normal mode at some bracket."
  (interactive)
  (let ((normal-eval-fn (lambda ()
                          (save-excursion
                            (goto-char (1+ (point)))
                            (call-interactively eval-fn)))))
    (if (member (string (char-after)) '("(" "[" "{"))
        (progn
          (evil-jump-item)
          (eval (list normal-eval-fn))
          (evil-jump-item))
      (eval (list normal-eval-fn)))))

(defun almacs/helm-ls-git-word-at-point ()
  (interactive)
  (helm :sources helm-ls-git-default-sources
        :ff-transformer-show-only-basename nil
        :buffer "*helm lsgit*"
        :input (thing-at-point 'word 'no-properties)))

(defun almacs/xfce4-term ()
  (interactive)
  (start-process-shell-command "xfce4-terminal" nil "xfce4-terminal"))

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

(defun almacs/uuid ()
  (interactive)
  (s-trim (shell-command-to-string "uuidgen")))

(defmacro almacs/define-key (state keymap &rest bindings)
  (-let ((keymaps `(if (symbolp ,keymap)
                       (list ,keymap) ,keymap))

         (bindings1 `(->> ',bindings
                          (-partition 2)
                          (-reduce-from (-lambda ((acc &as x . xs) (binding &as k v))
                                          (if (stringp k)
                                              (cons (list binding) acc)
                                            (cons (cons binding x) xs)))
                                        nil)
                          (-map 'reverse)
                          (reverse))))
    `(-each ,keymaps
       (lambda (km)
         (-each ,bindings1
           (-lambda (((key def) . options))
             (cond
              ((or
                (and (symbolp def) (fboundp def))
                (eq 'lambda (car def))
                (eq nil def))
               (evil-define-key* ,state (symbol-value km) key def))
              (t (evil-define-key* ,state (symbol-value km) key `(lambda () (interactive) ,def))))
             (-each options
               (-lambda ((opt v))
                 (cond
                  ((eq opt :wk)
                   (progn (which-key-add-major-mode-key-based-replacements
                            (intern (s-chop-suffix "-map" (symbol-name km)))
                            key v)))
                  (t nil))))))))))

(defun almacs/helm-occur-from-region ()
  (interactive)
  (helm-multi-occur-1
   (list (current-buffer))
   (buffer-substring-no-properties
    (region-beginning)
    (region-end))))

(defun almacs/motion-escape ()
  (interactive)
  (evil-mc-undo-all-cursors))

(defun almacs/avy-cp-backward-up ()
  (interactive)
  (call-interactively 'avy-goto-char-timer)
  (call-interactively 'evil-cp-backward-up-sexp))
