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

(defun almacs/go-to-core-el ()
  (interactive)
  (find-file "~/.emacs.d/core.el"))

(defun almacs/go-to-packages-el ()
  (interactive)
  (find-file "~/.emacs.d/packages.el"))

(defun almacs/go-to-keys-el ()
  (interactive)
  (find-file "~/.emacs.d/keys.el"))

(defun almacs/set-font-global-size (size)
  (interactive "sSet font size: ")
  (set-face-attribute 'default nil :height (string-to-number size)))
