(show-paren-mode)
(winner-mode)
(dirtrack-mode)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)

(setq
 js-indent-level 4
 make-backup-files nil
 create-lockfiles nil
 auto-save-default nil)

(global-whitespace-mode)

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
  (shell buffer-name))

(defun almacs/load-el-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun almacs/go-to-core-el ()
  (interactive)
  (find-file "~/.emacs.d/core.el"))
