(setq package-enable-at-startup nil
      inhibit-splash-screen t
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 64000000
      gc-cons-percentage 1
      auto-window-vscroll nil
      backup-inhibited t
      mae-backup-files nil
      auto-save-default nil)

(defvar file-name-handler-alist-old file-name-handler-alist)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(defun almacs/core-configure ()
  (show-paren-mode)
  (winner-mode)
  (dirtrack-mode)

  (setq
   make-backup-files nil
   auto-save-default nil
   whitespace-style '(face trailing))

  (global-whitespace-mode)

  (load-file "~/.emacs.d/core.el"))

(defun almacs/module-configure ()

  (predd-defmulti almacs/after-save #'identity)
  (predd-defmethod almacs/after-save :default (mode) nil)

  (predd-defmulti almacs/major-mode-change #'identity)
  (predd-defmethod almacs/major-mode-change :default (mode) nil)

  (add-hook
   'after-save-hook
   (lambda ()
     (almacs/after-save major-mode)))

  (add-hook
   'switch-buffer-functions
   (lambda (prev-buf curr-buf)
     (when (not (equal prev-buf curr-buf))
       (general-unbind '(normal) ",")
       (almacs/major-mode-change major-mode))))

  (almacs/load-el-directory "~/.emacs.d/modules/"))

(defun almacs/reconfigure ()
  (interactive)
  (almacs/core-configure)
  (almacs/module-configure))

(add-hook 'after-init-hook
          (lambda ()
	     (setq inhibit-splash-screen t)
	     (menu-bar-mode -1)
	     (tool-bar-mode -1)
	     (scroll-bar-mode -1)

	     (almacs/reconfigure)

	     (setq file-name-handler-alist file-name-handler-alist-old
		   gc-cons-threshold 800000
		   gc-cons-percentage 0.1)
	     (garbage-collect)))
;; end
