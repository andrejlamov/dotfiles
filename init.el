(setq package-enable-at-startup nil
      inhibit-splash-screen t
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 64000000
      gc-cons-percentage 1
      auto-window-vscroll nil
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      inhibit-splash-screen t)

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

(add-hook 'after-init-hook
          (lambda ()
	    (menu-bar-mode -1)
	    (tool-bar-mode -1)
	    (scroll-bar-mode -1)

	    (load-file "~/.emacs.d/core.el")
	    (almacs/setup-modules)
	    (load-file "~/.emacs.d/theme.el")

	    (setq file-name-handler-alist file-name-handler-alist-old
		  gc-cons-threshold 800000
		  gc-cons-percentage 0.1)
	    (garbage-collect)))
;; end
