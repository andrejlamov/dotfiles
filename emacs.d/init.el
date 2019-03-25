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
      inhibit-splash-screen t
      js-indent-level 4
      make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      whitespace-style '(face trailing tabs trailing-whitespace newline newline-mark))

(add-to-list 'exec-path "~/.bin")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode)
(winner-mode)
(dirtrack-mode)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(global-whitespace-mode)
(setq ido-default-buffer-method 'selected-window)
(require 'tramp)
(tramp-change-syntax 'default)

;; Disabling prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(defvar file-name-handler-alist-old file-name-handler-alist)

(defvar bootstrap-version)
(let ((bootstrap-version 5)
      (bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
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

(use-package dash-functional)
(use-package dash)

(defun almacs/reload ()
  (interactive)
  (load-file "~/.emacs.d/packages.el")
  (load-file "~/.emacs.d/funs.el")
  (load-file "~/.emacs.d/keys.el")
  (load-file "~/.emacs.d/theme.el")
  (almacs/load-el-directory "~/.emacs.d/modules/"))

(add-hook 'after-init-hook
          (lambda ()
           (almacs/reload)
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))
;; end
