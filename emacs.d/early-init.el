'(custom-set-faces

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
      whitespace-style '(face trailing tabs trailing-whitespace newline newline-mark)
      val-expression-print-length nil
      eval-expression-print-level nil)

(menu-bar-mode -1)
(show-paren-mode)
(winner-mode)
(dirtrack-mode)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq ido-default-buffer-method 'selected-window)
(require 'tramp)
(tramp-change-syntax 'default)
(auto-compression-mode 1)
(setq vc-handled-backends nil)
(blink-cursor-mode 0)
(global-auto-revert-mode 1)
(add-hook 'prog-mode-hook 'whitespace-mode)

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
(straight-use-package 'org)
(straight-use-package 'org-plus-contrib)
(straight-use-package 'use-package)
