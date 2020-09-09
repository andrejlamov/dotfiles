(custom-set-faces
 '(mode-line ((t (:background "black" :foreground "white"))))
 '(whitespace-tab ((t (:background "grey90"))))
 '(region ((t (:background "#eee8d5")))))

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
'(add-hook 'prog-mode-hook 'whitespace-mode)

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

(use-package dash-functional)
(use-package dash)
(use-package s)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-move-beyond-eol t)
  :config
  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package evil-little-word
  :straight (evil-little-word :type git :host github :repo "tarao/evil-plugins" :files ("evil-little-word.el")))
(use-package dash-functional)

(use-package general
  :config
  (general-evil-setup))

(use-package scala-mode)

(use-package helm-ag)

(use-package helm
  :config
  (require 'helm-config)
  (helm-top-poll-mode)
  (define-key shell-mode-map  (kbd "M-p") 'helm-comint-input-ring)
  (define-key helm-map (kbd "M-w") 'helm-yank-text-at-point)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)

  (helm-autoresize-mode 1)
  (setq helm-autoresize-min-height 40
        helm-ff-auto-update-initial-value nil
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-split-window-inside-p t
        helm-locate-command "locate %s -e -A --regex %s | grep -v \"^$HOME/\\\..*\"")
  (helm-mode 1)
  (defvar almacs/helm-window-height 0.3)
  (setq helm-display-function (lambda (buffer &optional _resume)
                                (let ((window (or (purpose-display-reuse-window-buffer buffer nil)
                                                  (purpose-display-reuse-window-purpose buffer nil)
                                                  (purpose-display-at-bottom buffer nil almacs/helm-window-height))))
                                  (if window
                                      (progn
                                        (select-window window)
                                        (switch-to-buffer buffer t t))
                                    (funcall #'helm-default-display-buffer buffer)))))

  (defun almacs/kill-hggrep-buffer (&rest args)
    (ignore-errors
      (kill-buffer "*hggrep*")))

  (advice-add 'helm-git-grep-save-results :before #'almacs/kill-hggrep-buffer)

  (setq helm-source-buffers-list
        (helm-make-source "Buffers" 'helm-source-buffers))

  (helm-add-action-to-source "Switch to buffer (same window)"
                             'almacs/switch-to-buffer
                             helm-source-buffers-list 0))

(use-package avy)

(use-package company :config (global-company-mode))

(use-package smartparens
  :config
  ;; https://github.com/Fuco1/smartparens/issues/908
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil))

(use-package paredit)

(use-package evil-cleverparens
  :straight (evil-cleverparens :type git :host github :repo "andrejlamov/evil-cleverparens")
  :commands evil-cleverparens-mode
  :init
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'hy-mode-hook #'evil-cleverparens-mode)
  :config
  (require 'evil-cleverparens-text-objects)
  (setq evil-cleverparens-use-additional-bindings t
        evil-cleverparens-use-additional-movement-keys t)
  (almacs/define-key 'normal '(evil-cleverparens-mode-map)
                     "\M-H" almacs/avy-cp-backward-up
                     "\M-r" nil
                     "\M-r(" sp-splice-sexp-killing-backward
                     "\M-rr" sp-raise-sexp
                     "\M-r\M-r" sp-raise-sexp
                     "\M-r)" sp-splice-sexp-killing-forward))

(use-package clojure-mode)

(use-package which-key
  :init
  (setq which-key-idle-delay 1
        which-key-add-column-padding 0)
  :config
  (which-key-mode))

(use-package predd
  :straight (predd :type git :host github :repo "skeeto/predd" :files ("predd.el")))

(use-package evil-visualstar
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

(use-package expand-region
  :config
  (setq expand-region-fast-keys-enabled t
        expand-region-contract-fast-key "V"))

(use-package xclip
  :config
  (xclip-mode))

(use-package window-purpose
  :config
  (purpose-mode))

(use-package ace-jump-helm-line
  :config
  (setq ace-jump-helm-line-default-action 'select)
  (define-key helm-map (kbd "C-j") 'ace-jump-helm-line))

(use-package popwin
  :straight (popwin :type git :host github :repo "bmag/popwin-el")
  :config (popwin-mode 1))

(use-package ace-window
  :config
  (setq aw-keys '(?h ?j ?k ?l)))

(use-package web-mode)

(use-package bash-completion
  :config
  (bash-completion-setup)
  (setq eshell-default-completion-function 'eshell-bash-completion)

  (defun eshell-bash-completion ()
    (while (pcomplete-here
            (nth 2 (bash-completion-dynamic-complete-nocomint
                    (save-excursion (eshell-bol) (point))
                    (point)))))))

(use-package flycheck
  :config (add-hook 'sh-mode-hook 'flycheck-mode))

(use-package hydra)

(use-package tile)

(use-package highlight-parentheses
  :config (global-highlight-parentheses-mode))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package writeroom-mode
  :config
  (setq
   writeroom-mode-line t
   writeroom-width 142))

(use-package yaml-mode)

(use-package helpful
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key)
  ("C-c C-d" . 'helpful-at-point)
  ("C-h F" . 'helpful-function)
  ("C-h C" . 'helpful-command))

(use-package hy-mode)

(use-package esup)

(use-package aggressive-indent
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package edbi)

(use-package adaptive-wrap
  :config
  (global-visual-line-mode)
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))

(use-package eyebrowse
  :config
  (eyebrowse-init)
  (eyebrowse-setup-evil-keys))

(use-package evil-magit
  :commands evil-magit-init)

(use-package helm-ls-git
  :commands helm-ls-git-ls helm-browse-project)

(use-package helm-git-grep
  :config
  (setq helm-git-grep-sources '(helm-git-grep-source)))

(use-package magit
  :commands magit-status magit-log magit-log-head
  :config
  (evil-magit-init)
  (setq magit-diff-refine-hunk 'all))

(load-file "~/.emacs.d/funs.el")
(load-file "~/.emacs.d/keys.el")
(almacs/load-el-directory "~/.emacs.d/lisp/")

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))
;; end
