(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-custom-known-commands '((sp-raise-sexp . ((:default . evil-mc-execute-default-call-with-count)))))
  (custom-set-faces
   '(evil-mc-cursor-default-face ((t (:inherit cursor :background "tan" :inverse-video nil))))))

(use-package dash-functional)

(use-package general
  :config
  (general-evil-setup))

(use-package scala-mode)

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
        helm-ff-auto-update-initial-value t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
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
  :config
  (require 'evil-cleverparens-text-objects)
  (setq evil-cleverparens-use-additional-bindings t
        evil-cleverparens-use-additional-movement-keys nil)
  (defalias 'evil-cp-insert 'evil-insert)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'hy-mode-hook #'evil-cleverparens-mode))

(use-package clojure-mode)

(use-package which-key
  :init
  (setq which-key-idle-delay 1
        which-key-add-column-padding 0)
  :config
  (which-key-mode))

(use-package switch-buffer-functions)

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
  (purpose-mode)
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . cljs))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . cljr))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . cljc))
  (add-to-list 'purpose-user-regexp-purposes '("^\\*cider-repl .*(cljs:figwheel)\\*" . cljs-repl))
  (add-to-list 'purpose-user-regexp-purposes '("^\\*cider-repl .*(clj)\\*" . clj-repl))
  (add-to-list 'purpose-user-name-purposes '("*cider-test-report*" . cider-test-report))
  (add-to-list 'purpose-user-name-purposes '("*cider-result*" . cider-result-report))
  (purpose-compile-user-configuration))

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

(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (setcar (memq 'source-inplace (flycheck-checker-get 'typescript-tslint 'command))
          'source-original)

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; formats the buffer before saving
  ;;(add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

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


(use-package focus)

(use-package writeroom-mode)

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
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package edbi)
