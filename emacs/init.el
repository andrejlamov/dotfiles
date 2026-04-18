;;; -*- lexical-binding: t -*-
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (expand-file-name "../lisp"))

(use-package s)
(use-package dash)

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))



(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (global-auto-revert-mode t)

  ;; meta space
  (defvar al/meta-spc-map (make-sparse-keymap))
  (global-set-key (kbd "M-SPC") al/meta-spc-map)

  ;; grep
  (setq grep-save-buffers t)


  ;; registers
  (define-key al/meta-spc-map "R" 'point-to-register)
  (define-key al/meta-spc-map "r" 'jump-to-register)
  (advice-add 'jump-to-register :after (lambda (&rest r)
                                         (when (eq major-mode 'shell-mode)
                                           (end-of-buffer))))
  (setq register-preview-delay 0)

  ;; theme
  (load-theme 'modus-operandi-tinted t nil)
  (if (equal (system-name) "void")
      (set-face-attribute 'default nil :height 80)
    (set-face-attribute 'default nil :height 120))

  ;; (defun my/apply-monochrome-bold-syntax ()
  ;;   "Strip colors from font-lock and use typography instead."
  ;;   (interactive)
  ;;   (let ((fg (face-attribute 'default :foreground)))
  ;;     ;; Reset common faces to default foreground
  ;;     (dolist (face '(font-lock-variable-name-face
  ;;                     font-lock-function-name-face
  ;;                     font-lock-type-face
  ;;                     font-lock-constant-face
  ;;                     font-lock-string-face
  ;;                     font-lock-warning-face))
  ;;       (set-face-attribute face nil :foreground fg :weight 'normal :slant 'normal))

  ;;     ;; Set specific typographic rules
  ;;     (set-face-attribute 'font-lock-keyword-face nil :foreground fg :weight 'bold)
  ;;     (set-face-attribute 'font-lock-builtin-face nil :foreground fg :weight 'bold)
  ;;     (set-face-attribute 'font-lock-string-face nil :foreground fg :weight 'bold)
  ;;     (set-face-attribute 'font-lock-comment-face nil :foreground fg :slant 'italic)))

  ;; ;; Run it now:
  ;; (my/apply-monochrome-bold-syntax)

  (global-set-key (kbd "M-q") 'fill-paragraph)
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice t)
  (setq user-emacs-directory (expand-file-name "~/.config/emacs/.emacs.d"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq comint-completion-addsuffix nil)
  (setq use-short-answers t)


  ;; shell
  (defun al/named-shell (name)
    (interactive "sName:")
    (shell (s-concat name)))
  (define-key al/meta-spc-map (kbd "a s") 'al/named-shell)
  (add-hook 'shell-mode-hook
            (lambda ()
              (setenv "PAGER" "cat")))
  ;; highiligh line
  (setq-default cursor-in-non-selected-windows nil)
  (add-hook 'prog-mode-hook 'hl-line-mode)

  ;; cursor
  (blink-cursor-mode -1)

  ;; default to truncate long lines
  (setq-default truncate-lines nil)
  (global-visual-line-mode 1)
  (setq visual-wrap-extra-indent 2)

  (global-visual-wrap-prefix-mode 1)
  ;; remove truncate dollar sign
  (set-display-table-slot standard-display-table 'truncation ?\ )




  ;; just stop
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  ;; align
  (setq align-region-separate 'entire)

  ;;isearch
  (setq isearch-regexp-lax-whitespace t)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)

  (global-set-key (kbd "C-M-v") 'scroll-other-window-down)
  (global-set-key (kbd "C-M-S-v") 'scroll-other-window)

  (add-hook 'prog-mode-hook (lambda ()
                              (display-line-numbers-mode)
                              (setq display-line-numbers-type 'relative)))

  ;; compile mode
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)

  (define-key project-prefix-map (kbd "C") 'project-recompile)

  ;; js default
  (setq js-indent-level 2
        js-jsx-indent-level 2)

  (global-set-key (kbd "C-z") 'repeat)
  (global-set-key (kbd "C-x C-f") 'find-file)
  (global-set-key (kbd "C-x C-S-f") 'ffap)

  (defun al/backward-kill-or-kill-region ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'kill-region)
      (call-interactively 'backward-kill-word)))
  (global-set-key (kbd "C-w") 'al/backward-kill-or-kill-region)

  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (setq context-menu-mode t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)))


(use-package winner
  :ensure nil
  :init
  (winner-mode 1)
  :config
  (define-key al/meta-spc-map (kbd "w u") 'winner-undo)
  (define-key al/meta-spc-map (kbd "w r") 'winner-redo))

(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20)        ;; Show more candidates
  (vertico-resize nil)      ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  (keymap-set vertico-map "C-:" #'vertico-quick-exit)
  (keymap-set vertico-map "C-;" #'vertico-quick-insert))

(use-package marginalia
  :config
  (marginalia-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))


(use-package paredit
  :hook emacs-lisp-mode
  :bind (:map paredit-mode-map
              ("M-q" . nil)
              ("M-s" . nil)))

(use-package magit
  :ensure t
  :config
  (setq magit-diff-refine-hunk 'all))

(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-2)
   ("C-;" . avy-goto-char)
   ("C-M-;" . avy-goto-char-in-line)
   :map isearch-mode-map
   ("C-:" . avy-isearch)))

(use-package whitespace
  :init (add-hook 'prog-mode-hook 'whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark missing-newline-at-eof)))

(use-package gptel
  :ensure t
  :config
  (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t)
  (gptel-make-gh-copilot "Copilot")
  (setq gptel-log-level 'debug))

(use-package markdown-mode
  :ensure t)

(use-package org
  :ensure t
  :config
  (setq org-confirm-babel-evaluate nil))

(use-package which-key
  :init
  (setq which-key-idle-delay 0)
  (which-key-mode))

(use-package al-live
  :load-path "../lisp/"
  :bind (("M-s G" . al-live/grep)
         ("M-s g" . al-live/git-grep)
         ("M-s o" . al-live/occur)
         ("M-s f" . al-live/find)
         ("M-s l" . al-live/git-ls-files))
  :config
  (define-key occur-mode-map (kbd "C-c C-p") 'occur-edit-mode)
  (setq list-matching-lines-default-context-lines 0))

(use-package al-debouncer
  :load-path "../lisp/")

(use-package eglot
  :ensure t)

(use-package bufler
  :init (bufler-mode)
  :bind (("C-x b" . bufler-switch-buffer)
         ("C-x B" . bufler-list)))

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (hl-line-mode -1))))

(use-package yaml-mode)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-background nil)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (setq aw-display-mode-overlay nil)
  (ace-window-display-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :config

  (defun my-corfu-enable-in-minibuffer ()
    (setq-local corfu-auto t
                corfu-echo-delay nil
                corfu-popupinfo-delay nil)
    (corfu-mode 1))
  (add-hook 'minibuffer-setup-hook #'my-corfu-enable-in-minibuffer))

(use-package docker)


;; TODO: create cursors from phi-search matches
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines) ;; TODO: breaks with relative line number mode?
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (setq mc/always-run-for-all t)
  (require 'phi-replace)
  (defun al/use-phi-search ()
    (if multiple-cursors-mode (progn
                                (message "enabled")
                                (global-set-key (kbd "C-s") 'phi-search)
                                (global-set-key (kbd "C-r") 'phi-search-backward)
                                (global-set-key (kbd "M-%") 'phi-replace-query))

      (message "disabled")
      (global-set-key (kbd "C-s") 'isearch-forward-regexp)
      (global-set-key (kbd "C-r") 'isearch-backward-regexp)
      (global-set-key (kbd "M-%") 'query-replace)))
  (add-hook 'multiple-cursors-mode-hook 'al/use-phi-search))

(use-package phi-search)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))


(use-package crux)

(use-package yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
              ("C-c y" . yas-expand)         ("C-c & C-n" . yas-next-field)
              ("C-c & C-p" . yas-prev-field))
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

(use-package yasnippet-snippets
  :ensure t)


(use-package al-typo
  :load-path "../lisp"
  :commands typo)

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package dired
  :ensure nil
  :config
  (when (equal window-system 'mac)
    (setq insert-directory-program "gls"))
  (setq dired-listing-switches "-alh")
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode))
