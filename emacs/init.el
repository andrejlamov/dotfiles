;; -*- lexical-binding: t -*-
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package no-littering
  :ensure t
  :config
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
  (make-directory dir t)
  (setq lock-file-name-transforms `((".*" ,dir t))))

  (require 'recentf)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

(use-package emacs
  :config
  (load-theme 'modus-vivendi-deuteranopia t nil)
  (add-to-list 'load-path (expand-file-name "../lisp"))

  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice t)
  (setq user-emacs-directory (expand-file-name "~/.config/emacs/.emacs.d"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq comint-completion-addsuffix nil)
  (setq use-short-answers t)
  (winner-mode 1)

  ;;dired
  (require 'dired-x)

  ;;isearch
  (setq isearch-regexp-lax-whitespace t)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)

  (global-set-key (kbd "C-M-v") 'scroll-other-window-down)
  (global-set-key (kbd "C-M-S-v") 'scroll-other-window)

  (add-hook 'prog-mode-hook (lambda ()
                              (display-line-numbers-mode)
                              (setq display-line-numbers-type t)))
  ;(visual-line-mode)
  ;(add-hook 'prog-mode-hook 'visual-line-mode)


  (defvar al/meta-spc-map (make-sparse-keymap))
  (global-set-key (kbd "M-SPC") al/meta-spc-map)
  (global-set-key (kbd "C-z") 'repeat)
  (global-set-key (kbd "C-x C-f") 'find-file)
  (global-set-key (kbd "C-x C-S-f") 'find-file-literally)


  (defun al/backward-kill-or-kill-region ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'kill-region)
      (call-interactively 'backward-kill-word)))
  (global-set-key (kbd "C-w") 'al/backward-kill-or-kill-region)

(fido-mode -1)
  (fido-vertical-mode 1)
  (defun al/icomplete-styles ()
    (setq-local completion-styles '(orderless)))
  (add-hook 'icomplete-minibuffer-setup-hook 'al/icomplete-styles)
  (define-key icomplete-fido-mode-map (kbd "TAB") #'icomplete-force-complete)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1))

(use-package orderless
  :ensure t
  :config
  (fido-mode)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))


(use-package paredit
  :hook emacs-lisp-mode
  :bind (:map paredit-mode-map
              ("M-q" . nil)
              ("M-s" . nil)))

(use-package magit
  :ensure t)

(use-package avy
  :ensure t
  :init
  (global-unset-key (kbd "M-q"))
  :bind (("M-q" . avy-goto-char)
         :map isearch-mode-map
         ("M-q" . avy-isearch)
         :map prog-mode-map
         ("M-q" . avy-goto-char)))

(use-package whitespace
  :init (add-hook 'prog-mode-hook 'whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark missing-newline-at-eof)))

(use-package gptel
  :ensure t
  :config
  (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t)
  (setq gptel-log-level 'debug))

(use-package markdown-mode
  :ensure t)

(use-package org
  :ensure t
  :config
  (setq org-confirm-babel-evaluate nil))

(use-package al-live
  :load-path "../lisp/"
  :bind (("M-s g" . al-live/grep)
         ("M-s p" . al-live/git-grep)
         ("M-s o" . al-live/occur)
         ("M-s f" . al-live/find)
         ("M-s l" . al-live/git-ls-files)))

(use-package eglot
  :ensure t)

(use-package company
  :hook ((prog-mode . company-mode)
         (shell-mode . company-mode)
         (text-mode . company-mode))
  :ensure t
  :config
  (setq company-idle-delay 0))

(use-package bufler
  :init (bufler-mode)
  :bind (("C-x b" . bufler-switch-buffer)))
