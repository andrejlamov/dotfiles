;; -*- lexical-binding: t -*-

'(use-package no-littering
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
  (require 'dired-x)

  ;;isearch
  (setq isearch-regexp-lax-whitespace t)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (add-hook 'prog-mode-hook (lambda ()
                              (display-line-numbers-mode)
                              (setq display-line-numbers-type t)))
  ;; (visual-line-mode)
  ;; (add-hook 'prog-mode-hook 'visual-line-mode)

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
  (global-set-key (kbd "C-w") 'al/backward-kill-or-kill-region))

(use-package icomplete
  :demand t
  :config
  (fido-mode 1)
  (fido-vertical-mode 1)
  (setq completion-auto-help 'visible)
  (setq completion-show-help nil)
  (setq completion-auto-select 'second-tab)
  (setq icomplete-in-buffer t
        tab-always-indent 'complete
        completion-auto-help t
        icomplete-show-matches-on-no-input t))


(use-package magit
  :ensure t)

(use-package avy
  :ensure t
  :bind (("M-q" . avy-goto-char)
         :map isearch-mode-map
         ("M-q" . avy-isearch)))

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

(use-package al-search
  :load-path "../lisp/"
  :commands al/live-git-ls-files al/live-grep al/live-occur
  )
