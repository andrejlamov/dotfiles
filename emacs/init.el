;; -*- lexical-binding: t -*-

(use-package emacs
  :config
  (load-theme 'modus-operandi-deuteranopia t nil)
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice t)
  (setq user-emacs-directory (expand-file-name "~/.config/emacs/.emacs.d"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq use-short-answers t)

  (display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  (visual-line-mode)
  (add-hook 'prog-mode-hook 'visual-line-mode)
  
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

(use-package icomplete
  :demand t
  :config
  (fido-vertical-mode)
  (fido-mode 1)
  (setq icomplete-in-buffer nil
        tab-always-indent 'complete
        completion-styles '(basic)
        completion-auto-help nil
        icomplete-show-matches-on-no-input t)

  ;; This complition-in-region-function is using the send completion style as
  ;; minibuffer, so that we get a consistent experience that looks good in terminal.
  (defun al/completion-in-region-function (start end collection &optional predicate)
           (let* ((initial (buffer-substring-no-properties start end))
                  (res (completing-read "Complete: " collection predicate t initial)))
             (when res
               (delete-region start end)
               (insert res)))
               t)
  ;; Completion-in-region is a variable holding the stratey of region-completion.
  (setq completion-in-region-function #'al/completion-in-region-function)
  (define-key icomplete-fido-mode-map (kbd "TAB") 'icomplete-fido-ret))

(use-package magit
  :ensure t)

(use-package winner
  :ensure t
  :config
  (winner-mode))

(use-package avy
  :ensure t
  :config
  (keymap-unset prog-mode-map "M-q")
  (global-set-key (kbd "M-q") 'avy-goto-char))

(use-package whitespace
  :config
  (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark missing-newline-at-eof))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package gptel
  :ensure t
  :config
  (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t)
  (setq gptel- 'org-mode)
  (setq gptel-log-level 'debug))


(use-package markdown-mode
  :ensure t)

(use-package org
  :ensure t
  :config
  ;; Just allow running all languages in src blocks.
  (setq org-confirm-babel-evaluate nil))
