;; -*- lexical-binding: t -*-
(setq use-package-always-defer t)
(load-theme 'modus-operandi-deuteranopia t nil)

(use-package emacs
  :config
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (setq use-package-always-ensure t)

  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice t)
  (setq user-emacs-directory (expand-file-name "~/.config/emacs/.emacs.d"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq use-short-answers t)
  (visual-line-mode 1)
  (add-hook 'prog-mode-hook 'visual-line-mode)
  (ignore-errors
    (setq visible-bell nil)
    (scroll-bar-mode -1)
    (set-fringe-mode 0))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
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
        completion-styles '(flex basic)
        completion-auto-help nil
        icomplete-show-matches-on-no-input t)

  ;; This complition-in-region-function is using the send completion style as
  ;; minibuffer, so that we get a consistent experience that looks good in terminal.
  (defun al/completion-in-region-function (start end collection &optional predicate)
           (let* ((initial (buffer-substring-no-properties start end))
                  (res (completing-read "Complete: " collection predicate t initial)))
             (when res
               (delete-region start end)
               (insert res)))b
               t)
  ;; Completion-in-region is a variable holding the stratey of region-completion.
  (setq completion-in-region-function #'al/completion-in-region-function)
  (define-key icomplete-fido-mode-map (kbd "TAB") 'icomplete-fido-ret))

'(use-package magit
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

'(progn
  (setq al/-occur-debouncer nil)
  (defun al/-occur-on-change (beg end len)
    (occur-1 (minibuffer-contents) nil (list (window-buffer (minibuffer-selected-window))) "*Occur*"))

  (defun al/live-occur ()
    (interactive)
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'after-change-functions #'al/-occur-on-change nil t))
      (read-string "Enter special input: "))))

