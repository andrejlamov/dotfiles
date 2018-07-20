;;; emacs built-in setup

(show-paren-mode)
(winner-mode)
(dirtrack-mode)

(setq
 make-backup-files nil
 auto-save-default nil
 whitespace-style '(face trailing))

(global-whitespace-mode)

;;; almacs packages

(use-package evil
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package general
  :config
  (general-evil-setup))

(use-package helm
  :config
  (require 'helm-config)
  (define-key helm-map (kbd "M-w") 'helm-yank-text-at-point)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)

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
	      (funcall #'helm-default-display-buffer buffer))))))

(use-package helm-swoop
  :defer t
  :config
  (setq helm-swoop-split-with-multiple-windows t)
  ;; TODO: do not winner-undo when delete-other-windows on a single window
  ;(add-hook 'helm-quit-hook (lambda ()
  ;			      (when (equal helm-last-buffer "*Helm Swoop*")
  ;				(winner-undo))))
  ;(advice-add 'helm-swoop :before #'delete-other-windows)
  (define-key helm-swoop-map (kbd "C-w") 'helm-yank-text-at-point))

(use-package iedit)

(use-package avy)

(use-package company :config (global-company-mode))

(use-package smartparens
  :config
  ;; https://github.com/Fuco1/smartparens/issues/908
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil))

(use-package paredit)

(use-package evil-cleverparens
  :requires (paredit smartparens)
  :config
  (require 'evil-cleverparens-text-objects)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (setq evil-cleverparens-use-regular-insert t)
  (smartparens-global-strict-mode))

(use-package clojure-mode)

(use-package which-key
  :init
  (setq which-key-idle-delay 0.1
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

(use-package evil-mc
  :config
  (global-evil-mc-mode 1))

(use-package expand-region
  :config
  (setq expand-region-fast-keys-enabled t
	expand-region-contract-fast-key "V"))

(use-package evil-iedit-state)

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
  (add-to-list 'purpose-user-regexp-purposes '("^\\*cider-repl .*(cljs)\\*" . cljs-repl))
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
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;; almacs core functions

(defun almacs/delete-current-buffer-file ()
  (interactive)
  (delete-file (buffer-file-name))
  (kill-buffer))

(defun almacs/rename-current-file (new-file-name)
  (interactive "Fnew name:")
  (let* ((old-file-name (buffer-file-name))
	 (old-buffer-name (current-buffer)))
    (rename-file old-file-name new-file-name t)
    (kill-buffer old-buffer-name)
    (find-file new-file-name)))

(defun almacs/named-shell (buffer-name)
  (interactive "Bname: ")
  (shell buffer-name))

(defun almacs/load-el-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun almacs/go-to-core-el ()
  (interactive)
  (find-file "~/.emacs.d/core.el"))

;;; almacs keys

; avy cheat sheet
'(defcustom avy-dispatch-alist
  '((?x . avy-action-kill-move)
    (?X . avy-action-kill-stay)
    (?t . avy-action-teleport)
    (?m . avy-action-mark)
    (?n . avy-action-copy)
    (?y . avy-action-yank)
    (?i . avy-action-ispell)
    (?z . avy-action-zap-to-char)))
'(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
    (?m aw-swap-window "Swap Windows")
    (?M aw-move-window "Move Window")
    (?j aw-switch-buffer-in-window "Select Buffer")
    (?n aw-flip-window)
    (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
    (?c aw-split-window-fair "Split Fair Window")
    (?v aw-split-window-vert "Split Vert Window")
    (?b aw-split-window-horz "Split Horz Window")
    (?o delete-other-windows "Delete Other Windows")
    (?? aw-show-dispatch-help)))

;; us international
(general-def '(motion insert)
  ; l
  "M-ø" 'avy-goto-line
  "ø" 'avy-goto-char-in-line
  ; a
  "á" 'avy-goto-word-1-above
  "M-á" 'avy-goto-word-1-below)

(general-def 'motion
  "C-u" 'evil-scroll-up
  "TAB" 'indent-for-tab-command
  "C-w" 'ace-window)

(general-create-definer almacs/leader-def
  :states '(normal visual emacs)
  :prefix "SPC"
  :keymaps 'override
  :non-normal-prefix "M-SPC"
  :prefix-map 'almacs/prefix-map
  :prefix-command 'almacs/prefix-command)

(almacs/leader-def
  "SPC" '(helm-M-x :wk "M-x")
  "TAB" '(evil-switch-to-windows-last-buffer :wk "last buffer")

  "a" '(:ignore t :wk "stuff")
  "aS" '(almacs/named-shell :wk "named shell")
  "as" '(shell :wk "shell")
  "ac" '(almacs/go-to-core-el :wk "core.el")

  "F" '(helm-semantic-or-imenu :wk "semantic search")
  "s" '(helm-swoop :wk "swoop")
  "r" '(helm-resume :wk "resume")
  "k" '(helm-show-kill-ring :wk "kill ring")

  "f" '(:ignore t :wk "file")
  "fs" '(save-buffer :wk "save")
  "ff" '(helm-find-files :wk "ff")
  "fR" '(almacs/rename-current-file :wk "rename file")
  "fD" '(almacs/delete-current-buffer-file :wk "delete file")

  "p" '(:ignore t :wk "purpose")
  "ps" '(purpose-save-window-layout :wk "save")
  "pl" '(purpose-load-window-layout :wk "load")
  "pw" '(purpose-toggle-window-purpose-dedicated :wk "toggle buffer")

  "C-w" '(ace-window :wk "ace")

  "w" '(:ignore t :wk "windows")
  "wu" '(winner-undo :wk "winner-undo")
  "wr" '(winner-redo :wk "winner-redo")
  "wd" '(delete-window :wk "delete")

  "b" '(:ignore t :wk "buffers")
  "bd" '(kill-this-buffer :wk "kill")
  "br" '(rename-buffer :wk "rename")
  "bb" '(helm-buffers-list :wk "list")

  "e" '(:ignore t :wk "edit")
  "ev" '(er/expand-region :wk "expand")
  "ei" '(evil-iedit-state/iedit-mode :wk "iedit")
  "es" '(evil-iedit-state :wk "iedit state")
  "er" '(evil-iedit-state/iedit-mode-from-expand-region :wk "iedit from expand"))

;;; setup modules

(defun almacs/setup-modules ()

  (predd-defmulti almacs/after-save #'identity)
  (predd-defmethod almacs/after-save :default (mode) nil)

  (predd-defmulti almacs/major-mode-change #'identity)
  (predd-defmethod almacs/major-mode-change :default (mode) nil)

  (add-hook
   'after-save-hook
   (lambda ()
     (almacs/after-save major-mode)))

  (add-hook
   'switch-buffer-functions
   (lambda (prev-buf curr-buf)
     (when (not (equal prev-buf curr-buf))
       (general-unbind '(normal) ",")
       (almacs/major-mode-change major-mode))))

  (almacs/load-el-directory "~/.emacs.d/modules/"))
