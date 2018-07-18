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
  (helm-mode 1))

(use-package iedit)

(use-package avy)

(use-package company :config (global-company-mode))

(use-package helm-swoop
  :defer t
  :config
  (define-key helm-swoop-map (kbd "C-w") 'helm-yank-text-at-point))

(use-package smartparens
  :defer t
  :config
  ;; https://github.com/Fuco1/smartparens/issues/908
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil))

(use-package paredit :defer t)

(use-package evil-cleverparens
  :requires (paredit smartparens)
  :commands evil-cleverparens-mode
  :config (smartparens-global-strict-mode))

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

(use-package expand-region)

(use-package evil-iedit-state)

(use-package xclip
  :config
  (xclip-mode))

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

;;  (defcustom avy-dispatch-alist
;;   '((?x . avy-action-kill-move)
;;     (?X . avy-action-kill-stay)
;;     (?t . avy-action-teleport)
;;     (?m . avy-action-mark)
;;     (?n . avy-action-copy)
;;     (?y . avy-action-yank)
;;     (?i . avy-action-ispell)
;;     (?z . avy-action-zap-to-char))
(general-def 'motion
  "C-u" 'evil-scroll-up
  "T" 'avy-goto-line
  "TAB" 'indent-for-tab-command
  "t" 'avy-goto-char-in-line
  "f" 'avy-goto-word-1-below
  "F" 'avy-goto-word-1-above)

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

  "a" '(:ignore t :wk "app")
  "as" '(almacs/named-shell :wk "named shell")
  "ae" '(almacs/reconfigure :wk "reconfigure emacs")

  "s" '(helm-swoop :wk "swoop")
  "r" '(helm-resume :wk "resume")

  "f" '(:ignore t :wk "file")
  "fs" '(save-buffer :wk "save")
  "ff" '(helm-find-files :wk "ff")
  "fR" '(almacs/rename-current-file :wk "rename file")
  "fD" '(almacs/delete-current-buffer-file :wk "delete file")

  "p" '(:ignore t :wk "projects/bookmarks")
  "pp" '(helm-projects-history :wk "projects history")
  "pb" '(helm-bookmarks :wk "bookmarks")

  "w" '(:ignore t :wk "windows")
  "wu" '(winner-undo :wk "winner-undo")
  "wr" '(winner-redo :wk "winner-redo")

  "b" '(:ignore t :wk "buffers")
  "bd" '(kill-this-buffer :wk "kill")
  "br" '(rename-buffer :wk "rename")
  "bB" '(helm-buffers-list :wk "list")

  "e" '(:ignore t :wk "edit")
  "ee" '(er/expand-region :wk "expand")
  "ei" '(evil-iedit-state/iedit-mode :wk "iedit")
  "es" '(evil-iedit-state :wk "iedit state")
  "er" '(evil-iedit-state/iedit-mode-from-expand-region :wk "iedit from expand"))
