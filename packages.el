(use-package evil
  :pin melpa
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :pin melpa
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package general
  :pin melpa
  :config
  (general-evil-setup))

(use-package helm
  :pin melpa
  :config
  (require 'helm-config)
  (define-key helm-map (kbd "M-w") 'helm-yank-text-at-point)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

(use-package iedit)

(use-package avy :pin melpa)

(use-package company :config (global-company-mode))

(use-package paredit :defer t)

(use-package helm-swoop
  :defer t
  :config
  (define-key helm-swoop-map (kbd "C-w") 'helm-yank-text-at-point))

(use-package smartparens :defer t)

(use-package evil-cleverparens
  :pin melpa
  :commands evil-cleverparens-mode)

(use-package clojure-mode)

(use-package which-key
  :init
  (setq which-key-idle-delay 0.1
	which-key-add-column-padding 0)
  :config
  (which-key-mode))

(use-package switch-buffer-functions :pin melpa)

(use-package predd :load-path "~/.emacs.d/gitsubmodules/predd")
