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

(use-package paredit :defer t)

(use-package helm-swoop
  :defer t
  :config
  (define-key helm-swoop-map (kbd "C-w") 'helm-yank-text-at-point))

(use-package smartparens :defer t)

(use-package evil-cleverparens
  :commands evil-cleverparens-mode)

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
