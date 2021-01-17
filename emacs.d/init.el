(add-to-list 'load-path "~/.emacs.d/lisp")
(defvar bootstrap-version)
(let ((bootstrap-version 5)
      (bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package gcmh
  :config
  (gcmh-mode 1))

(setq whitespace-style '(face trailing ))
(global-whitespace-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions nil)

(when window-system
  (set-frame-font "Source Code Pro")
  (fringe-mode 0)
  (setq visible-bell nil
      ring-bell-function 'ignore))

(use-package s :defer t)

(use-package general
  :after evil
  :config
  (general-evil-setup)
  (setq general-override-states '(normal visual motion))
  (general-override-mode)

  (general-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "<SPC>"
    "v" 'evil-visual-char
    "TAB" 'evil-switch-to-windows-last-buffer
    "a s" 'shell
    "b d" 'evil-delete-buffer
    "w m" 'delete-other-windows
    "w l" 'split-window-right
    "w j" 'split-window-below
    "f s" 'save-buffer))

(use-package almacs-utils
  :straight nil
  :defer t
  :general
  (:states
   '(normal visual)
   :keymaps 'override
    "<SPC> b r" 'almacs/rename-buffer))

(use-package better-defaults)

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-move-beyond-eol t
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package avy
  :general
  (:states
   '(override normal)
   "M-'" 'avy-goto-char-2
   "M-\"" 'avy-goto-line
   "'" 'avy-goto-char-2)
  :config
  (setq avy-background t)
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "M-'") 'avy-isearch)))

(use-package company
  :config (global-company-mode)
  :defer 2)

(use-package helm-ls-git
  :general (:states
            '(normal visual)
            :prefix "<SPC>"
            :keymaps 'override
            "g g" 'helm-grep-do-git-grep
            "B" 'helm-ls-git-ls))

(use-package helm
  :general (:states
            '(normal visual motion)
            :prefix "<SPC>"
            :keymaps 'override
            "<SPC>" 'helm-M-x
            "b b" 'helm-buffers-list
            "k" 'helm-show-kill-ring
            "r" 'helm-resume
            "s s" 'helm-occur
            "s g" 'helm-do-grep-ag
            "s f" 'helm-find
            "f f" 'helm-find-files)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1)
  (setq helm-ff-preferred-shell-mode 'shell-mode)
  (general-def
    :states
    '(normal visual insert)
    :keymaps
    '(inferior-python-mode-map shell-mode-map)
    "M-p" 'helm-comint-input-ring))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-cleverparens
  :hook
  ((clojure-mode
    lisp-mode
    emacs-lisp-mode
    hy-mode) . evil-cleverparens-mode)
  :general
  (:keymaps
   'evil-cleverparens-mode-map
   :states '(normal visual)
   "M-r" nil
   "M-r (" 'sp-splice-sexp-killing-backward
   "M-r r" 'sp-raise-sexp
   "M-r M-r" 'sp-raise-sexp
   "M-r )" 'sp-splice-sexp-killing-forward)
  :config
  (require 'evil-cleverparens-text-objects)
  (setq evil-cleverparens-use-additional-bindings t
        evil-cleverparens-use-additional-movement-keys t))

(use-package winner
  :general
  (:states
   '(normal visual)
   "<SPC> w u" 'winner-undo)
  :config
  (winner-mode))

(with-eval-after-load 'lisp-mode
  (general-def 'normal 'emacs-lisp-mode-map
    ",ee" 'eval-last-sexp
    ",eb" 'eval-buffer)
  (general-def 'visual 'emacs-lisp-mode-map
    ",ee" 'eval-region))

(use-package ace-window
  :general
  (:states '(visual normal motion)
           :keymaps 'override
           "C-w" 'ace-window)
  :config
  (ace-window-display-mode 1)
  (setq aw-keys '(?h ?j ?k ?l)))

(use-package magit
  :general
  (:states '(normal visual)
           :keymaps 'override
           "<SPC> g l" 'magit-log-current
           "<SPC> g s" 'magit-status)
  :config
  (setq magit-diff-refine-hunk 'all))

(use-package which-key
  :config
  (setq which-key-idle-delay 0
        which-key-idle-secondary-delay 0)
  (which-key-mode))

(use-package wgrep-helm
  :defer t
  :hook ((helm-occur-mode helm-grep-mode) . wgrep-change-to-wgrep-mode))

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package helpful
  :defer t
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key)
  ("C-c C-d" . 'helpful-at-point)
  ("C-h F" . 'helpful-function)
  ("C-h C" . 'helpful-command))

(use-package expand-region
  :general (:states 'normal
                    "v" 'er/expand-region)
  :config
  (setq expand-region-smart-cursor t))

(use-package evil-org
  :hook ((org-mode) . evil-org-mode)
  :general (:states
            '(normal)
            :keymaps 'org-mode-map
            ",t" 'org-show-todo-tree)
  :init
  (setq org-agenda-files '("~/org/todo.org"))
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package undo-tree
  :general (:states '(normal visual)
                    :keymaps 'override
                    "U" 'undo-tree-visualize
                    "u" 'undo-tree-undo)
  :config
  (global-undo-tree-mode))

(use-package ace-jump-helm-line
  :after helm
  :config
  (setq ace-jump-helm-line-default-action 'select)
  (define-key helm-map (kbd "M-'") 'ace-jump-helm-line))

(use-package anaconda-mode
  :hook (python-mode . anaconda-mode)
  :config
  (general-def 'normal 'anaconda-mode-map
    ",ee" 'python-shell-send-statement
    ",eb" 'python-shell-send-buffer
    ",ws" 'python-shell-switch-to-shell
    ",ss" 'run-python
    ",sa" 'pythonic-activate)
  (general-def 'visual 'anaconda-mode-map
    ",ee" 'python-shell-send-region))

(use-package company-anaconda
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package git-auto-commit-mode
  :defer t)

(message (emacs-init-time))
