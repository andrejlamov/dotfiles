(use-package dash-functional)

(use-package dash)

(use-package s)

(load-file "~/.emacs.d/funs.el")

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-move-beyond-eol t)
  :config
  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package evil-little-word
  :straight (evil-little-word :type git :host github :repo "tarao/evil-plugins" :files ("evil-little-word.el")))

(use-package dash-functional)

(use-package general
  :config
  (general-evil-setup)
  (setq general-override-states '(normal visual motion))
  (general-override-mode))

(use-package scala-mode)

(use-package helm-ag)

(use-package helm
  :config
  (require 'helm-config)
  (helm-top-poll-mode)
  (define-key shell-mode-map  (kbd "M-p") 'helm-comint-input-ring)
  (define-key helm-map (kbd "M-w") 'helm-yank-text-at-point)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)

  (helm-autoresize-mode 1)
  (setq helm-autoresize-min-height 40
        helm-ff-auto-update-initial-value nil
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-split-window-inside-p t
        helm-locate-command "locate %s -e -A --regex %s | grep -v \"^$HOME/\\\..*\"")
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
                                    (funcall #'helm-default-display-buffer buffer)))))

  (defun almacs/kill-hggrep-buffer (&rest args)
    (ignore-errors
      (kill-buffer "*hggrep*")))

  (advice-add 'helm-git-grep-save-results :before #'almacs/kill-hggrep-buffer)

  (setq helm-source-buffers-list
        (helm-make-source "Buffers" 'helm-source-buffers))

  (helm-add-action-to-source "Switch to buffer (same window)"
                             'almacs/switch-to-buffer
                             helm-source-buffers-list 0))

(use-package avy)

(use-package company :config (global-company-mode))

(use-package smartparens
  :config
  ;; https://github.com/Fuco1/smartparens/issues/908
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil))

(use-package paredit)

(use-package evil-cleverparens
  :straight (evil-cleverparens :type git :host github :repo "andrejlamov/evil-cleverparens")
  :commands evil-cleverparens-mode
  :init
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'hy-mode-hook #'evil-cleverparens-mode)
  :config
  (require 'evil-cleverparens-text-objects)
  (setq evil-cleverparens-use-additional-bindings t
        evil-cleverparens-use-additional-movement-keys t)
  (almacs/define-key 'normal '(evil-cleverparens-mode-map)
                     "\M-H" almacs/avy-cp-backward-up
                     "\M-r" nil
                     "\M-r(" sp-splice-sexp-killing-backward
                     "\M-rr" sp-raise-sexp
                     "\M-r\M-r" sp-raise-sexp
                     "\M-r)" sp-splice-sexp-killing-forward))

(use-package clojure-mode)

(use-package which-key
  :init
  (setq which-key-idle-delay 0
        which-key-add-column-padding 0)
  :config
  (which-key-mode))

(use-package predd
  :straight (predd :type git :host github :repo "skeeto/predd" :files ("predd.el")))

(use-package evil-visualstar
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

(use-package expand-region
  :config
  (setq expand-region-fast-keys-enabled t
        expand-region-contract-fast-key "V"))

(use-package xclip
  :config
  (xclip-mode))

(use-package window-purpose
  :config
  (purpose-mode))

(use-package ace-jump-helm-line
  :config
  (setq ace-jump-helm-line-default-action 'select)
  (define-key helm-map (kbd "C-j") 'ace-jump-helm-line))

(use-package popwin
  :straight (popwin :type git :host github :repo "bmag/popwin-el")
  :config (popwin-mode 1))

(use-package ace-window
  :config
  (setq aw-keys '(?h ?j ?k ?l)))

(use-package bash-completion
  :config
  (bash-completion-setup)
  (setq eshell-default-completion-function 'eshell-bash-completion)

  (defun eshell-bash-completion ()
    (while (pcomplete-here
            (nth 2 (bash-completion-dynamic-complete-nocomint
                    (save-excursion (eshell-bol) (point))
                    (point)))))))

(use-package flycheck
  :config (add-hook 'sh-mode-hook 'flycheck-mode))

(use-package hydra)

(use-package tile)

(use-package highlight-parentheses
  :config (global-highlight-parentheses-mode))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package writeroom-mode
  :config
  (setq
   writeroom-mode-line t
   writeroom-width 142))

(use-package yaml-mode)

(use-package helpful
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key)
  ("C-c C-d" . 'helpful-at-point)
  ("C-h F" . 'helpful-function)
  ("C-h C" . 'helpful-command))

(use-package hy-mode
  :config
  (general-create-definer hy-def
    :states '(normal visual)
    :keymaps '(hy-mode-map)
    :prefix ","
    :keymaps 'override)

  (hy-def
    "e" '(:ignore t :wk "eval")
    "ef" '(hy-shell-eval-current-form :wk "form")
    "er" '(hy-shell-eval-region :wk "region")
    "ee" '((lambda () (interactive)
             (almacs/eval-enclosed-sexp 'hy-shell-eval-last-sexp)) :wk "exp")
    "eb" '(hy-shell-eval-buffer :wk "buffer")))

(use-package esup)

(use-package aggressive-indent
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package edbi)

(use-package adaptive-wrap
  :config
  (global-visual-line-mode)
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))

(use-package eyebrowse
  :config
  (eyebrowse-init)
  (eyebrowse-setup-evil-keys))

(use-package evil-magit
  :commands evil-magit-init)

(use-package helm-ls-git
  :commands helm-ls-git-ls helm-browse-project)

(use-package helm-git-grep
  :config
  (setq helm-git-grep-sources '(helm-git-grep-source)))

(use-package magit
  :commands magit-status magit-log magit-log-head
  :config
  (evil-magit-init)
  (setq magit-diff-refine-hunk 'all))

(use-package graphql-mode)

(use-package git-auto-commit-mode)

(use-package markdown-mode)

(use-package docker-tramp
  :config
  (setq docker-tramp-use-names t))

(use-package docker)

(use-package docker-compose-mode)

(load-file "~/.emacs.d/keys.el")

(use-package cider
  :config
  (add-hook 'cider-test-report-mode-hook (lambda ()
                                           (force-mode-line-update)))
  cider-auto-test-mode
  (evil-set-command-property 'cider-find-var :jump t)
  (setq
   cider-repl-display-help-banner nil
   cljr-inject-dependencies-at-jack-in nil)

  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . cljs))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . cljr))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . cljc))
  (add-to-list 'purpose-user-regexp-purposes '("^\\*cider-repl .*(cljs:figwheel.*)\\*" . cljs-repl))
  (add-to-list 'purpose-user-regexp-purposes '("^\\*cider-repl .*(clj)\\*" . clj-repl))
  (add-to-list 'purpose-user-name-purposes '("*cider-test-report*" . cider-test-report))
  (add-to-list 'purpose-user-name-purposes '("*cider-result*" . cider-result-report))
  (purpose-compile-user-configuration)

  (almacs/define-key 'normal '(clojure-mode-map clojurescript-mode-map clojurec-mode-map cider-repl-mode-map)
                     ",q" cider-quit
                     ",'" cider-jack-in-clj&cljs
                     ",tt" cider-test-run-ns-tests

                     ",fd" cider-format-defun
                     ",fb" cider-format-buffer

                     ",l1" (purpose-load-window-layout-file "~/.emacs.d/layouts/cider.window-layout")
                     :wk "cider layout"

                     ",eb" cider-eval-buffer
                     ",ec" cider-repl-clear-buffer

                     ",em" (almacs/eval-enclosed-sexp 'cider-macroexpand-1) :wk "macro exp"
                     ",ee" (almacs/eval-enclosed-sexp 'cider-eval-last-sexp) :wk "eval"
                     ",er" (almacs/eval-enclosed-sexp
                            'cider-pprint-eval-last-sexp) :wk "pprint eval"
                     ",ec" (almacs/eval-enclosed-sexp
                            'cider-pprint-eval-last-sexp-to-comment)
                     ",ef" cider-pprint-eval-defun-at-point))

;; emacs lisp mode
(defun almacs/eval-ert-t ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t))

(defun almacs/ert-t ()
  (interactive)
  (ert t))

(defun almacs/elisp-check-eval-buffer ()
  (interactive)
  (check-parens)
  (eval-buffer))

(almacs/define-key 'normal 'emacs-lisp-mode-map
                   ",i" indent-sexp
                   ",c" check-parens
                   ",D" toggle-debug-on-error
                   ",eb" almacs/elisp-check-eval-buffer :wk "buffer"
                   ",ep" (almacs/eval-enclosed-sexp 'pp-eval-last-sexp) :wk "exp pretty"
                   ",ee" (almacs/eval-enclosed-sexp 'eval-last-sexp) :wk "exp"
                   ",tt" almacs/ert-t
                   ",tB" almacs/eval-ert-t)
;;;;

(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  (setq company-tooltip-align-annotations t))

(use-package web-mode
  :general
  (:keymaps
   'web-mode-map

   :states '(normal visual)
   ",/" 'web-mode-element-close
   "a" 'web-mode-element-content-select
   ",b" 'web-mode-element-beginning
   ",c" 'web-mode-element-clone
   ",d" 'web-mode-element-child
   ",e" 'web-mode-element-end
   ",f" 'web-mode-element-children-fold-or-unfold
   ",i" 'web-mode-element-insert
   ",I" 'web-mode-element-insert-at-point
   ",k" 'web-mode-element-kill
   ",m" 'web-mode-element-mute-blanks
   ",n" 'web-mode-element-next
   ",p" 'web-mode-element-previous
   ",r" 'web-mode-element-rename
   ",s" 'web-mode-element-select
   ",t" 'web-mode-element-transpose
   ",u" 'web-mode-element-parent
   ",v" 'web-mode-element-vanish
   ",w" 'web-mode-element-wrap
   ",+" 'web-mode-element-extract
   ",-" 'web-mode-element-contract)

  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-auto-closing t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style  1
        web-mode-enable-css-colorization t))

(use-package prettier-js
  :config '(add-hook 'web-mode-hook 'prettier-js-mode))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-custom-known-commands
        '((sp-splice-sexp . ((:default . evil-mc-execute-default-call-with-count)))
          (sp-convolute-sexp . ((:default . evil-mc-execute-default-call-with-count)))
          (sp-raise-sexp . ((:default . evil-mc-execute-default-call-with-count)))))
  (custom-set-faces
   '(evil-mc-cursor-default-face ((t (:inherit cursor :background "tan" :inverse-video nil)))))

  (defun almacs/vr-evil-mc (regexp start end)
    (interactive
     (vr--interactive-get-args 'vr--mode-regexp 'vr--calling-func-mc-mark))
    (with-current-buffer vr--target-buffer
      (evil-mc-undo-all-cursors)
      (activate-mark)
      (let ((deactivate-mark nil)
            (first-fake-cursor nil))
        (vr--feedback-function (vr--get-regexp-string) t nil (lambda (i j begin end)
                                                               (when (zerop j)
                                                                 (with-current-buffer vr--target-buffer
                                                                   (goto-char end)
                                                                   (push-mark begin)
                                                                   (activate-mark)
                                                                   (let ((fc (evil-mc-make-cursor-here)))
                                                                     (unless first-fake-cursor
                                                                       (setq first-fake-cursor fc)))))))

        (when first-fake-cursor
          (evil-mc-undo-last-added-cursor))
        (pop-mark)
        (evil-force-normal-state))))

  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg))

(use-package visual-regexp)

(use-package evil-org
  :ensure t
  :after org
  :config
  (require 'org-id)
  (setq org-id-link-to-org-use-id t)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-define-key :keymaps 'org-mode-map
                      [remap evil-jump-forward] 'org-cycle)

  (setq org-agenda-files (list "~/org/todo.org" "~/org/know.org")
        org-outline-path-complete-in-steps nil
        org-capture-templates '(("c" "Inbox" entry (file+headline "~/org/todo.org" "Inbox")))
        org-refile-use-outline-path 'file
        org-agenda-span 10
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                                   (todo . " %i %-12:c %-6e")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))

  (general-create-definer org-def
    :states '(motion)
    :keymaps '(org-mode-map evil-org-mode-map)
    :prefix ","
    :keymaps 'override)

  (org-def
    "a" '(org-agenda :wk "agenda")

    "r" '(org-refile :wk "refile")
    "s" '(org-insert-structure-template :wk "template")
    "t" '(org-todo :wk "todo")
    "'" '(org-set-tags-command :wk "tag")

    ;; clock
    "ci" '(org-clock-in :wk "clock in")
    "co" '(org-clock-out :wk "clock out")
    "ces" '(org-set-effort :wk "set effort")
    "cs" '(org-schedule :wk "schedule")

    ;; subtree / filter
    "f" '(:ignore t :wk "tree")
    "fa" '(org-archive-subtree :wk "archive")
    "ft" '(org-show-todo-tree :wk "todo tree")

    ;; list items
    "i" '(:ignore t :wk "list items")
    "ir" '(org-toggle-radio-button :wk "radio")
    "ic" '(org-toggle-checkbox :wk "radio")

    ;; links
    "l" '(:ignore t :wk "links")
    "ls" '(org-store-link :wk "store")
    "li" '(org-insert-link :wk "insert")

    "h" '(helm-org-in-buffer-headings :wk "headings")
    "d" '(org-display-inline-images :wk "display inline images")

    "p" '(:ignore t :wk "prio")
    "pp" '(org-priority :wk "up")
    "pk" '(org-priority-up :wk "up")
    "pj" '(org-priority-down :wk "down")

    "m" '(:ignore t :wk "mind map")
    "mf" '(org-mind-map-write :wk "file")
    "mt" '(org-mind-map-write-current-tree :wk "tree")
    "mb" '(org-mind-map-write-current-branch :wk "branch")))

(use-package org-mind-map
  :init (require 'ox-org)
  :config (setq org-mind-map-engine "dot"))

(use-package helm-org)

(use-package org-drill)

(use-package wgrep)

(use-package wgrep-helm
  :config

  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t)

  (defun almacs/force-wgrep ()
    (grep-mode)
    (wgrep-change-to-wgrep-mode))

  ;; (add-hook 'grep-mode-hook 'almacs/force-wgrep)
  ;; (add-hook 'helm-grep-mode-hook 'almacs/force-wgrep)
  ;; (add-hook 'helm-git-grep-mode-hook 'almacs/force-wgrep)
  ;; (add-hook 'helm-occur-mode-hook 'almacs/force-wgrep)

  (general-create-definer wgrep-def
    :states '(normal)
    :keymaps '(wgrep-mode-map grep-mode-map helm-occur-mode-map helm-git-grep-mode-map)
    :prefix ","
    :keymaps 'override)

  (wgrep-def
    "w" '(wgrep-change-to-wgrep-mode :wk "wgrep"))

  (evil-collection-define-key 'normal 'helm-grep-mode-map
    (kbd "C-l") 'compile-goto-error)
  (evil-collection-define-key 'normal 'grep-mode-map
    (kbd "C-l") 'compile-goto-error)
  (evil-collection-define-key 'normal 'wgrep-mode-map
    (kbd "C-l") 'compile-goto-error))

(almacs/load-el-directory "~/.emacs.d/lisp/")

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; end
