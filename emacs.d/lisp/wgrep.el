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

