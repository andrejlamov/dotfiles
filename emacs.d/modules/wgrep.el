(use-package wgrep)
(use-package wgrep-helm
  :config
  (add-hook 'helm-grep-mode-hook 'wgrep-change-to-wgrep-mode)
  (add-hook 'helm-git-grep-mode-hook 'wgrep-change-to-wgrep-mode)
  (add-hook 'helm-occur-mode-hook 'wgrep-change-to-wgrep-mode)

  (general-create-definer wgrep-def
    :states '(normal)
    :keymaps '(wgrep-mode-map grep-mode-map helm-occur-mode-map helm-git-grep-mode-map)
    :prefix ","
    :keymaps 'override)

  (wgrep-def
    "w" '(wgrep-change-to-wgrep-mode :wk "wgrep"))

  (defun almacs/wgrep-jump-to ()
    (interactive)
    (pcase major-mode
      ('helm-grep-mode (helm-grep-mode-jump-other-window))
      ('helm-occur-mode (helm-occur-mode-goto-line))
      (other (message "other"))))

  (evil-collection-define-key 'normal 'wgrep-mode-map
    (kbd "C-l") #'almacs/wgrep-jump-to))

