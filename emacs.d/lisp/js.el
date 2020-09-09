(use-package rjsx-mode
  :config
  (add-hook 'js-mode-hook #'rjsx-mode))

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

  (setq company-tooltip-align-annotations t)
  (add-hook 'js-mode-hook #'setup-tide-mode))

(almacs/define-key 'normal '(tide-mode-map rjsx-mode-map)
                   ",r" rjsx-rename-tag-at-point
                   ",gb" tide-jump-back)
