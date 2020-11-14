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
