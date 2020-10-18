(use-package web-mode
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook 'js-mode-hook 'web-mode)

  '(setq js-indent-level 2
         web-mode-enable-auto-pairing t
         web-mode-enable-auto-expanding t
         web-mode-markup-indent-offset 2
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-indent-style  1
         web-mode-enable-css-colorization t))

(use-package company-web
  :config
  (push '(company-web-html company-css company-dabbrev) company-backends))

(almacs/define-key '(motion) '(web-mode-map)
                   ",ff" web-mode-fold-or-unfold
                   ",fc" web-mode-element-children-fold-or-unfold
                   ",ei" web-mode-element-insert
                   ",ec" web-mode-element-clone
                   ",ek" web-mode-element-kill
                   ",ev" web-mode-element-vanish
                   ",er" web-mode-element-rename
                   "(" web-mode-element-parent
                   ",ew" web-mode-element-wrap)
