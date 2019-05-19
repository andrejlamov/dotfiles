(use-package fancy-narrow)

(use-package evil-mc
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-custom-known-commands
        '((sp-splice-sexp . ((:default . evil-mc-execute-default-call-with-count)))
          (sp-raise-sexp . ((:default . evil-mc-execute-default-call-with-count)))))
  (custom-set-faces
   '(evil-mc-cursor-default-face ((t (:inherit cursor :background "tan" :inverse-video nil))))))

(defun almacs/-add-mc-widen-exit-isearch ()
  (ignore-errors
    (backward-char)
    (evil-mc-make-cursor-here)
    (while (re-search-forward isearch-string nil t nil)
      (backward-char)
      (evil-mc-make-cursor-here)))
  (fancy-widen)
  (remove-hook 'isearch-mode-end-hook 'almacs/-add-mc-widen-exit-isearch))

(defun almacs/evil-mc-isearch-region ()
  (interactive)
  (add-hook 'isearch-mode-end-hook 'almacs/-add-mc-widen-exit-isearch)
  (call-interactively 'fancy-narrow-to-region)
  (evil-force-normal-state)
  (evil-goto-first-line)
  (call-interactively 'isearch-forward-regexp))


