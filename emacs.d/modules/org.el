(use-package evil-org
  :after org
  :config

  (org-babel-do-load-languages 'org-babel-load-languages
    '((shell . t)))

  (defun almacs/org/confirm-babel-evaluate (lang body) nil)
  (setq org-confirm-babel-evaluate 'almacs/org/confirm-babel-evaluate)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  (evil-define-key 'normal org-mode-map
    (kbd "TAB") 'org-cycle
    "t" 'org-todo))
