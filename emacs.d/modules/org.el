(use-package evil-org
  :after org
  :config

  (org-babel-do-load-languages 'org-babel-load-languages
    '((shell . t)))

  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  (evil-define-key 'normal org-mode-map
    (kbd "TAB") 'org-cycle
    "t" 'org-todo))
