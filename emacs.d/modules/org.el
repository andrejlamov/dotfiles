(use-package org-mind-map
  :init (require 'ox-org)
  :config (setq org-mind-map-engine "dot"))

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

(general-create-definer org-def
  :states '(normal)
  :keymaps '(org-mode-map evil-org-mode-map)
  :prefix ","
  :keymaps 'override)

(org-def
  "m" '(:ignore t :wk "mind map")
  "mf" '(org-mind-map-write :wk "file")
  "mt" '(org-mind-map-write-current-tree :wk "tree")
  "mb" '(org-mind-map-write-current-branch :wk "branch"))
