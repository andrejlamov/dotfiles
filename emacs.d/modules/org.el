(use-package org-evil
  :config
  (require 'org-evil)
  (require' org-id)
  (setq
   org-id-link-to-org-use-id t
   org-directory "~/Dropbox/org"
   org-default-notes-file (concat org-directory "/inbox.org")
   org-capture-templates
   '(("p" "Capture at point" entry (file "~/Dropbox/org/inbox.org")
      "* %?\n  %i\n  %a")
     ("c" "Capture" entry (file "~/Dropbox/org/inbox.org")
      "* %?"))
   org-agenda-start-day "-1d"
   org-startup-indented t
   org-agenda-span 14
   org-agenda-start-on-weekday nil
   org-startup-with-inline-images t
   org-refile-use-outline-path nil
   org-agenda-files '("~/Dropbox/org/inbox.org"
                      "~/Dropbox/org/projects.org"
                      "~/Dropbox/org/reference.org"
                      "~/Dropbox/org/someday.org"
                      "~/Dropbox/org/calendar.org")
   org-refile-targets '((("~/Dropbox/org/projects.org") :maxlevel . 1)))
  ;; eval src blocks
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)))
  (org-display-inline-images t t)
  (defun almacs/org/confirm-babel-evaluate (lang body) nil)
  (setq org-confirm-babel-evaluate 'almacs/org/confirm-babel-evaluate)

  (defun almacs/org-insert-custom-id (&rest rest)
    (org-set-property "CUSTOM_ID" (s-concat "id-" (almacs/uuid))))
  (advice-add 'org-insert-heading-respect-content :after #'almacs/org-insert-custom-id))

(use-package org-mind-map
  :init (require 'ox-org)
  :config (setq org-mind-map-engine "dot"))

(use-package helm-org)

(general-create-definer org-def
  :states '(motion)
  :keymaps '(org-mode-map evil-org-mode-map)
  :prefix ","
  :keymaps 'override)

(org-def
  "r" '(org-refile :wk "refile")
  "s" '(org-insert-structure-template :wk "template")
  "t" '(org-todo :wk "todo")
  "'" '(org-set-tags-command :wk "tag")
  "l" '(:ignore t :wk "link")
  "ls" '(org-store-link :wk "store")
  "li" '(org-insert-link :wk "insert")
  "d" '(org-display-inline-images :wk "display inline images")

  "p" '(:ignore t :wk "prio")
  "pp" '(org-priority :wk "up")
  "pk" '(org-priority-up :wk "up")
  "pj" '(org-priority-down :wk "down")

  "m" '(:ignore t :wk "mind map")
  "mf" '(org-mind-map-write :wk "file")
  "mt" '(org-mind-map-write-current-tree :wk "tree")
  "mb" '(org-mind-map-write-current-branch :wk "branch"))
