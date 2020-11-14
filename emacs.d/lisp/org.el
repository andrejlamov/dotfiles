;; (use-package org-evil
;;   :config
;;   (require 'org-evil)
;;   (require' org-id)
;;   (setq
;;    org-id-link-to-org-use-id t
;;    org-directory "~/Dropbox/org"
;;    org-default-notes-file (concat org-directory "/inbox.org")
;;    org-capture-templates
;;    '(("p" "Capture at point" entry (file "~/Dropbox/org/inbox.org")
;;       "* %?\n  %i\n  %a")
;;      ("c" "Capture" entry (file "~/Dropbox/org/inbox.org")
;;       "* %?"))
;;    org-agenda-start-day "-1d"
;;    org-startup-indented t
;;    org-agenda-span 14
;;    org-agenda-start-on-weekday nil
;;    org-startup-with-inline-images t
;;    org-refile-use-outline-path nil
;;    org-agenda-files '("~/Dropbox/org/inbox.org"
;;                       "~/Dropbox/org/projects.org"
;;                       "~/Dropbox/org/reference.org"
;;                       "~/Dropbox/org/someday.org"
;;                       "~/Dropbox/org/calendar.org")
;;    org-refile-targets '((("~/Dropbox/org/projects.org") :maxlevel . 1)))
;;   ;; eval src blocks
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                '((shell . t)))
;;   (org-display-inline-images t t)
;;   (defun almacs/org/confirm-babel-evaluate (lang body) nil)
;;   (setq org-confirm-babel-evaluate 'almacs/org/confirm-babel-evaluate)

;;   '(defun almacs/org-insert-custom-id (&rest rest)
;;      (org-set-property "CUSTOM_ID" (s-concat "id-" (almacs/uuid))))
;;   '(advice-add 'org-insert-heading-respect-content :after #'almacs/org-insert-custom-id)
;;   (general-define-key
;;    :keymaps 'org-mode-map
;;    [remap evil-jump-forward] 'org-cycle))

(use-package evil-org
  :ensure t
  :after org
  :config
  (require 'org-id)
  (setq org-id-link-to-org-use-id t)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-define-key :keymaps 'org-mode-map
                      [remap evil-jump-forward] 'org-cycle)

  (setq org-agenda-files (list "~/org/todo.org" "~/org/know.org")
        org-outline-path-complete-in-steps nil
        org-capture-templates '(("c" "Inbox" entry (file+headline "~/org/todo.org" "Inbox")))
        org-refile-use-outline-path 'file
        org-agenda-span 10
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                                   (todo . " %i %-12:c %-6e")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c"))))

(use-package org-mind-map
  :init (require 'ox-org)
  :config (setq org-mind-map-engine "dot"))

(use-package helm-org)

(use-package org-drill)

(general-create-definer org-def
  :states '(motion)
  :keymaps '(org-mode-map evil-org-mode-map)
  :prefix ","
  :keymaps 'override)

(org-def
  "a" '(org-agenda :wk "agenda")

  "r" '(org-refile :wk "refile")
  "s" '(org-insert-structure-template :wk "template")
  "t" '(org-todo :wk "todo")
  "'" '(org-set-tags-command :wk "tag")

  ;; clock
  "ci" '(org-clock-in :wk "clock in")
  "co" '(org-clock-out :wk "clock out")
  "ces" '(org-set-effort :wk "set effort")
  "cs" '(org-schedule :wk "schedule")

  ;; subtree / filter
  "f" '(:ignore t :wk "tree")
  "fa" '(org-archive-subtree :wk "archive")
  "ft" '(org-show-todo-tree :wk "todo tree")

  ;; list items
  "i" '(:ignore t :wk "list items")
  "ir" '(org-toggle-radio-button :wk "radio")
  "ic" '(org-toggle-checkbox :wk "radio")

  ;; links
  "l" '(:ignore t :wk "links")
  "ls" '(org-store-link :wk "store")
  "li" '(org-insert-link :wk "insert")

  "h" '(helm-org-in-buffer-headings :wk "headings")
  "d" '(org-display-inline-images :wk "display inline images")

  "p" '(:ignore t :wk "prio")
  "pp" '(org-priority :wk "up")
  "pk" '(org-priority-up :wk "up")
  "pj" '(org-priority-down :wk "down")

  "m" '(:ignore t :wk "mind map")
  "mf" '(org-mind-map-write :wk "file")
  "mt" '(org-mind-map-write-current-tree :wk "tree")
  "mb" '(org-mind-map-write-current-branch :wk "branch"))

