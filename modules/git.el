(use-package evil-magit
  :commands evil-magit-init)

(use-package helm-ls-git
  :commands helm-ls-git-ls helm-browse-project)

(use-package helm-git-grep
  :config
  (setq helm-git-grep-sources '(helm-git-grep-source)))

(use-package magit
  :commands magit-status magit-log magit-log-head
  :config (evil-magit-init))

(almacs/leader-def
  "g" '(:ignore t :wk "git")
  "gs" '(magit-status :wk "status")
  "gh" '(magit-log-head :wk "log HEAD")
  "gg" '(helm-git-grep :wk "git grep")
  "gG" '(helm-git-grep-at-point :wk "git grep point")
  "b"  '(helm-browse-project :wk "browse repo"))
