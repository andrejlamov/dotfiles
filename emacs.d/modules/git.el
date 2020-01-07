(use-package evil-magit
  :commands evil-magit-init)

(use-package helm-ls-git
  :commands helm-ls-git-ls helm-browse-project)

(use-package helm-git-grep
  :config
  (setq helm-git-grep-sources '(helm-git-grep-source)))

(use-package magit
  :commands magit-status magit-log magit-log-head
  :config
  (evil-magit-init)
  (setq magit-diff-refine-hunk t))

(almacs/leader-def
  "B"  '(helm-browse-project :wk "browse repo")
  "g" '(:ignore t :wk "git")
  "gF" '(magit-fetch-all :wk "git fetch all")
  "gG" '(helm-git-grep-at-point :wk "git grep point")
  "gR" '(magit-reset-hard :wk "git reset hard")
  "gW" '(almacs/helm-ls-git-word-at-point :wk "lsgit word")
  "gg" '(helm-git-grep :wk "git grep")
  "gh" '(magit-log-head :wk "log HEAD")
  "gs" '(magit-status :wk "status"))
