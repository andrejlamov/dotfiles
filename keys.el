;;  (defcustom avy-dispatch-alist
;;   '((?x . avy-action-kill-move)
;;     (?X . avy-action-kill-stay)
;;     (?t . avy-action-teleport)
;;     (?m . avy-action-mark)
;;     (?n . avy-action-copy)
;;     (?y . avy-action-yank)
;;     (?i . avy-action-ispell)
;;     (?z . avy-action-zap-to-char))
(general-def 'motion
  "C-u" 'evil-scroll-up
  "T" 'avy-goto-line
  "TAB" 'indent-for-tab-command
  "t" 'avy-goto-char-in-line
  "f" 'avy-goto-word-1-below
  "F" 'avy-goto-word-1-above)

(general-create-definer almacs/leader-def
  :states '(normal emacs)
  :prefix "SPC"
  :keymaps 'override
  :non-normal-prefix "M-SPC"
  :prefix-map 'almacs/prefix-map
  :prefix-command 'almacs/prefix-command)

(almacs/leader-def
  "SPC" '(helm-M-x :wk "M-x")
  "TAB" '(evil-switch-to-windows-last-buffer :wk "last buffer")

  "a" '(:ignore t :wk "app")
  "as" '(almacs/named-shell :wk "named shell")
  "ae" '(almacs/reconfigure :wk "reconfigure emacs")

  "s" '(helm-occur :wk "occur")
  "r" '(helm-resume :wk "resume")

  "f" '(:ignore t :wk "file")
  "fs" '(save-buffer :wk "save")
  "ff" '(helm-find-files :wk "ff")
  "fR" '(almacs/rename-current-file :wk "rename file")
  "fD" '(almacs/delete-current-buffer-file :wk "delete file")

  "p" '(:ignore t :wk "projects/bookmarks")
  "pp" '(helm-projects-history :wk "projects history")
  "pb" '(helm-bookmarks :wk "bookmarks")

  "w" '(:ignore t :wk "windows")
  "wu" '(winner-undo :wk "winner-undo")
  "wr" '(winner-redo :wk "winner-redo")

  "b" '(:ignore t :wk "buffers")
  "bd" '(kill-this-buffer :wk "kill")
  "br" '(rename-buffer :wk "rename")
  "bb" '(helm-buffers-list :wk "list"))
