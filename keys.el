; avy cheat sheet
'(defcustom avy-dispatch-alist
  '((?x . avy-action-kill-move)
    (?X . avy-action-kill-stay)
    (?t . avy-action-teleport)
    (?m . avy-action-mark)
    (?n . avy-action-copy)
    (?y . avy-action-yank)
    (?i . avy-action-ispell)
    (?z . avy-action-zap-to-char)))
'(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
    (?m aw-swap-window "Swap Windows")
    (?M aw-move-window "Move Window")
    (?j aw-switch-buffer-in-window "Select Buffer")
    (?n aw-flip-window)
    (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
    (?c aw-split-window-fair "Split Fair Window")
    (?v aw-split-window-vert "Split Vert Window")
    (?b aw-split-window-horz "Split Horz Window")
    (?o delete-other-windows "Delete Other Windows")
    (?? aw-show-dispatch-help)))

;; us international
(general-def '(motion insert)
  "ø" 'avy-goto-line ;; l
  "þ" 'avy-goto-char-timer ;; t
  "ß" 'avy-goto-word-or-subword-1 ;; s
  )

(general-def 'motion
  "C-u" 'evil-scroll-up
  "TAB" 'indent-for-tab-command
  "C-w" 'ace-window)

(general-create-definer almacs/leader-def
  :states '(normal visual emacs)
  :prefix "SPC"
  :keymaps 'override
  :non-normal-prefix "M-SPC"
  :prefix-map 'almacs/prefix-map
  :prefix-command 'almacs/prefix-command)

(almacs/leader-def
  "SPC" '(helm-M-x :wk "M-x")
  "TAB" '(evil-switch-to-windows-last-buffer :wk "last buffer")

  "a" '(:ignore t :wk "stuff")
  "aS" '(almacs/named-shell :wk "named shell")
  "as" '(shell :wk "shell")
  "ac" '(almacs/go-to-core-el :wk "core.el")
  "aC" '(almacs/reload :wk "reload almacs")
  "aw" '(global-whitespace-mode :wk "whitespace")
  "aW" '(whitespace-cleanup :wk "clean whitespace")

  "F" '(helm-semantic-or-imenu :wk "semantic search")
  "s" '(helm-swoop-without-pre-input :wk "swoop")
  "S" '(helm-swoop :wk "swoop input")
  "r" '(helm-resume :wk "resume")
  "k" '(helm-show-kill-ring :wk "kill ring")

  "f" '(:ignore t :wk "file")
  "fs" '(save-buffer :wk "save")
  "ff" '(helm-find-files :wk "ff")
  "fR" '(almacs/rename-current-file :wk "rename file")
  "fD" '(almacs/delete-current-buffer-file :wk "delete file")

  "p" '(:ignore t :wk "purpose")
  "ps" '(purpose-save-window-layout :wk "save")
  "pl" '(purpose-load-window-layout :wk "load")
  "pw" '(purpose-toggle-window-purpose-dedicated :wk "toggle buffer")

  "C-w" '(ace-window :wk "ace")

  "w" '(:ignore t :wk "windows")
  "wu" '(winner-undo :wk "winner-undo")
  "wr" '(winner-redo :wk "winner-redo")
  "wf" '(delete-other-windows :wk "full")
  "wd" '(delete-window :wk "delete")
  "ws" '(split-window-below :wk "split below")
  "wS" '(split-window-right :wk "split right")

  "b" '(:ignore t :wk "buffers")
  "bd" '(kill-this-buffer :wk "kill")
  "br" '(rename-buffer :wk "rename")
  "bb" '(helm-buffers-list :wk "list")

  "e" '(:ignore t :wk "edit")
  "ev" '(er/expand-region :wk "expand")
  "ei" '(evil-iedit-state/iedit-mode :wk "iedit")
  "es" '(evil-iedit-state :wk "iedit state")
  "er" '(evil-iedit-state/iedit-mode-from-expand-region :wk "iedit from expand"))
