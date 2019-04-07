;; avy cheat sheet
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

(general-def 'motion
  "®" 'er/expand-region
  "C-u" 'evil-scroll-up
  "C-SPC" 'tile
  "C-@" 'tile
  "TAB" 'indent-for-tab-command
  "C-w" 'ace-window)

(general-create-definer almacs/leader-def
  :states '(normal visual emacs kakel-normal)
  :prefix "SPC"
  :keymaps 'override
  :non-normal-prefix "M-SPC"
  :prefix-map 'almacs/prefix-map
  :prefix-command 'almacs/prefix-command)

(almacs/leader-def
  "C-w" '(ace-window :wk "ace")

  "C-j" '(avy-pop-mark :wk "avy pop")
  "J" '(avy-goto-line :wk "avy line")
  "j" '(avy-goto-char-timer :wk "avy timer")

  "F" '(helm-semantic-or-imenu :wk "semantic search")
  "SPC" '(helm-M-x :wk "M-x")

  "TAB" '(evil-buffer :wk "toggle buffer")
  "<backtab>" '(bs-cycle-previous :wk "prev buffer")
  "C-M-i" '(bs-cycle-next :wk "next buffer")

  "a" '(:ignore t :wk "stuff")
  "aS" '(almacs/named-shell :wk "named shell")
  "at" '(almacs/bash-term :wk "term")
  "ak" '(almacs/go-to-keys-el :wk "keys.el")
  "ap" '(almacs/go-to-packages-el :wk "packages.el")
  "aC" '(almacs/reload :wk "reload almacs")
  "aw" '(global-whitespace-mode :wk "whitespace")
  "aW" '(whitespace-cleanup :wk "clean whitespace")
  "az" '(text-scale-adjust :wk "zoom")

  "F" '(helm-semantic-or-imenu :wk "semantic search")
  "s" '(almacs/helm-occur :wk "occur")
  "r" '(helm-resume :wk "resume")
  "k" '(helm-show-kill-ring :wk "kill ring")

  "az" '(almacs/set-font-global-size :wk "global font-size")
  "f" '(:ignore t :wk "file")
  "fs" '(save-buffer :wk "save")
  "ff" '(helm-find-files :wk "ff")
  "fR" '(almacs/rename-current-file :wk "rename file")
  "fD" '(almacs/delete-current-buffer-file :wk "delete file")
  "fL" '(helm-locate :wk "locate file")

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
  "ws" '(split-window-right :wk "split right")
  "wS" '(split-window-below :wk "split below")
  "wR" '(hydra-window-resize/body :wk "resize window")

  "b" '(:ignore t :wk "buffers")
  "bd" '(kill-this-buffer :wk "kill")
  "br" '(rename-buffer :wk "rename")
  "bb" '(helm-buffers-list :wk "list"))

(defhydra hydra-window-resize ()
  "Window resize"
  ("h" shrink-window-horizontally "-h")
  ("j" shrink-window "-v")
  ("k" enlarge-window "+v" )
  ("l" enlarge-window-horizontally "+h"))
