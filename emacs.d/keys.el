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

(general-def 'visual
  "C-s" 'almacs/helm-occur-from-region
  "M-s" 'almacs/helm-occur
  "v" 'er/expand-region)

(general-def 'emacs
  "<escape>" 'evil-force-normal-state)

(general-def 'normal
  "<escape>" 'almacs/motion-escape)

(general-def 'motion
  "e" 'evil-forward-little-word-end
  "w" 'evil-a-little-word
  "C-s" 'isearch-forward-regexp
  "C-u" 'evil-scroll-up
  "C-SPC" 'tile
  "C-@" 'tile
  "TAB" 'indent-for-tab-command
  "C-w" 'ace-window

  "t" 'evil-avy-goto-char-in-line
  "T" 'evil-avy-goto-line
  "f" 'evil-avy-goto-char-timer)

(general-create-definer almacs/leader-def
  :states '(normal visual emacs)
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
  "as" '(shell :wk "shell")
  "aS" '(almacs/named-shell :wk "named shell")
  "at" '(almacs/xfce4-term :wk "term")
  "ak" '(almacs/go-to-keys-el :wk "keys.el")
  "am" '(almacs/go-to-modules-el :wk "modules")
  "ap" '(almacs/go-to-base-packages-el :wk "base-packages.el")
  "aC" '(almacs/reload :wk "reload almacs")
  "aw" '(whitespace-cleanup :wk "clean whitespace")
  "az" '(almacs/set-font-global-size :wk "global font-size")
  "aZ" '(text-scale-adjust :wk "local text scale")
  "al" '(toggle-input-method :wk "toggle input")

  "F" '(helm-semantic-or-imenu :wk "semantic search")
  "s" '(almacs/helm-occur :wk "occur")
  "m" '(almacs/vr-evil-mc :wk "mc")
  "r" '(helm-resume :wk "resume")
  "R" '(helm-recentf :wk "recentf")
  "k" '(helm-show-kill-ring :wk "kill ring")
  "c" '(comment-region :wk "comment")
  "C" '(uncomment-region :wk "comment")
  "t" '(toggle-truncate-lines t :wk "toggle truncate")

  "f" '(:ignore t :wk "file")
  "fs" '(save-buffer :wk "save")
  "ff" '(helm-find-files :wk "ff")
  "fR" '(almacs/rename-current-file :wk "rename file")
  "fD" '(almacs/delete-current-buffer-file :wk "delete file")
  "fL" '(helm-locate :wk "locate file")

  "o" '(:ignore t :wk "org")
  "oc" '(org-capture :wk "capture")

  "dd" '(almacs/helm-docker-ps :wk "docker ps")
  "dc" '(docker-compose :wk "docker compose")
  "de" '(helm-list-emacs-process :wk "emacs ps")
  "dh" '(helm-top :wk "top")
  "dt" '(helm-timers :wk "timers")

  "p" '(:ignore t :wk "purpose")
  "ps" '(purpose-save-window-layout :wk "save")
  "pl" '(purpose-load-window-layout :wk "load")
  "pw" '(purpose-toggle-window-purpose-dedicated :wk "toggle buffer")

  "C-w" '(ace-window :wk "ace")

  "w" '(:ignore t :wk "windows")
  "wu" '(almacs/winner-undo :wk "winner-undo")
  "ww" '(almacs/writeroom-enable :wk "toogle writeroom")
  "wf" '(delete-other-windows :wk "full")
  "wd" '(delete-window :wk "delete")
  "wb" '(balance-windows :wk "balance")
  "ws" '(split-window-right :wk "split right")
  "wS" '(split-window-below :wk "split below")
  "wm" '(delete-other-windows :wk "full screen")
  "wr" '(hydra-window-resize/body :wk "resize window")
  "w1" '(eyebrowse-switch-to-window-config-1 :wk "w1")
  "w2" '(eyebrowse-switch-to-window-config-2 :wk "w2")
  "w3" '(eyebrowse-switch-to-window-config-3 :wk "w3")
  "w4" '(eyebrowse-switch-to-window-config-4 :wk "w4")

  "b" '(:ignore t :wk "buffers")
  "bd" '(kill-this-buffer :wk "kill")
  "br" '(rename-buffer :wk "rename")
  "bb" '(helm-buffers-list :wk "list")
  "b." '(almacs/revert-buffer :wk "revert")
  "?" '(helm-man-woman :wk "man"))

(defhydra hydra-window-resize ()
  "Window resize"
  ("h" shrink-window-horizontally "-h")
  ("j" shrink-window "-v")
  ("k" enlarge-window "+v" )
  ("l" enlarge-window-horizontally "+h"))
