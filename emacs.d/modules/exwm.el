(use-package exwm
  :config
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)
  (setq exwm-workspace-number 1)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (customize-set-variable 'exwm-input-global-keys
                          `(([?\s-&] . (lambda (command)
                                         (interactive (list (read-shell-command "$ ")))
                                         (start-process-shell-command command nil command)))
                            ([?\s-g] . keyboard-quit)

                            ([?\s-h] . windmove-left)
                            ([?\s-j] . windmove-down)
                            ([?\s-k] . windmove-up)
                            ([?\s-l] . windmove-right)

                            ([?\s-t] . tile)

                            ([?\s-C] . (lambda ()
                                         (interactive)
                                         (start-process-shell-command "chromium" nil "chromium")))
                            ([?\s-n] . almacs/next-buffer)
                            ([?\s-n] . almacs/next-buffer)
                            ([?\s-p] . almacs/prev-buffer)
                            ([?\s-b] . helm-buffers-list)
                            ([?\s-B] . helm-browse-project)
                            ([?\s-r] . helm-resume)
                            ([?\s-u] . winner-undo)
                            ([?\s-m] . delete-other-windows)
                            ([?\s-F] . exwm-layout-set-fullscreen)
                            ([?\s-f] . exwm-layout-unset-fullscreen)
                            ([?\s-d] . delete-window)
                            ([?\s-D] . kill-this-buffer)
                            ([?\s-\ ] . almacs/prefix-command)
                            ([?\s-,] . almacs/switch-buffer-by-first-char)))

  (exwm-input-set-key (kbd "s-R") #'exwm-reset)
  (exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
  (exwm-enable)

  ;; (setq exwm-randr-workspace-monitor-plist '(0 "DP-3-2" 0 "eDP-1"))
  ;; (add-hook 'exwm-randr-screen-change-hook
  ;;          (lambda ()
  ;;            (start-process-shell-command
  ;;             "xrandr" nil "xrandr --output DP-3-2 --primary --auto --output eDP-1 --off")))
  (require 'exwm-randr)
  (exwm-randr-enable))



