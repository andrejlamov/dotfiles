(when window-system
  (set-frame-font "Source Code Pro")
  (fringe-mode 0)
  (setq visible-bell nil
        ring-bell-function 'ignore))

(set-face-attribute 'mode-line nil
                    :box nil
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :overline nil
                    :underline nil)


(provide 'al-theme)

