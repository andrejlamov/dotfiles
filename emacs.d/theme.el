(if (display-graphic-p)
    (progn
      (set-foreground-color "#002b36")
      (set-background-color "#fdf6e3")
      (set-frame-font "Source code pro")
      (custom-set-faces
       '(magit-diff-context-highlight ((t (:background "#fdf6e3"))))
       '(region ((t (:background "#eee8d5"))))
       '(mode-line ((t (:background "brightblack" :foreground "brightwhite"))))
       '(mode-line-inactive ((t (:background "brightwhite"))))
       '(helm-ls-git-modified-not-staged-face ((t (:foreground "dark orange"))))))
  (custom-set-faces
   '(region ((t (:background "#eee8d5"))))))
