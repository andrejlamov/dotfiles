(when (display-graphic-p)
  (set-foreground-color "#002b36")
  (set-background-color "#fdf6e3")
  (set-frame-font "Source code pro")
  (custom-set-faces
   '(mode-line ((t (:background "#002b36" :foreground "#fdf6e3"))))
   '(mode-line-inactive ((t (:background "#eee8d5"))))
   '(helm-ls-git-modified-not-staged-face ((t (:foreground "dark orange"))))))
