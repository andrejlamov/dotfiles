(when (display-graphic-p)
  (set-foreground-color "#002b36")
  (set-background-color "#fdf6e3")
  (set-frame-font "Source code pro")
  (custom-set-faces
   '(mode-line ((t (:background "#002b36" :foreground "#fdf6e3"))))
   '(mode-line-inactive ((t (:background "#eee8d5"))))
   '(helm-ls-git-modified-not-staged-face ((t (:foreground "dark orange"))))))

(-each (face-list)
  (lambda (face)
    (set-face-attribute face nil
                        :font "Source code pro"
                        :height 80
                        :bold nil
                        :italic nil)))

(require 'color)
(-map (lambda (face)
        (let ((color (face-attribute face :foreground)))
          (when (not (eq color 'unspecified))
            (let* ((rgb (color-values color))
                   (hsl (apply 'color-rgb-to-hsl rgb)))
              (list color hsl)))))
      (face-list))
