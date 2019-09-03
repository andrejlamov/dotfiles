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

(defun h-to-domain (domain h)
  (let ((range (/ 1.0 (length domain))))
    (->> domain
         (-map-indexed
          (lambda (idx d)
            (list
             (* (1+ idx) range) d)))
         (-first (-lambda ((r d)) (< h r)))
         (-second-item))))

'(-each (face-list)
   (lambda (face)
     (let ((color (face-attribute face :foreground)))
       (when (not (eq color 'unspecified))
         (-let* ((rgb (color-values color))
                 ((h s l) (apply 'color-rgb-to-hsl rgb))
                 (c (h-to-domain
                     '("RoyalBlue1" "RoyalBlue1"
                       "RoyalBlue1" "RoyalBlue1"
                       "RoyalBlue2" "RoyalBlue2"
                       "RoyalBlue2" "RoyalBlue2"
                       "RoyalBlue3" "RoyalBlue3"
                       "RoyalBlue3" "RoyalBlue3"
                       "RoyalBlue4" "RoyalBlue4"
                       "RoyalBlue4" "RoyalBlue4"
                       "SlateBlue1" "SlateBlue1"
                       "SlateBlue1" "SlateBlue1"
                       "SlateBlue2" "SlateBlue2"
                       "SlateBlue2" "SlateBlue2"
                       "SlateBlue3" "SlateBlue3"
                       "SlateBlue3" "SlateBlue3"
                       "SlateBlue4" "SlateBlue4"
                       "SlateBlue4" "SlateBlue4"
                       ) h)))
           (set-face-attribute face nil
                               :foreground c))))))

(->>
 (face-list)
 (-map (lambda (face)
         (let ((color (face-attribute face :foreground)))
           (when (not (eq color 'unspecified))
             (-let* ((rgb (color-values color))
                     ((h s l) (apply 'color-rgb-to-hsl rgb))
                     (c (h-to-domain
                         '("brown1" "brown2" "brown3" "brown4") h)))
               (set-face-attribute face nil
                                   :foreground c)
               h)))))
 (-filter 'numberp))
