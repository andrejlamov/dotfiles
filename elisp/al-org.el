(require 'dash)
(require 'org)

(setq org-agenda-files `("~/org-next/" "~/org-next/journaling/"))
(setq al-org-dir "~/org-next")
(setq al-org-journal-dir "~/org-next/journaling/")
(setq org-todo-keywords `((sequence "TODO(t)" "NOTE(n)" "|" "DONE(d/!)" "CANCELLED(c@/!)" "LOGGED(l)")))
(setq org-refile-targets '(("~/org-next/todo.org" :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path t)

(define-prefix-command 'al-org-key-map)
(define-key al-org-key-map "t" 'al/todo-file)
(define-key al-org-key-map "j" 'al/org-journal-today)
(define-key al-org-key-map "a" 'org-agenda)


(defun al/org-journal-today ()
  (interactive)
  (-let* (((month day year) (calendar-current-date))
          (file-name (s-lex-format "${year}-${month}-${day}.org" )))
    (find-file (s-lex-format "${al-org-journal-dir}/journal-${file-name}"))))

(defun al/todo-file ()
  (interactive)
  (find-file (s-lex-format "${al-org-dir}/todo.org")))


(provide 'al-org)
