(general-create-definer hy-def
  :states '(normal visual)
  :keymaps '(hy-mode-map)
  :prefix ","
  :keymaps 'override)

(hy-def
  "e" '(:ignore t :wk "eval")
  "ef" '(hy-shell-eval-current-form :wk "form")
  "er" '(hy-shell-eval-region :wk "region")
  "ee" '((lambda () (interactive)
           (almacs/eval-enclosed-sexp 'hy-shell-eval-last-sexp)) :wk "exp")
  "eb" '(hy-shell-eval-buffer :wk "buffer"))
