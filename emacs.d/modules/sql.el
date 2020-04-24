(defun almacs/sql-send-line-or-region ()
  (interactive)
  (pcase evil-state
    ('normal (call-interactively 'sql-send-line-and-next))
    ('visual (call-interactively 'sql-send-region))))

(general-create-definer sql-def
  :states '(normal visual)
  :keymaps '(sql-mode-map sql-interactive-mode-map)
  :prefix ","
  :keymaps 'override)

(sql-def
  "'" '(sql-set-sqli-buffer-generally :wk "sqli buffer generally")

  "e" '(:ignore t :wk "eval")
  "eb" '(sql-send-buffer :wk "buffer")
  "ee" '(almacs/sql-send-line-or-region :wk "line/region"))
