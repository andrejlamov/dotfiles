(defun almacs/eval-ert-t ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t))

(defun almacs/ert-t ()
  (interactive)
  (ert t))

(general-create-definer elisp-def
  :states '(normal)
  :keymaps '(emacs-lisp-mode-map)
  :prefix ","
  :keymaps 'override)

(elisp-def
 "i" '(indent-sexp :wk "indent sexp")
 "c" '(check-parens :wk "check parens")
 "e" '(:ignore t :wk "eval")
 "D" '(toggle-debug-on-error :wk "toggle debug on error")
 "eb" '(eval-buffer :wk "buffer")
 "ee" '(almacs/eval-sexp :wk "eval sexp")
 "t" '(:ignore t :wk "test")
 "tt" '(almacs/ert-t :wk "ert t")
 "tB" '(almacs/eval-ert-t :wk "eval ert t")
 "d" '(:ignore t :wk "debug")
 "dT" '(kak-debug :wk "kak"))
