(defun almacs/eval-ert-t ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t))

(defun almacs/ert-t ()
  (interactive)
  (ert t))

(predd-defmethod almacs/major-mode-change 'emacs-lisp-mode (mode)
  (general-define-key
   :keymaps '(normal)
   :prefix ","
   "i" '(indent-sexp :wk "indent sexp")
   "e" '(:ignore t :wk "eval")
   "eb" '(eval-buffer :wk "buffer")
   "ee" '(eval-last-sexp :wk "sexp")
   "t" '(:ignore t :wk "test")
   "tt" '(almacs/ert-t :wk "ert t")
   "tB" '(almacs/eval-ert-t :wk "eval ert t")))
