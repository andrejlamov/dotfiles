(defun almacs/elisp-test-buffer ()
  (interactive)
  (eval-buffer)
  (ert t))

(defun almacs/elisp-ert-t ()
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
   "tt" '(almacs/elisp-ert-t :wk "test buffer")
   "tB" '(almacs/elisp-test-buffer :wk "test buffer")))
