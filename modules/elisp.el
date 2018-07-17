(predd-defmethod almacs/major-mode-change 'emacs-lisp-mode (mode)
  (evil-cleverparens-mode)
  (general-define-key
   :keymaps '(normal)
   :prefix ","
   "e" '(:ignore t :wk "eval")
   "eb" '(eval-buffer :wk "buffer")
   "ee" '(eval-last-sexp :wk "sexp")))

