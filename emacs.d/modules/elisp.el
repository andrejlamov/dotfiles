(defun almacs/eval-ert-t ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t))

(defun almacs/ert-t ()
  (interactive)
  (ert t))

(defun almacs/elisp-check-eval-buffer ()
  (interactive)
  (check-parens)
  (eval-buffer))

(evil-define-key 'normal emacs-lisp-mode-map
  ",i" 'indent-sexp
  ",c" 'check-parens
  ",D" 'toggle-debug-on-error
  ",eb" 'almacs/elisp-check-eval-buffer
  ",ee" (lambda () (interactive)
          (almacs/eval-enclosed-sexp 'eval-last-sexp))
  ",tt" 'almacs/ert-t
  ",tB" 'almacs/eval-ert-t)
