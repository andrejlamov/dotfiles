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

(almacs/define-key 'normal 'emacs-lisp-mode-map
                   ",i" indent-sexp
                   ",c" check-parens
                   ",D" toggle-debug-on-error
                   ",eb" almacs/elisp-check-eval-buffer :wk "buffer"
                   ",ep" (almacs/eval-enclosed-sexp 'pp-eval-last-sexp) :wk "exp pretty"
                   ",ee" (almacs/eval-enclosed-sexp 'eval-last-sexp) :wk "exp"
                   ",tt" almacs/ert-t
                   ",tB" almacs/eval-ert-t)
