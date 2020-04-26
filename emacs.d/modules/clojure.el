(use-package cider
  :config
  (add-hook 'cider-test-report-mode-hook (lambda ()
                                           (force-mode-line-update)))
  (cider-auto-test-mode)
  (evil-set-command-property 'cider-find-var :jump t)
  (setq
   cider-repl-display-help-banner nil
   cljr-inject-dependencies-at-jack-in nil))

(-each
    (list clojure-mode-map clojurescript-mode-map clojurec-mode-map cider-repl-mode-map)
  (lambda (mode-map)
    (evil-define-key* 'normal mode-map
                      ",q" 'cider-quit
                      ",'" 'cider-jack-in-clj&cljs
                      ",tt" 'cider-test-run-ns-tests

                      ",fd" 'cider-format-defun
                      ",fb" 'cider-format-buffer

                      ",eb" 'cider-eval-buffer
                      ",ec" 'cider-repl-clear-buffer
                      ",em" (lambda () (interactive)
                              (almacs/eval-enclosed-sexp
                               'cider-macroexpand-1))
                      ",ee" (lambda () (interactive)
                              (almacs/eval-enclosed-sexp
                               'cider-eval-last-sexp))
                      ",er" (lambda () (interactive)
                              (almacs/eval-enclosed-sexp
                               'cider-pprint-eval-last-sexp))
                      ",ec" (lambda () (interactive)
                              (almacs/eval-enclosed-sexp
                               'cider-pprint-eval-last-sexp-to-comment))
                      ",ef" 'cider-pprint-eval-defun-at-point)))
