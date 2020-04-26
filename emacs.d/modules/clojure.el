(use-package cider
  :config
  (add-hook 'cider-test-report-mode-hook (lambda ()
                                           (force-mode-line-update)))
  (cider-auto-test-mode)
  (evil-set-command-property 'cider-find-var :jump t)
  (setq
   cider-repl-display-help-banner nil
   cljr-inject-dependencies-at-jack-in nil)

  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . cljs))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . cljr))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . cljc))
  (add-to-list 'purpose-user-regexp-purposes '("^\\*cider-repl .*(cljs:figwheel.*)\\*" . cljs-repl))
  (add-to-list 'purpose-user-regexp-purposes '("^\\*cider-repl .*(clj)\\*" . clj-repl))
  (add-to-list 'purpose-user-name-purposes '("*cider-test-report*" . cider-test-report))
  (add-to-list 'purpose-user-name-purposes '("*cider-result*" . cider-result-report))
  (purpose-compile-user-configuration))

(almacs/evil-define-key 'normal (list clojure-mode-map clojurescript-mode-map clojurec-mode-map cider-repl-mode-map)
                        ",q" 'cider-quit
                        ",'" 'cider-jack-in-clj&cljs
                        ",tt" 'cider-test-run-ns-tests

                        ",fd" 'cider-format-defun
                        ",fb" 'cider-format-buffer

                        ",l1" (lambda () (interactive)
                                (purpose-load-window-layout-file "~/.emacs.d/layouts/cider.window-layout"))

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
                        ",ef" 'cider-pprint-eval-defun-at-point)
