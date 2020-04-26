(defun almacs/after-cider-refresh (response _log-buffer)
  (nrepl-dbind-response response (out err reloading status error error-ns after before)
    (when (member "ok" status)
      (call-interactively 'cider-test-run-project-tests))))

(defun almacs/quick-cider-purpose ()
  (interactive)
  (purpose-load-window-layout-file "~/.emacs.d/layouts/cider.window-layout"))

(defun almacs/clj-repl-purpose ()
  (interactive)
  (purpose-load-window-layout-file "~/.emacs.d/layouts/clj-repl.window-layout"))

(use-package cider
  :commands clojure-mode cider-jack-in-clojurescript cider-jack-in
  :config
  '(add-hook 'cider-mode-hook
             (lambda ()
               (add-hook 'after-save-hook 'cider-eval-buffer nil 'make-it-local)))

  (add-hook 'cider-test-report-mode-hook (lambda ()
                                           (force-mode-line-update)))
  (cider-auto-test-mode)
  (evil-set-command-property 'cider-find-var :jump t)
  (setq cider-font-lock-reader-conditionals nil)

  (purpose-x-popwin-setup)
  (push "*cider-error*" purpose-x-popwin-buffer-names)
  (push "*cider-result*" purpose-x-popwin-buffer-names)
  (push "*cider-result*" popwin:special-display-config)
  (purpose-x-popwin-update-conf)

  '(advice-add 'cider-refresh :before #'save-buffer)
  '(advice-add 'cider-ns-refresh--handle-response :after #'almacs/after-cider-refresh)

  (setq
   cider-repl-display-help-banner nil
   cljr-inject-dependencies-at-jack-in nil
   ;; cljs-lein-repl "(do (require 'figwheel-sidecar.repl-api)
   ;;                     (figwheel-sidecar.repl-api/start-figwheel!)
   ;;                     (figwheel-sidecar.repl-api/cljs-repl))"
   ))

(general-create-definer cloj-def
  :states '(normal)
  :keymaps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map cider-repl-mode-map)
  :prefix ","
  :keymaps 'override)

(cloj-def
  "Q" '(cider-quit :wk "quit")
  "'" '(cider-jack-in :wk "jack-in clj")
  "\"" '(cider-jack-in-clj&cljs :wk "jack-in cljs&clj")
  "T" '(cider-test-run-project-tests :wk "project tests")
  "t" '(cider-test-run-test :wk "run test")
  "p" '(almacs/quick-cider-purpose :wk "cider purpose")

  "e" '(:ignore t :wk "eval")
  "eb" '(cider-eval-buffer :wk "eval buffer")
  "em" '((lambda () (interactive)
           (almacs/eval-enclosed-sexp
            'cider-macroexpand-1)) :wk "macro expand")
  "ee" '((lambda () (interactive)
           (almacs/eval-enclosed-sexp
            'cider-eval-last-sexp)) :wk "eval sexp")
  "er" '((lambda () (interactive)
           (almacs/eval-enclosed-sexp
            'cider-pprint-eval-last-sexp)) :wk "pprint eval sexp")
  "ec" '((lambda () (interactive)
           (almacs/eval-enclosed-sexp
            'cider-pprint-eval-last-sexp-to-comment)) :wk "pprint eval sexp to comment")
  "ef" '(cider-pprint-eval-defun-at-point :wk "eval defun")

  "f" '(cider-format-defun :wk "format defun")
  "1" '(almacs/clj-repl-purpose :wk "clj repl purpose")

  "b" '(:ignore t :wk "buffer")
  "bc" '(cider-repl-clear-buffer :wk "cider repl clear"))
