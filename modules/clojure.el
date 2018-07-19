(defun almacs/after-cider-refresh (response _log-buffer)
  (let ((status (nrepl-dbind-response response
                    (out err reloading status error error-ns after before)
                  status)))
    (pcase status
      ('("ok") (progn
		 ;(call-interactively 'cider-eval-buffer)
		 (call-interactively 'cider-test-run-loaded-tests)))
      (other nil))))

(defun almacs/quick-cider-purpose ()
    (interactive)
    (purpose-load-window-layout-file "~/.emacs.d/layouts/cider.window-layout"))

(use-package cider
  :commands clojure-mode cider-jack-in-clojurescript cider-jack-in
  :config
  (purpose-x-popwin-setup)
  (push "*cider-error*" purpose-x-popwin-buffer-names)
  (push "*cider-result*" purpose-x-popwin-buffer-names)
  (push "*cider-result*" popwin:special-display-config)
  (purpose-x-popwin-update-conf)


  (advice-add 'cider-refresh :before #'save-buffer)
  (advice-add 'cider-refresh--handle-response :after #'almacs/after-cider-refresh)

  (setq
   cider-repl-display-help-banner nil
   cljr-inject-dependencies-at-jack-in nil
   cljs-lein-repl "(do (require 'figwheel-sidecar.repl-api)
                       (figwheel-sidecar.repl-api/start-figwheel!)
                       (figwheel-sidecar.repl-api/cljs-repl))"))

(predd-defmethod almacs/after-save 'clojurec-mode (mode)
  (almacs/after-save 'clojure-mode))
(predd-defmethod almacs/after-save 'clojurescript-mode (mode)
  (almacs/after-save 'clojure-mode))

(predd-defmethod almacs/after-save 'clojure-mode (mode)
  (cider-eval-buffer))

(predd-defmethod almacs/major-mode-change 'clojurec-mode (mode)
  (almacs/major-mode-change 'clojure-mode))
(predd-defmethod almacs/major-mode-change 'clojurescript-mode (mode)
  (almacs/major-mode-change 'clojure-mode))
(predd-defmethod almacs/major-mode-change 'cider-repl-mode (mode)
  (almacs/major-mode-change 'clojure-mode))

(predd-defmethod almacs/major-mode-change 'clojure-mode (mode)
  (evil-cleverparens-mode)
  (general-define-key
   :keymaps '(normal)
   :prefix ","
   "Q" '(cider-quit :wk "quit")
   "'" '(cider-jack-in :wk "jack-in clj")
   "\"" '(cider-jack-in-clj&cljs :wk "jack-in cljs&clj")
   "T" '(cider-test-run-project-tests :wk "project tests")
   "R" '(cider-refresh :wk "refresh")
   "p" '(almacs/quick-cider-purpose :wk "cider purpose")
   "e" '(:ignore t :wk "eval")
   "eb" '(cider-eval-buffer :wk "eval buffer")
   "ee" '(cider-pprint-eval-last-sexp :wk "eval sexp")
   "ef" '(cider-pprint-eval-defun-at-point :wk "eval defun")
   "bc" '(cider-repl-clear-buffer :wk "eval sexp")))

