(progn
  "Fast startup"
  (setq gc-cons-threshold (* 1024 1024 100)))

(progn
  "Bootstrap straight package manager."
  (defvar bootstrap-version)
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" (or (bound-and-true-p straight-base-dir) user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer (url-retrieve-synchronously "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el" 'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(progn
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  '(global-display-line-numbers-mode)
  (setq visible-bell nil)
  (set-fringe-mode 0)
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setopt use-short-answers t)
  (straight-use-package 'ef-themes)
  (ignore-errors
    (set-frame-font "-*-IBM Plex Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
  (global-set-key (kbd "M-SPC t r") 'ef-themes-load-random)
  (load-theme 'ef-cherie t))

(progn
  (setq vc-follow-symlinks t))

(progn
  (defvar al/meta-spc-map (make-sparse-keymap))
  (global-set-key (kbd "M-SPC") al/meta-spc-map)
  (global-set-key (kbd "C-z") 'repeat)
  (global-set-key (kbd "C-x C-f") 'find-file)
  (global-set-key (kbd "C-x C-S-f") 'find-file-literally)

  (defun al/backward-kill-or-kill-region ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'kill-region)
      (call-interactively 'backward-kill-word)))
  (global-set-key (kbd "C-w") 'al/backward-kill-or-kill-region))

(progn
  (add-to-list 'load-path "~/elisp/"))

(progn
  (straight-use-package 'no-littering)
  (require 'no-littering)
  (no-littering-theme-backups))


(progn
  (straight-use-package 'helpful)
  (global-set-key (kbd "C-h f") 'helpful-callable)
  (global-set-key (kbd "C-h v") 'helpful-variable)
  (global-set-key (kbd "C-h k") 'helpful-key)
  (global-set-key (kbd "C-h x") 'helpful-command)
  (global-set-key (kbd "C-c C-d") 'helpful-at-point)
  (global-set-key (kbd "C-h F") 'helpful-function))


(progn
  ;; todo: Custom autoload?
  (straight-use-package 'good-scroll)
  (good-scroll-mode 1)
  (global-set-key (kbd "M-v") 'good-scroll-down)
  (global-set-key (kbd "C-v") 'good-scroll-up))


(progn
  (setq-default whitespace-style
                '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))
  (setq-default whitespace-display-mappings
                '( ;; space -> · else .
                  (space-mark 32 [183] [46])
                  ;; new line -> ¬ else $
                  (newline-mark ?\n [172 ?\n] [36 ?\n])
                  ;; carriage return (Windows) -> ¶ else #
                  (newline-mark ?\r [182] [35])
                  ;; tabs -> » else >
                  (tab-mark ?\t [187 ?\t] [62 ?\t])))

  (setq-default whitespace-global-modes
                '(not shell-mode
                      help-mode
                      magit-mode
                      magit-diff-mode
                      ibuffer-mode
                      dired-mode
                      occur-mode))

  (global-whitespace-mode))


(progn
  (straight-use-package 'adaptive-wrap)
  (setq adaptive-wrap-extra-indent 2)
  (adaptive-wrap-prefix-mode 1)
  (global-visual-line-mode 1)
  (setq-default truncate-lines 1)

  (defun al/truncate-lines ()
    (interactive)
    (adaptive-wrap-prefix-mode 0)
    (global-visual-line-mode 0)
    (toggle-truncate-lines 1))

  (defun al/wrap-lines ()
    (interactive)
    (adaptive-wrap-prefix-mode 1)
    (global-visual-line-mode 1)
    (toggle-truncate-lines 0))

  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
  ;; todo: check if whitespace mode is on
  (defun al/adaptive-wrap--prefix-face (&rest rest)
    'whitespace-space)

  (advice-add
   'adaptive-wrap--prefix-face
   :override
   #'al/adaptive-wrap--prefix-face))


(progn
  (winner-mode +1)
  (global-set-key (kbd "M-S-p") 'winner-undo)
  (global-set-key (kbd "M-S-n") 'winner-redo)
  (global-set-key (kbd "M-P") 'winner-undo)
  (global-set-key (kbd "M-N") 'winner-redo))


(progn
  (straight-use-package 'magit)
  (setq magit-diff-refine-hunk 'all)
  (add-hook 'magit-status-mode-hook (lambda ()
                                      (adaptive-wrap-prefix-mode -1)
                                      (toggle-truncate-lines 1))))


(progn
  (straight-use-package 'smartparens)
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)

  ;; sp move
  (global-set-key (kbd "C-M-a") 'sp-beginning-of-sexp)
  (global-set-key (kbd "C-M-e") 'sp-end-of-sexp)
  (global-set-key (kbd "C-M-f") 'sp-forward-sexp)
  (global-set-key (kbd "C-M-b") 'sp-backward-sexp)
  (global-set-key (kbd "C-M-n") 'sp-down-sexp)
  (global-set-key (kbd "C-M-S-n") 'sp-backward-down-sexp)
  (global-set-key (kbd "C-M-p") 'sp-up-sexp)
  (global-set-key (kbd "C-M-S-p") 'sp-backward-up-sexp)

  ;; sp slurp & barf
  (global-set-key (kbd "C-M-h") 'sp-backward-slurp-sexp)
  (global-set-key (kbd "C-M-S-h") 'sp-backward-barf-sexp)
  (global-set-key (kbd "C-M-l") 'sp-forward-slurp-sexp)
  (global-set-key (kbd "C-M-S-l") 'sp-forward-barf-sexp)

  ;; sp wrap
  (global-set-key (kbd "C-M-S-r") 'sp-rewrap-sexp)
  (global-set-key (kbd "C-M-u") 'sp-unwrap-sexp)
  (global-set-key (kbd "C-M-S-u") 'sp-backward-unwrap-sexp)
  (global-set-key (kbd "C-M-(") 'sp-wrap-round)
  (global-set-key (kbd "C-M-{") 'sp-wrap-curly)
  (global-set-key (kbd "C-M-[") 'sp-wrap-square)

  ;; sp juggle
  (global-set-key (kbd "C-M-S-s") 'sp-split-sexp)
  (global-set-key (kbd "C-M-s") 'sp-splice-sexp)
  (global-set-key (kbd "C-M-r") 'sp-raise-sexp)
  (global-set-key (kbd "C-M-j") 'sp-join-sexp)
  (global-set-key (kbd "C-M-t") 'sp-transpose-sexp)
  (global-set-key (kbd "C-M-S-a") 'sp-absorb-sexp)
  (global-set-key (kbd "C-M-S-e") 'sp-emit-sexp)
  (global-set-key (kbd "C-M-o") 'sp-convolute-sexp)

  ;; sp destruct
  (global-set-key (kbd "C-M-c") 'sp-change-inner)
  (global-set-key (kbd "C-M-S-c") 'sp-change-enclosing)
  (global-set-key (kbd "C-M-k") 'sp-kill-sexp)
  (global-set-key (kbd "C-M-S-k") 'sp-backward-kill-sexp)
  (global-set-key (kbd "C-M-w") 'sp-copy-sexp))

(progn
  (straight-use-package 'wgrep))


(progn
  straight-use-package 'vertico
  (straight-use-package 'consult)
  (straight-use-package 'orderless)
  (straight-use-package 'corfu)
  (straight-use-package 'consult-ls-git)

  (setq corfu-auto t)
  (global-corfu-mode)
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  (vertico-mode)
  (setq enable-recursive-minibuffers nil)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)

  (keymap-global-set "M-R" #'vertico-repeat)
  (keymap-set vertico-map "M-P" #'vertico-repeat-previous)
  (keymap-set vertico-map "M-N" #'vertico-repeat-next)
  (keymap-set vertico-map "S-<prior>" #'vertico-repeat-previous)
  (keymap-set vertico-map "S-<next>" #'vertico-repeat-next)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (global-set-key (kbd "M-s l") 'consult-line)
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "M-y") 'consult-yank-pop)

  (global-set-key (kbd "M-l") 'consult-ls-git)
  (global-set-key (kbd "M-L") 'consult-project-buffer)
  (global-set-key (kbd "M-u") 'consult-git-grep)
  (global-set-key (kbd "M-U") 'consult-grep)
  (keymap-set vertico-map "M-q" #'vertico-quick-insert)
  (keymap-set vertico-map "C-q" #'vertico-quick-exit))


(progn
  (straight-use-package 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

(progn
  (global-set-key (kbd "M-m") 'kmacro-start-macro-or-insert-counter)
  (global-set-key (kbd "M-M") 'kmacro-end-or-call-macro))

(progn
  (defun al/whitespace-delete-forward ()
    (interactive)
    (save-mark-and-excursion
      (let ((start-point (if (looking-at "[[:space:]\n]")
                             (point)
                           (progn (re-search-forward "[^[:space:]\n]")
                                  (point))))
            (end-point (re-search-forward "[^[:space:]\n]")))
        (delete-region start-point (- end-point 1)))))
  (define-key al/meta-spc-map (kbd "wd") #'al/whitespace-delete-forward))


(progn
  "avy"
  (straight-use-package 'avy)
  (straight-use-package 'casual-avy)
  (global-set-key (kbd "M-q") 'avy-goto-char-timer)
  (keymap-global-set "M-Q" #'casual-avy-tmenu)

  '(custom-set-faces
   '(avy-lead-face ((t (:background "#7feaff" :foreground "black" :inherit bold))))
   '(avy-lead-face-0 ((t (:background "#ffaaff" :foreground "black" :inherit bold))))
   '(avy-lead-face-1 ((t (:background "#7feaff" :foreground "black" :inherit bold))))
   '(avy-lead-face-2 ((t (:background "#ffaaff" :foreground "black" :inherit bold))))))


(progn
  "window managment"
  (straight-use-package 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (ace-window-display-mode 1)
  (global-set-key (kbd "M-o") 'ace-window)


  (global-set-key (kbd "M-SPC w r") 'crux-transpose-windows))


(progn

  (defun al/shell-named (name)
    (interactive "sName: ")
    (shell (concat "*shell* " name)))

  (defun al/async-shell-named (name)
    (interactive "sName: ")
    )
  (define-key al/meta-spc-map (kbd "as") #'al/shell-named)
  (define-key al/meta-spc-map (kbd "aa") #'async-shell-command))


(progn
  (straight-use-package 'deadgrep))


(progn
  "lsp"
  (straight-use-package 'eglot)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-stay-out-of 'flymake))
  ;; todo: how do i set these? https://github.com/typescript-language-server/typescript-language-server/blob/master/docs/configuration.md#workspacedidchangeconfiguration
  )


(progn
  "use eglot format for tab command when in treesitter tsx mode. treesitter indention does not work in jsx files."
  (defun al/eglot-indent-for-tab-command ()
    (interactive)
    (if (use-region-p)
        (call-interactively 'eglot-format)
      (eglot-format (line-beginning-position) (line-end-position))))

  (defun al/setup-eglot-format ()
    (setq-local tab-width 2)
    (if (member 'eglot--managed-mode minor-mode-list)
        (define-key tsx-ts-mode-map [remap indent-for-tab-command] 'al/eglot-indent-for-tab-command)))

  (add-hook 'eglot-managed-mode-hook 'al/setup-eglot-format))


(progn
  (straight-use-package 'combobulate))


(progn
  "treesitter stuff"

  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (mapc (lambda (l) (unless (treesit-language-available-p l)
                      (treesit-install-language-grammar l)))
        (mapcar #'car treesit-language-source-alist)))

(progn
  "js,jsx,ts,tsx,html,css"
  (straight-use-package 'typescript-mode)
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook 'tsx-ts-mode))


(progn
  (straight-use-package 'crux))

(progn
  (straight-use-package 'embark)
  (straight-use-package 'embark-consult)

  ;; embark
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "C-;") 'embark-dwim)
  (global-set-key (kbd "C-h B") 'embark-bindings))


(progn
  ;; todo: https://www.ackerleytng.com/posts/emacs-xref/
  ;; todo: https://git.sr.ht/~pkal/xref-union/
  (straight-use-package 'dumb-jump)
  (global-set-key (kbd "M-s j") 'dumb-jump-go))


(progn
  "isearch"

  (straight-use-package 'isearch-plus)
  (eval-after-load "isearch" '(require 'isearch+))
  (define-key isearch-mode-map (kbd "M-q") 'avy-isearch)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)

  (straight-use-package 'casual-isearch)
  (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu))


(progn
  "regexp builder"
  (straight-use-package 'casual-re-builder)
  (require 're-builder)
  (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu))


(progn
  (straight-use-package 'which-key)
  (setq which-key-idle-delay 0.1)
  (setq which-key-idle-secondary-delay 0.1)
  
  (which-key-mode 1))


(progn
  (straight-use-package 'cider))


(progn
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
)
(progn
  (straight-use-package 'casual-dired)
  (require 'casual-dired)
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first")
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu)
  (keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu))


(progn
  (straight-use-package 'markdown-mode)
  (setq markdown-command "pandoc -t html5"))




(progn
  (straight-use-package 'flymake-shellcheck)
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)

  )

(progn
  (straight-use-package 'exec-path-from-shell)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(progn
  (straight-use-package 'pet)
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (add-hook 'python-mode-hook 'pet-mode -10)
  (add-hook 'inferior-python-mode-hook 'pet-mode -10)
  (straight-use-package 'auto-virtualenvwrapper)
  (require 'auto-virtualenvwrapper)
  (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4)

  (defun al/python-eval-dwim ()
    (interactive)
    (let* ((process (python-shell-get-process))
           (code-snippet (buffer-substring
                          (if (region-active-p)
                              (region-beginning)
                            (save-excursion
                              (python-nav-beginning-of-statement)
                              (point)))
                          (if (region-active-p)
                              (region-end)
                            (save-excursion
                              (python-nav-end-of-statement)
                              (point))))))
      (save-window-excursion
        (switch-to-buffer-other-window (process-buffer process))
        (end-of-buffer))
      (python-shell-send-string code-snippet process)))

  (defun al/python-eval-defun ()
    (interactive)
    (let* ((process (python-shell-get-process))
           (code-snippet (buffer-substring
                          (save-excursion
                            (python-nav-beginning-of-defun)
                            (point))
                          (save-excursion
                            (python-nav-end-of-defun)
                            (point)))))
      (save-window-excursion
        (switch-to-buffer-other-window (process-buffer process))
        (end-of-buffer))
      (python-shell-send-string code-snippet process)))


  (defun al/django-shell ()
    (interactive)
    (let ((python-shell-interpreter "python")
          (python-shell-interpreter-args "manage.py shell -i ipython"))
      (call-interactively 'run-python)))

  (add-hook 'python-mode-hook
          (lambda ()
            (setq-local python-shell-interpreter (pet-executable-find "python")
                        python-shell-virtualenv-root (pet-virtualenv-root))))


  ;; (defun python-comint-filter (output)
  ;;   (replace-regexp-in-string "__PYTHON_EL.*" "" output))
  ;; (add-to-list 'comint-preoutput-filter-functions #'python-comint-filter)

  (keymap-set python-mode-map "C-x C-e" #'al/python-eval-dwim)
  (keymap-set python-mode-map "C-M-x" #'al/python-eval-defun))

(progn
  (straight-use-package 'yaml-mode))


(progn
  (straight-use-package '(edraw-org :type git :host github :repo "misohena/el-easydraw"))
  (with-eval-after-load 'org
    (require 'edraw-org)
    (edraw-org-setup-default))
  ;; When using the org-export-in-background option (when using the
  ;; asynchronous export function), the following settings are
  ;; required. This is because Emacs started in a separate process does
  ;; not load org.el but only ox.el.
  (with-eval-after-load "ox"
    (require 'edraw-org)
    (edraw-org-setup-exporter)))


(progn
  (straight-use-package 'org-download)
  (add-hook 'dired-mode-hook 'org-download-enable))


(progn
    (setq org-log-done 'time))

(progn
  "thanks magnars"
  (straight-use-package 'dash))

(progn
  (require 'al-watch))