;;(use-package lispy)

(evil-define-state smartparens
  "smartparens state"
  :enable (motion)                      ;
  :entry-hook ()
  :tag "<sp> ")

(setq evil-smartparens-state-map (make-sparse-keymap))

(define-key evil-normal-state-map ";" 'evil-smartparens-state)
(define-key evil-insert-state-map (kbd "C-;") 'evil-smartparens-state)
(define-key evil-smartparens-state-map ";" 'evil-force-normal-state)

(define-key evil-smartparens-state-map "i" 'evil-insert)

(define-key evil-smartparens-state-map "u" 'undo-tree-undo)
(define-key evil-smartparens-state-map "p" 'evil-paste-after)

(define-key evil-smartparens-state-map "sa" 'sp-splice-sexp-killing-around)
(define-key evil-smartparens-state-map "sb" 'sp-splice-sexp-killing-backward)
(define-key evil-smartparens-state-map "sf" 'sp-splice-sexp-killing-forward)
(define-key evil-smartparens-state-map "sj" 'sp-join-sexp)
(define-key evil-smartparens-state-map "sp" 'sp-splice-sexp)
(define-key evil-smartparens-state-map "st" 'sp-split-sexp)

(define-key evil-smartparens-state-map "df" 'sp-kill-sexp)
(define-key evil-smartparens-state-map "db" 'sp-backward-kill-sexp)

(define-key evil-smartparens-state-map "wb" 'sp-backward-unwrap-sexp)
(define-key evil-smartparens-state-map "wf" 'sp-unwrap-sexp)

(define-key evil-smartparens-state-map "ys" 'sp-copy-sexp)
(define-key evil-smartparens-state-map "yb" 'sp-backward-copy-sexp)

(define-key evil-smartparens-state-map "t" 'sp-transpose-sexp)
(define-key evil-smartparens-state-map "c" 'sp-convolute-sexp)


(define-key evil-smartparens-state-map "fs" 'sp-forward-slurp-sexp)
(define-key evil-smartparens-state-map "fb" 'sp-forward-barf-sexp)
(define-key evil-smartparens-state-map "bs" 'sp-backward-slurp-sexp)
(define-key evil-smartparens-state-map "bb" 'sp-backward-barf-sexp) ;


(define-key evil-smartparens-state-map "l" 'sp-forward-sexp)
(define-key evil-smartparens-state-map "h" 'sp-backward-sexp)
(define-key evil-smartparens-state-map "k" 'sp-previous-sexp)
(define-key evil-smartparens-state-map "j" 'sp-next-sexp)

(define-key evil-smartparens-state-map "L" 'sp-beginning-of-next-sexp)
(define-key evil-smartparens-state-map "H" 'sp-beginning-of-previous-sexp)
(define-key evil-smartparens-state-map "K" 'sp-end-of-net-sexp)
(define-key evil-smartparens-state-map "J" 'sp-end-of-previous-sexp)


(define-key evil-smartparens-state-map "}" 'sp-up-sexp)
(define-key evil-smartparens-state-map "]" 'sp-down-sexp)
(define-key evil-smartparens-state-map "{" 'sp-backward-down-sexp)
(define-key evil-smartparens-state-map "[" 'sp-backward-up-sexp)

;;(define-key evil-smartparens-state-map "ac" 'lispy-ace-char)
;;(define-key evil-smartparens-state-map "as" 'lispy-ace-symbol)
;;(define-key evil-smartparens-state-map "at" 'avy-goto-char-timer)
