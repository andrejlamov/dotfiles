(use-package evil-mc
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-custom-known-commands
        '((sp-splice-sexp . ((:default . evil-mc-execute-default-call-with-count)))
          (sp-convolute-sexp . ((:default . evil-mc-execute-default-call-with-count)))
          (sp-raise-sexp . ((:default . evil-mc-execute-default-call-with-count)))))
  (custom-set-faces
   '(evil-mc-cursor-default-face ((t (:inherit cursor :background "tan" :inverse-video nil))))))

(use-package visual-regexp)

(defun almacs/vr-evil-mc (regexp start end)
  (interactive
   (vr--interactive-get-args 'vr--mode-regexp 'vr--calling-func-mc-mark))
  (with-current-buffer vr--target-buffer
    (evil-mc-undo-all-cursors)
    (activate-mark)
    (let ((deactivate-mark nil)
          (first-fake-cursor nil))
      (vr--feedback-function (vr--get-regexp-string) t nil (lambda (i j begin end)
                                                             (when (zerop j)
                                                               (with-current-buffer vr--target-buffer
                                                                 (goto-char end)
                                                                 (push-mark begin)
                                                                 (activate-mark)
                                                                 (let ((fc (evil-mc-make-cursor-here)))
                                                                   (unless first-fake-cursor
                                                                     (setq first-fake-cursor fc)))))))

      (when first-fake-cursor
        (evil-mc-undo-last-added-cursor))
      (pop-mark)
      (evil-force-normal-state))))

(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)
