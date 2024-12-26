;; Display the value of a variable in a dedicated buffer.
;; The buffe is automatically updated on variable update.
;; To achive this, built-in `add-variable-watcher` is used.


(require 'dash)

(defvar al-watch-buffer-prefix "*al-watch*")
(defvar al-watch-formatter 'al-watch-default-formatter)

(defun al-watch-default-formatter (val)
  (let ((res-list nil))
    (if (listp val)
        (progn
          (push "(" res-list )
          (-each-indexed val (lambda (i e)
                               (unless (eq i 0)
                                 (push  "\n " res-list))
                               (push  (prin1-to-string e) res-list)))
          (push  ")" res-list))
      (push  (prin1-to-string val) res-list))
    (apply 'concat (reverse res-list))))

(defun al-watch-watcher-fn (symbol newval operation where)
  (let ((b (get-buffer-create (concat al-watch-buffer-prefix " " (symbol-name symbol)))))
    (with-current-buffer b
      (save-excursion
        (lisp-mode)
        (erase-buffer)
        (insert ";; active watcher\n")
        (insert "\n")
        (insert (funcall al-watch-formatter newval))))))

(defun al-watch-add-fn (symbol)
  (interactive "S")
  (add-variable-watcher
   symbol #'al-watch-watcher-fn))


(defun al-watch-remove-fn (symbol)
  (interactive "S")
  (let ((b (get-buffer-create (concat al-watch-buffer-prefix " " (symbol-name symbol)))))
    (with-current-buffer b
      (save-excursion
        (lisp-mode)
        (goto-char 0)
        (kill-line)
        (insert ";; inactive watcher"))))
  (remove-variable-watcher symbol #'al-watch-watcher-fn))

(provide 'al-watch)
