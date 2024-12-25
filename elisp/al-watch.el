;; Display the value of a variable in a dedicated buffer.
;; The buffe is automatically updated on variable update.
;; To achive this, built-in `add-variable-watcher` is used.


(require 'dash)

(defvar al-watch-buffer-prefix "*al-watch*")

(defun al-watch-watcher-fn (symbol newval operation where)
  (let ((b (get-buffer-create (concat al-watch-buffer-prefix " " (symbol-name symbol)))))
    (with-current-buffer b
      (save-excursion
        (lisp-mode)
        (erase-buffer)
        (insert ";; active watcher\n")
        (insert "\n")
        (insert (prin1-to-string newval))))))

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
