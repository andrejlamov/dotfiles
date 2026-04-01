(setq al-debouncer/registry (make-hash-table))

;;;###autoload
(cl-defun al-debouncer/create (&key
                        (name 'global)
                        (fn (lambda () ))
                        (time 1))
  (let ((timer (gethash name al-debouncer/registry))
        (new-timer (run-at-time time nil fn)))
    (when timer
      (cancel-timer timer))
    (puthash name new-timer  al-debouncer/registry)))

(provide 'al-debouncer)
