(defun almacs/non-asterisk-other-hidden-buffers ()
  (let ((other-visible-buffers (->> (window-list)
                                   (-map (-compose 'buffer-name 'window-buffer))
                                   (-remove-item (buffer-name)))))
    (->> (buffer-list)
         (-map 'buffer-name)
         (-filter (lambda (b) (not (-contains? other-visible-buffers b))))
         (-filter (lambda (b) (not (s-matches? "\*.*\*" (s-trim b)))))
         (-sort (lambda (a b) (s-less? (s-lower-camel-case a)
                                       (s-lower-camel-case b)))))))

(defun almacs/non-asterisk-other-hidden-buffers-same-prefix ()
  (->> (almacs/non-asterisk-other-hidden-buffers)
       (-filter (lambda (b) (s-starts-with? (-> (buffer-name)
                                                (substring 0 1)
                                                (s-lower-camel-case))
                                            (s-lower-camel-case b))))))

(defun almacs/switch-buffer (buffers next-idx-fn)
  (let* ((max-idx (1- (length buffers)))
         (current-idx (-elem-index (buffer-name) buffers)))
    (if current-idx
        (let ((next-idx (funcall next-idx-fn current-idx max-idx)))
          (switch-to-buffer (nth next-idx buffers) t t))
      (switch-to-buffer (car buffers) t t))))

(defun almacs/-prev-idx  (current-idx max-idx)
  (cond
   ((eq current-idx 0) max-idx)
   (t (1- current-idx))))

(defun almacs/-next-idx (current-idx max-idx)
  (cond
   ((eq current-idx max-idx) 0)
   (t (1+ current-idx))))

(defun almacs/prev-buffer-within-prefix ()
  (interactive)
  (almacs/switch-buffer (almacs/non-asterisk-other-hidden-buffers-same-prefix)
                        'almacs/-prev-idx))

(defun almacs/next-buffer-within-prefix ()
  (interactive)
  (almacs/switch-buffer (almacs/non-asterisk-other-hidden-buffers-same-prefix)
                        'almacs/-next-idx))

(defun almacs/prev-buffer ()
  (interactive)
  (almacs/switch-buffer (almacs/non-asterisk-other-hidden-buffers)
                        'almacs/-prev-idx))

(defun almacs/next-buffer ()
  (interactive)
  (almacs/switch-buffer (almacs/non-asterisk-other-hidden-buffers)
                        'almacs/-next-idx))

(defun almacs/switch-buffer-by-first-char (char)
  (interactive "c")
  (when-let ((b (->> (buffer-list)
                     (-map 'buffer-name)
                     (-filter (lambda (b) (not (s-equals? (buffer-name) (s-trim b)))))
                     (-sort 's-less?)
                     (-find (lambda (b) (s-starts-with? (char-to-string char) (s-trim b) t))))))
    (switch-to-buffer b t t)))

(defun almacs/switch-to-buffer (candidate)
  (interactive)
  (switch-to-buffer candidate nil t))

(defun almacs/delete-current-buffer-file ()
  (interactive)
  (delete-file (buffer-file-name))
  (kill-buffer))
