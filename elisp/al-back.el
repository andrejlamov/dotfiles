;; Sove position and buffer with granularity of 1 line. The ambition is to
;; be the only "go back" capability ever needed.

(require 'dash)

(defvar al-back-stack nil)
(defvar al-back-max-length 1000)

(defun al-back-push-pos ()
  (let ((not-minibuffer (not (minibuffer-window-active-p (get-buffer-window))))
        (not-popping-stack (not (-contains? '(undo al-back-go-backward) last-command))))
    (when (and not-minibuffer not-popping-stack)
      (-let* ((new-entry (al-back-pos-entry))
              ((&plist :line new-line) new-entry)
              ((&plist :line prev-line) (-first-item al-back-stack)))
        (unless (eq prev-line new-line)
          (progn
            (push (al-back-pos-entry) al-back-stack)
            (setq al-back-stack (-take al-back-max-length al-back-stack))))))))


(defun al-back-pop ()
  (pop al-back-stack))


(defun al-back-pos-entry ()
  (interactive)
  (list :buffer (current-buffer) :line (line-number-at-pos) :point (point)))

(defun al-back-in-minibuffer? ()
  (minibuffer-window-active-p (get-buffer-window)))

(defun al-back-go-backward ()
  (interactive)
  (-let (((&plist :point pos :buffer b) (al-back-pop)))
    (when pos
      (switch-to-buffer b)
      (goto-char pos))))

(defun al-back-go-forward ()
  (interactive))

(provide 'al-back)
