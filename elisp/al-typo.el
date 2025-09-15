(require 'dash)
(require 's)

(setq typo-lf1 "§1qa!QA`~"
      typo-lf2 "2wsz@WSZ"
      typo-lf3 "3edx#EDX"
      typo-lf4 "4rfcvgt5$RFCVGT6^"
      typo-rf4 "6^yhhn7ujmkKYHNMJU&bB"
      typo-rf3 "8ik,*IK<"
      typo-rf2 "9ol.(OL>"
      typo-rf1 "0p;/)P:?-['/_{\"?=+]}\|")

(setq typo-fingers `(typo-lf1 typo-lf2 typo-lf3 typo-lf4 typo-rf1 typo-rf2 typo-rf3 typo-rf4))

(setq typo-original-buffer "*typo-original*")
(setq typo-practice-buffer "*typo*")

(defun typo-generate-all-combo-buffer (&optional fingers)
  (interactive)
  (progn
    (let ((text (typo-generate-strings (typo-pairs fingers))))

      (switch-to-buffer typo-practice-buffer)
      (erase-buffer)
      (insert text)

      (ignore-errors (kill-buffer typo-original-buffer))
      (switch-to-buffer typo-original-buffer)
      (erase-buffer)
      (insert text)
      (read-only-mode)

      (switch-to-buffer typo-practice-buffer)
      (goto-char (point-min))
      (typo-mode))))

(defun typo-after-change-functions (beg end l)
  (print `(:beg ,beg :end ,end :l ,l))
  (let ((original-char (save-excursion
                         (with-current-buffer typo-original-buffer
                           (char-before end))))
        (practice-char (save-excursion
                         (with-current-buffer typo-practice-buffer
                           (char-before end)))))

    (if (equal end beg)
        (let ((original-string (save-excursion
                                 (with-current-buffer typo-original-buffer
                                   (buffer-substring beg (+ end l))))))
          (save-excursion
            (with-current-buffer typo-practice-buffer
              (goto-char beg)
              (insert original-string))))
      (if (and original-char practice-char)
          (progn
            (with-current-buffer typo-practice-buffer
              (if (equal original-char practice-char)
                  (put-text-property beg (1+ end) 'face '(:foreground "green"))
                (put-text-property beg (1+ end) 'face '(:foreground "red"))))
            (delete-region beg end)
            (goto-char end))))))

(define-derived-mode typo-mode fundamental-mode  "typo"
  (add-hook 'after-change-functions 'typo-after-change-functions nil t))

(defun typo-pairs (&optional fingers)
  (->> typo-fingers
       (-map (lambda (p0)
               (-map (lambda (p1)
                       (-sort (lambda (a b)
                                (string< (symbol-name a) (symbol-name b)))
                              `(,p0 ,p1)))
                     (or fingers typo-fingers))))
       (-flatten-n 1)
       (-filter (-lambda ((p0 p1))
                  (not (equal p0 p1))))))

(defun typo-random-char (s)
  (let ((len (random (length s))))
    (substring s len (1+ len))))

(defun typo-generate-strings (pairs)
  (->> pairs
       (-map (-lambda ((p0 p1))
               (let ((sentence-len 4)
                     (domain (concat (symbol-value p0) (symbol-value p1)))\
                     (sentance nil))
                 (dotimes (_i sentence-len)
                   (let ((word ""))
                     (dotimes (_j (+ 3 (random 5)))
                       (setq word (concat (typo-random-char domain) word)))
                     (setq sentance
                           (cons (propertize word
                                             'pairs `(,p0 ,p1))
                                 sentance))))
                 (->> sentance
                      (-flatten)
                      (s-join  " ")))))
       (-flatten)
       (s-join "\n")))

(provide 'al-typo)
