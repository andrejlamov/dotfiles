(require 'dash)
(require 's)

(setq typo-lf1 "§1qa!QA`~")
(setq typo-lf2 "2wsz@WSZ")
(setq typo-lf3 "3edx#EDX")
(setq typo-lf4 "4rfcvgt5$RFCVGT6^")
(setq typo-rf4 "6^yhhn7ujmkKYHNMJU&bB")
(setq typo-rf3 "8ik,*IK<")
(setq typo-rf2 "9ol.(OL>")
(setq typo-rf1 "0p;/)P:?-['/_{\"?=+]}\|")

(setq typo-fingers `(typo-lf1
                    typo-lf2
                    typo-lf3
                    typo-lf4
                    typo-rf1
                    typo-rf2
                    typo-rf3
                    typo-rf4))

(defun typo-generate-all-combo-buffer ()
  (interactive)
  (progn
    (switch-to-buffer "*typo-all-combos*")
    (erase-buffer)
    (insert (typo-generate-strings typo-all-pairs))))

(defun typo-pairs ()
  (->> typo-fingers
       (-map (lambda (p0)
               (-map (lambda (p1)
                       (-sort (lambda (a b)
                                (string< (symbol-name a) (symbol-name b)))
                              `(,p0 ,p1)))
                     typo-fingers)))
       (-flatten-n 1)
       (-filter (-lambda ((p0 p1))
                  (not (equal p0 p1))))))

(defun typo-random-char (s)
  (let ((len (random (length s))))
    (substring s len (1+ len))))

(defun typo-generate-strings (pairs)
  (->> pairs
       (-map (-lambda ((p0 p1))
               (let ((sentence-len 10)
                     (res nil))
                 (dotimes (_i sentence-len)
                   (let ((word "")
                         (domain (concat (symbol-value p0) (symbol-value p1))))
                     (dotimes (_j (+ 3 (random 5)))
                       (setq word (concat (typo-random-char domain) word)))
                     (setq res (cons word res))))
                 res)))
       (-flatten)
       (s-join " ")))
