(in-package #:aoc-2015)

(defun parse-secret-key (lines) (first lines))

(defun make-coin (secret idx)
  (format nil "~{~2,'0X~}"
          (coerce (md5:md5sum-string (format nil "~A~D" secret idx)) 'list)))

(defun leading-zeros (n)
  (lambda (coin)
    (string= (make-string  n  :initial-element #\0) (subseq coin 0 n))))

(defun first-advent-coin-idx (secret &optional (advent-coin-p (leading-zeros 5)))
  (loop for x from 0
     when (funcall advent-coin-p (make-coin secret x))
     do (return x)))

(let ((test-data '((("abcdef") . 609043)
                   (("pqrstuv") . 1048970))))
  (loop for (input . idx) in test-data
     do (assert (= idx (first-advent-coin-idx (parse-secret-key input))))))

(defun first-index-with-six-zeros (secret)
  (first-advent-coin-idx secret (leading-zeros 6)))
