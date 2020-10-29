(in-package #:aoc-2015)

(defun what-floor (input)
  (let ((char-hash (reduce #'(lambda (acc c) (incf (gethash c acc 0)) acc) input
                           :initial-value (make-hash-table))))
    (- (gethash #\( char-hash 0) (gethash #\) char-hash 0))))

(let ((test-data '(("(())" . 0)
                   ("()()" . 0)
                   ("(((" . 3)
                   ("(()(()(" . 3)
                   ("))(((((" . 3)
                   ("())" . -1)
                   ("))(" . -1)
                   (")))" . -3)
                   (")())())" . -3))))
  (loop for (input . floor) in test-data
        do (assert (= (what-floor input) floor))))

;; (perform-day-task 1 (with-only-first-input #'what-floor))

(defun when-floor (input &optional (target-floor -1))
  (let ((current-floor 0))
    (1+ (position-if #'(lambda (delta) (= target-floor (incf current-floor delta)))
                     input
                     :key (lambda (c) (case c (#\( 1) (#\) -1)))))))

(let ((test-data '((")" . 1)
                   ("()())" . 5))))
  (loop for (input . position) in test-data
        do (assert (= (when-floor input) position))))

;; (perform-day-task 1 (with-only-first-input #'when-floor))
