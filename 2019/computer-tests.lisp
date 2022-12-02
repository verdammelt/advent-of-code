;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (load "./computer"))

(in-package #:computer)

(5am:def-suite* :aoc-2019/intcode :in :aoc-2019)

;;;
;;; tests
;;;
(5am:def-test add (:suite :aoc-2019/intcode)
  (5am:is (= 4 (peek (compute '(1 5 6 0 99 2 2)) 0))))

(5am:def-test mul (:suite :aoc-2019/intcode)
  (5am:is (= 6 (peek (compute '(2 5 6 0 99 2 3)) 0))))

(5am:def-test immediate-mode-first-parameter (:suite :aoc-2019/intcode)
  (5am:is (= 4 (peek (compute '(101 2 6 0 99 X 2)) 0))))

(5am:def-test immediate-mode-second-parameter (:suite :aoc-2019/intcode)
  (5am:is (= 4 (peek (compute '(1001 5 2 0 99 2 X)) 0))))

(5am:def-test immediate-mode-all-parameters (:suite :aoc-2019/intcode)
  (5am:is (= 4 (peek (compute '(1101 2 2 0 99)) 0))))

(5am:def-test example-program (:suite :aoc-2019/intcode)
  ;; (will terminate because 99 written to next PC)
  (5am:is (eq :halt (slot-value (compute '(1002 4 3 4 33)) 'state))))

(5am:def-test input (:suite :aoc-2019/intcode)
  (5am:is (= 666
             (with-input-from-string (input-stream "666")
               (peek (compute '(3 0 99)
                              :input-stream input-stream)
                     0)))))

(5am:def-test output (:suite :aoc-2019/intcode)
  (5am:is (string= "5 "
                   (with-output-to-string (output-stream)
                     (compute '(1101 2 3 0 4 0 99)
                              :output-stream output-stream)))))

(5am:def-test input/output (:suite :aoc-2019/intcode)
  (5am:is (string= "13 "
                   (with-output-to-string (output-stream)
                     (with-input-from-string (input-stream "13")
                       (compute '(3 0 4 0 999)
                                :input-stream input-stream
                                :output-stream output-stream))))))

(defun run-compute (program input)
  "Run the PROGRAM on the INPUT and return the programs OUTPUT."
  (string-trim
   '(#\Space)
   (with-output-to-string (output-stream)
     (with-input-from-string (input-stream input)
       (compute program :input-stream input-stream :output-stream output-stream)))))

(5am:def-test equals-positional (:suite :aoc-2019/intcode)
  (let ((program '(3 9 8 9 10 9 4 9 99 -1 8)))
    (5am:is (string= "1" (run-compute program "8 ")))
    (5am:is (string= "0" (run-compute program "9")))))

(5am:def-test equals-immediate (:suite :aoc-2019/intcode)
  (let ((program '(3 3 1108 -1 8 3 4 3 99)))
    (5am:is (string= "1" (run-compute program "8")))
    (5am:is (string= "0" (run-compute program "9")))))

(5am:def-test less-than-position (:suite :aoc-2019/intcode)
  (let ((program '(3 9 7 9 10 9 4 9 99 -1 8)))
    (5am:is (string= "1" (run-compute program "7")))
    (5am:is (string= "0" (run-compute program "9")))))

(5am:def-test less-than-immediate (:suite :aoc-2019/intcode)
  (let ((program '(3 3 1107 -1 8 3 4 3 99)))
    (5am:is (string= "1" (run-compute program "7")))
    (5am:is (string= "0" (run-compute program "9")))))

(5am:def-test less-than-equal-case (:suite :aoc-2019/intcode)
  (5am:is (string= "0" (run-compute '(3 9 7 9 10 9 4 9 99 -1 8) "8")))
  (5am:is (string= "0" (run-compute '(3 3 1107 -1 8 3 4 3 99) "8"))))

;;; big example
(5am:def-test big-examples (:suite :aoc-2019/intcode)
  (let ((program '(3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                   1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                   999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99)))
    (flet ((run-test (input expected)
             (5am:is (string= expected (run-compute program input)))))
      (run-test "7" "999")
      (run-test "8" "1000")
      (run-test "9" "1001"))))
