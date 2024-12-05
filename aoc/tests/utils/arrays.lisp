(in-package :aoc-tests)
(5am:in-suite aoc-tests)

(5am:test map-2d-array
  (let* ((grid (aoc:lists->2d-array '((1 2 3) (4 5 6) (7 8 9))))
         (expected (aoc:lists->2d-array '((2 3 4) (5 6 7) (8 9 10)))))
    (5am:is (equalp expected
                    (aoc:map-2d-array #'(lambda (m x y) (1+ (aref m x y))) grid)))))

(5am:test map-2d-array-values
  (let* ((grid (aoc:lists->2d-array '((1 2 3) (4 5 6) (7 8 9))))
         (expected (aoc:lists->2d-array '((2 3 4) (5 6 7) (8 9 10)))))
    (5am:is (equalp expected (aoc:map-2d-array-values #'1+ grid)))))
