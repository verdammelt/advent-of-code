(defpackage #:aoc-2023-02
  (:use :cl))

(in-package #:aoc-2023-02)

(aoc:def-today-suite*)

(defun trim-whitespace (str) (string-trim '(#\Space) str))

(defun parse-grab (str)
  (let ((cubes (aoc:split-string-on-chars '(#\, #\Space) str) ))
    (loop for (num color) on cubes by #'cddr
          collect (make-cube (parse-integer num) (aoc:keywordize color)))))

(defun parse-game (str)
  (let ((game-data (aoc:split-string-on-chars '(#\; #\:) str)))
    `(:id ,(parse-integer (first game-data) :start (length "Game"))
      :grabs ,(mapcar #'parse-grab (rest game-data)))))

(defun game-id (game) (getf game :id))
(defun game-grabs (game) (getf game :grabs))

(defun make-cube (num color) (cons color num))
(defun cube-num (cube) (cdr cube))
(defun (setf cube-num) (new-val cube) (setf (cdr cube) new-val))
(defun cube-color (cube) (car cube))

(defun cube-of-color (grab color) (assoc color grab))

(defun read-data (file)
  (aoc:read-data file :line-parser #'parse-game))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun valid-grab-p (limits)
  (lambda (grab)
    (every #'(lambda (cube) (<= (cube-num cube)
                           (cube-num (cube-of-color limits (cube-color cube)))))
           grab)))

(defun valid-game-p (limits)
  (lambda (game) (every (valid-grab-p limits) (game-grabs game))))

(defun part1 (input)
  (let ((valid-games
          (remove-if-not
           (valid-game-p (list (make-cube 12 :red)
                               (make-cube 13 :green)
                               (make-cube 14 :blue)))
           input)))
    (aoc:sum (mapcar #'game-id valid-games))))

(5am:def-test part1 (:suite :aoc-2023-02)
  (5am:is (= 8 (part1 +example+)))
  (5am:is (= 2593 (part1 +input+))))

(defun minium-number-of-cubes (game)
  (reduce #'(lambda (minimums grab)
              (mapcar #'(lambda (cube)
                          (when (> (cube-num cube)
                                   (cube-num (cube-of-color minimums (cube-color cube))))
                            (setf (cube-num (cube-of-color minimums (cube-color cube)))
                                  (cube-num cube))))
                      grab)
              minimums)
          (game-grabs game)
          :initial-value (list (make-cube 0 :red)
                               (make-cube 0 :green)
                               (make-cube 0 :blue))))

(defun part2 (input)
  (let ((minimum-cubes (mapcar #'minium-number-of-cubes input)))
    (aoc:sum
     (mapcar #'(lambda (cubes) (funcall #'aoc:product (mapcar #'cube-num cubes)))
             minimum-cubes))))

(5am:def-test part2 (:suite :aoc-2023-02)
  (5am:is (= 2286 (part2 +example+)))
  (5am:is (= 54699 (part2 +input+))))
