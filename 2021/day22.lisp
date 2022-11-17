(defpackage #:aoc-2021-22
  (:use :cl))

(in-package #:aoc-2021-22)

(aoc:def-today-suite*)

(defun parse-on-off (cmd)
  (cond ((string= cmd "on") t)
        ((string= cmd "off") nil)
        (t (error "Unknown reboot command ~S" cmd))))

(defun parse-range (range-str)
  (mapcar #'parse-integer (aoc:split-string-on-char #\. range-str)))

(defun parse-coordinates (x-range y-range z-range)
  (loop for x from (first x-range) to (second x-range)
        append (loop for y from (first y-range) to (second y-range)
                     append (loop for z from (first z-range) to (second z-range)
                                  collect (list x y z)))))

(defun parse-reboot-step (str)
  (let ((parts (aoc:split-string-on-chars '(#\Space #\= #\,) str)))
    (list (parse-on-off (first parts))
          (list (parse-range (third parts))
                (parse-range (fifth parts))
                (parse-range (seventh parts))))))

(defun read-data (file)
  (aoc:read-data file :line-parser #'parse-reboot-step))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +example-2+
  (read-data (aoc:today-data-pathname "example-2")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun range-check (min-x max-x allowed-min-x allowed-max-x
                    min-y max-y allowed-min-y allowed-max-y
                    min-z max-z allowed-min-z allowed-max-z)
  (and (or (<= allowed-min-x min-x allowed-max-x)
           (<= allowed-min-x max-x allowed-max-x))
       (or (<= allowed-min-y min-y allowed-max-y)
           (<= allowed-min-y max-y allowed-max-y))
       (or (<= allowed-min-z min-z allowed-max-z)
           (<= allowed-min-z max-z allowed-max-z))))

(defun command-in-range-p (cmd range)
  (destructuring-bind (_c ((min-x max-x) (min-y max-y) (min-z max-z))) cmd
    (declare (ignore _c))
    (destructuring-bind ((allowed-min-x allowed-max-x)
                         (allowed-min-y allowed-max-y)
                         (allowed-min-z allowed-max-z)) range
      (range-check min-x max-x allowed-min-x allowed-max-x
                   min-y max-y allowed-min-y allowed-max-y
                   min-z max-z allowed-min-z allowed-max-z))))


(defun set-cube-command (setting)
  (lambda (cubes coord)
    (if setting
        (setf (gethash coord cubes) setting)
        (remhash coord cubes))
    cubes))

(defun perform-command (cubes reboot-command)
  (destructuring-bind
      (setting ((min-x max-x) (min-y max-y) (min-z max-z)))
      reboot-command
      (let ((set-fn (set-cube-command setting)))
       (loop for x from min-x to max-x
             do (loop for y from min-y to max-y
                      do (loop for z from min-z to max-z
                               do (funcall set-fn cubes (list x y z))))))
    cubes))

(defun filter-commands-by-coordinates (commands coordinate-ranges)
  (remove-if-not #'(lambda (cmd) (command-in-range-p cmd coordinate-ranges)) commands))

(defun perform-reboot-commands (commands reactor-core)
  (reduce #'perform-command commands :initial-value reactor-core))

(defun part1 (input)
  (hash-table-count
   (perform-reboot-commands
    (filter-commands-by-coordinates input '((-50 50) (-50 50) (-50 50)))
    (make-hash-table :test #'equal))))

(5am:def-test part1 (:suite :aoc-2021-22)
  (5am:is (= 590784 (part1 +example+)))
  (5am:is (= 623748 (part1 +input+))))

(defun part2 (input)
  (perform-reboot-commands input (make-hash-table :test #'equal)))

(5am:def-test part2 (:suite :aoc-2021-22)
  (5am:skip ":aoc-2021-22.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
