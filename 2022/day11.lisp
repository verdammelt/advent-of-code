(defpackage #:aoc-2022-11
  (:use :cl))

(in-package #:aoc-2022-11)

(aoc:def-today-suite*)

(defclass monkey ()
  ((num :initarg :num :reader num)
   (items :initarg :items :initform (list) :accessor items)
   (delta-fn :initarg :delta :reader delta)
   (worried-divisor :initarg :worried-divisor :reader worried-divisor)
   (if-worried :initarg :if-worried :reader if-worried)
   (if-not-worried :initarg :if-not-worried :reader if-not-worried)
   (num-inspections :initform 0 :accessor num-inspections)))

(defclass no-worry-decrease-monkey (monkey) ()
  (:documentation "A Monkey that does not decrease worry after inspection"))

(defun throw-to (monkey item)
  (push item (items monkey)))

(defgeneric inspect-item (monkey item &rest extra-args))
(defmethod inspect-item ((monkey monkey) item &rest extra-args)
  (declare (ignore extra-args))
  (prog1 (floor (/ (funcall (delta monkey) item) 3))
    (incf (num-inspections monkey))))

;;;
;;; without the common divisor thing the numbers become way too big way too
;;; fast. While SBCL is happy to keep chugging away twiht bigger bigger BIGNUMs
;;; they just become slower and slower so we need to keep them at an OK size.
;;; There is not *one* number that would be the right size as just truncating
;;; the number or chopping the 'top' off it won't work. We need to keep the
;;; number smaller but still in the same proportions such that every monkey's
;;; worried functions won't be affected. Thus we compute the common divisor of
;;; all the worried-divisors of the monkeys and use that. Thus our number is
;;; kept smaller but still 'proportional' (if that is the right word) so that
;;; every monkey worried function still gives the same answer after we make the
;;; number smaller.
;;;
;;; (had to get this idea from hints in reddit)

(defmethod inspect-item ((monkey no-worry-decrease-monkey) item &rest extra-args)
  (let ((common-divisor (first extra-args)))
    (prog1  (mod (funcall (delta monkey) item) common-divisor)
      (incf (num-inspections monkey)))))

(defun take-turn (monkey monkeys)
  (let ((common-worry-divisor (aoc:product (mapcar #'worried-divisor monkeys))))
    (flet ((inspect-item (worry)  (inspect-item monkey worry common-worry-divisor))
           (worried-p (worry) (zerop (mod worry (worried-divisor monkey))))
           (if-worried (item) (funcall (if-worried monkey) item monkeys))
           (if-not-worried (item) (funcall (if-not-worried monkey) item monkeys)))
      (let ((items (mapcar #'inspect-item (items monkey))))
        (setf (items monkey) (list))
        (dolist (item items)
          (if (worried-p item) (if-worried item) (if-not-worried item)))))))

(defun do-round (monkeys)
  (dolist (monkey monkeys)
    (take-turn monkey monkeys)))

(defun make-monkey (num items delta worried if-true if-false)
  (flet ((throw-to-monkey (n) (lambda (item monkeys)(throw-to (nth n monkeys) item)))
         (call-delta (op arg) (lambda (old) (funcall op old arg))))
    (make-instance 'monkey
                   :num num
                   :items items
                   :delta (call-delta (first delta) (second delta))
                   :worried-divisor worried
                   :if-worried (throw-to-monkey if-true)
                   :if-not-worried (throw-to-monkey if-false))))

(defun copy-monkey (monkey)
  (make-instance 'monkey
                 :num (num monkey)
                 :items (copy-seq (items monkey))
                 :delta (delta monkey)
                 :worried-divisor (worried-divisor monkey)
                 :if-worried (if-worried monkey)
                 :if-not-worried (if-not-worried monkey)))

(defun parse-delta-fn (op n)
  (if (string-equal n "old")
      (list (find-symbol "EXPT") 2)
      (list (find-symbol (string-upcase op)) (parse-integer n))))

(defun get-number-at-end-of-str (str &key junk-allowed)
  (parse-integer
   (first (last (aoc:split-string-on-char #\Space str))) :junk-allowed junk-allowed))

(defun parse-monkey (lines)
  (destructuring-bind (monkey-num items delta-op worry-test if-true if-false) lines
    (let ((num (get-number-at-end-of-str monkey-num :junk-allowed t))
          (items (mapcar #'parse-integer
                         (rest (aoc:split-string-on-chars '(#\: #\,) items))))
          (delta-info (apply #'parse-delta-fn (last (aoc:split-string-on-char #\Space delta-op) 2)))
          (worried-num (get-number-at-end-of-str worry-test))
          (true-monkey (get-number-at-end-of-str if-true))
          (false-monkey (get-number-at-end-of-str if-false)))
      (make-monkey num items delta-info worried-num true-monkey false-monkey))))

(defmethod print-object ((obj monkey) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "#~D" (num obj))))

(defun read-data (file)
  (aoc:read-data file
                 :pre-process #'aoc:split-lines-on-empty-line
                 :line-parser #'parse-monkey))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun copy-monkeys (monkeys) (mapcar #'copy-monkey monkeys))

(defun do-n-rounds (monkeys num &key debug)
  (dotimes (n num)
    (when debug (format t "~5,' D " (1+ n)) (when (zerop (mod (1+ n) 10)) (terpri t)))
    (do-round monkeys))
  monkeys)

(defun top-2-busy-monkeys (monkeys)
  (subseq (sort monkeys #'> :key #'num-inspections) 0 2))

(defun part1 (input)
  (let ((monkeys (do-n-rounds (copy-monkeys input) 20)))
    (aoc:product
     (mapcar #'num-inspections (top-2-busy-monkeys monkeys)))))

(5am:def-test part1 (:suite :aoc-2022-11)
  (5am:is (= 10605 (part1 +example+)))
  (5am:is (= 112815 (part1 +input+))))

(defun part2 (input &optional (num-rounds 10000) (debug nil))
  (let ((no-worry-monkeys (mapcar #'(lambda (m) (change-class m 'no-worry-decrease-monkey))
                                  (copy-monkeys input))))
    (aoc:product
     (mapcar #'num-inspections
             (top-2-busy-monkeys (do-n-rounds no-worry-monkeys num-rounds :debug debug))))))

(5am:def-test part2 (:suite :aoc-2022-11)
  (5am:is (= 2713310158 (part2 +example+)))
  (5am:is (= 25738411485 (part2 +input+))))
