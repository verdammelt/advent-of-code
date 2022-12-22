(defpackage #:aoc-2022-20
  (:use :cl))

(in-package #:aoc-2022-20)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :line-parser #'parse-integer))

(defun %circular-buffer-calc-idx (buffer idx)
  (mod idx (length buffer)))

(defun circular-buffer-nth (buffer n)
  "Get the NTH item from the circular BUFFER."
  (elt buffer (%circular-buffer-calc-idx buffer n)))

(defun circular-buffer-position (buffer item)
  "Find the index of ITEM in BUFFER."
  (position item buffer))

(defun circular-buffer-move (buffer from steps)
  "Move an item in BUFFER from position FROM by the number of steps STEPS."
  (let* ((item (circular-buffer-nth buffer from))
         (without-item (append (subseq buffer 0 from)
                               (subseq buffer (1+ from))))
         (to (%circular-buffer-calc-idx without-item (+ from steps))))
    (append (subseq without-item 0 to)
            (list item)
            (subseq without-item to))))

(defun circular-buffer-rotate (buffer steps)
  "Rotate entire BUFFER STEPS number of times"
  (let ((n (%circular-buffer-calc-idx buffer steps)))
    (append (subseq buffer n)
            (subseq buffer 0 n))))

(5am:test circular-buffer
  (let ((buffer '(1 2 3 4 5)))
    (5am:is (= 0 (circular-buffer-position buffer 1)))
    (5am:is (= 2 (circular-buffer-position buffer 3)))

    (5am:is (= 1 (circular-buffer-nth buffer 0)))
    (5am:is (= 2 (circular-buffer-nth buffer 1)))
    (5am:is (= 5 (circular-buffer-nth buffer 4)))
    (5am:is (= 1 (circular-buffer-nth buffer 5)))
    (5am:is (= 4 (circular-buffer-nth buffer 23)))

    (5am:is (equal '(2 1 3 4 5) (circular-buffer-move buffer 0 1)))
    (5am:is (equal '(1 3 4 2 5) (circular-buffer-move buffer 1 2)))
    (5am:is (equal '(1 2 4 3 5) (circular-buffer-move buffer 3 3)))
    (5am:is (equal '(1 2 4 3 5) (circular-buffer-move buffer 2 -3)))

    (5am:is (equal '(5 1 2 3 4) (circular-buffer-rotate buffer -1)))
    (5am:is (equal '(2 3 4 5 1) (circular-buffer-rotate buffer 1)))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun %mix (with-ids ids)
  (dolist (item with-ids)
    (destructuring-bind (id . step) item
      (setf ids (circular-buffer-move ids (circular-buffer-position ids id) step))))
  ids)

(defun decode-input (input &optional (n-times 1))
  (let* ((with-ids (mapcar #'(lambda (n) (cons (gensym) n)) input))
         (ids (mapcar #'car with-ids)))
    (dotimes (n n-times)
      (setf ids (%mix with-ids ids)))
  (mapcar #'(lambda (id) (cdr (assoc id with-ids))) ids)))

(5am:test decode-input
  (5am:is (equal '(-2 1 2 -3 4 0 3) (decode-input +example+))))

(defun extract-coordinates (decoded)
  (let ((zero-idx (circular-buffer-position decoded 0)))
    (list (circular-buffer-nth decoded (+ zero-idx 1000))
          (circular-buffer-nth decoded (+ zero-idx 2000))
          (circular-buffer-nth decoded (+ zero-idx 3000)))))

(defun part1 (input)
  (aoc:sum (extract-coordinates (decode-input input))))

(5am:def-test part1 (:suite :aoc-2022-20)
  (5am:is (= 3 (part1 +example+)))
  (5am:is (= 8028 (part1 +input+))))

(defun decode-after-applying-key (input n-times)
  (let* ((decryption-key 811589153)
         (key-applied (mapcar #'(lambda (x) (* x decryption-key)) input)))
    (decode-input key-applied n-times)))

(5am:test decode-after-applying-key
  (flet ((rotate-for-zero-start (cb)
           "Rotate buffer so that it will start with 0"
           (circular-buffer-rotate cb (- (circular-buffer-position cb 0)
                                         (length cb)))))
    (5am:is (equal
             '(0 -2434767459 3246356612 -1623178306 2434767459 1623178306 811589153)
             (rotate-for-zero-start (decode-after-applying-key +example+ 1))))

    (5am:is (equal
             '(0 2434767459 1623178306 3246356612 -2434767459 -1623178306 811589153)
             (rotate-for-zero-start (decode-after-applying-key +example+ 2))))

    (5am:is (equal
             '(0 811589153 2434767459 3246356612 1623178306 -1623178306 -2434767459)
             (rotate-for-zero-start (decode-after-applying-key +example+ 3))))

    (5am:is (equal
             '(0 1623178306 -2434767459 811589153 2434767459 3246356612 -1623178306)
             (rotate-for-zero-start (decode-after-applying-key +example+ 4))))

    (5am:is (equal
             '(0 811589153 -1623178306 1623178306 -2434767459 3246356612 2434767459)
             (rotate-for-zero-start (decode-after-applying-key +example+ 5))))

    (5am:is (equal
             '(0 811589153 -1623178306 3246356612 -2434767459 1623178306 2434767459)
             (rotate-for-zero-start (decode-after-applying-key +example+ 6))))

    (5am:is (equal
             '(0 -2434767459 2434767459 1623178306 -1623178306 811589153 3246356612)
             (rotate-for-zero-start (decode-after-applying-key +example+ 7))))

    (5am:is (equal
             '(0 1623178306 3246356612 811589153 -2434767459 2434767459 -1623178306)
             (rotate-for-zero-start (decode-after-applying-key +example+ 8))))

    (5am:is (equal
             '(0 811589153 1623178306 -2434767459 3246356612 2434767459 -1623178306)
             (rotate-for-zero-start (decode-after-applying-key +example+ 9))))

    (5am:is (equal
             '(0 -2434767459 1623178306 3246356612 -1623178306 2434767459 811589153)
             (rotate-for-zero-start (decode-after-applying-key +example+ 10))))))

(defun part2 (input)
  (aoc:sum (extract-coordinates (decode-after-applying-key input 10))))

(5am:def-test part2 (:suite :aoc-2022-20)
  (5am:is (= 1623178306 (part2 +example+)))
  (5am:is (= 8798438007673 (part2 +input+))))
