(defpackage #:aoc-2021-18
  (:use :cl))

(in-package #:aoc-2021-18)

(aoc:def-today-suite*)

(defun pair-left (n) (first n))
(defun pair-right (n) (second n))
(defun is-regular-p (n) (numberp n))
(defun is-pair-p (n) (consp n))
(defun is-pair-of-regular-p (n) (and (consp n) (every #'is-regular-p n)))

(defun map-number-nodes (fn num)
  (let ((new-num num))
    (labels ((descend (tree depth)
               (cond ((null tree) nil)
                     ((or (is-regular-p tree)
                          (is-pair-of-regular-p tree))
                      (setq new-num (nsubst (funcall fn tree depth)
                                            tree
                                            new-num
                                            :test #'eq)))
                     (t (descend (pair-left tree) (1+ depth))
                        (descend (pair-right tree) (1+ depth))))))
      (descend num 0)
      new-num)))

(defun parse-snailfish-number (s)
  (let ((substitutions '((#\[ . #\()
                         (#\] . #\))
                         (#\, . #\Space))))
    (read-from-string
     (map 'string
          #'(lambda (c) (or (cdr (assoc c substitutions)) c))
          s))))

(defun read-data (file)
  (aoc:read-data file :line-parser #'parse-snailfish-number))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))


(defun add-regular (reg n) (when reg (+ reg n)))

(defun visit-leaves (num fn)
  (labels ((descend (tree depth)
             (cond ((null tree) nil)
                   ((or (is-regular-p tree)
                        (is-pair-of-regular-p tree))
                    (funcall fn tree depth))
                   (t (descend (pair-left tree) (1+ depth))
                      (descend (pair-right tree) (1+ depth))))))
    (descend num 0)))

(defun change-leaves (num changes)
  (reduce #'(lambda (n c) (subst (second c) (first c) n)) changes :initial-value num))

(defun is-explode-p (n d) (and (= d 4) (is-pair-of-regular-p n)))

(defun reduce-explode (num)
  "Reduces the left-most explode if found. Returns (VALUES REDUCED-N REDUCED-P)."
  (let ((previous-regular nil)
        (explode nil)
        (next-regular nil))
    (visit-leaves num #'(lambda (n d)
                          (cond ((and (not explode) (is-regular-p n))
                                 (setq previous-regular n))
                                ((and explode (not next-regular) (is-regular-p n))
                                 (setq next-regular n))
                                ((and (not explode) (is-explode-p n d))
                                 (setq explode n)))))
    (if explode
        (values (change-leaves num (list (list explode 0)
                                         (list previous-regular
                                               (add-regular previous-regular (pair-left explode)))
                                         (list next-regular
                                               (add-regular next-regular (pair-right explode)))))
                t)
        (values num nil))))

(5am:def-test explode (:suite :aoc-2021-18)
  (5am:is (equal (parse-snailfish-number "[[[[0,9],2],3],4]")
                 (reduce-explode (parse-snailfish-number "[[[[[9,8],1],2],3],4]"))))
  (5am:is (equal (parse-snailfish-number "[7,[6,[5,[7,0]]]]")
                 (reduce-explode (parse-snailfish-number "[7,[6,[5,[4,[3,2]]]]]"))))
  (5am:is (equal (parse-snailfish-number "[[6,[5,[7,0]]],3]")
                 (reduce-explode (parse-snailfish-number "[[6,[5,[4,[3,2]]]],1]"))))
  (5am:is (equal (parse-snailfish-number "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
                 (reduce-explode (parse-snailfish-number "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))))
  ;; (5am:is (equal (parse-snailfish-number "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  ;;                (reduce-explode (parse-snailfish-number "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))))

  ;; ;; from addition example
  ;; (5am:is (equal (parse-snailfish-number "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")
  ;;                (reduce-explode (parse-snailfish-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))))
  ;; (5am:is (equal (parse-snailfish-number "[[[[0,7],4],[15,[0,13]]],[1,1]]")
  ;;                (reduce-explode (parse-snailfish-number "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"))))
  ;; (5am:is (equal (parse-snailfish-number "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  ;;                (reduce-explode (parse-snailfish-number "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"))))
  )

(defun reduce-split (num)
  "Reduces the left-most split if found. Returns (VALUES REDUCED-N REDUCED-P"
  (let ((split nil))
    (visit-leaves num #'(lambda (n d) (declare (ignore d))
                          (if (and (is-regular-p n)
                                   (not split)
                                   (>= n 10))
                              (setq split n))))
    (if split
        (values (change-leaves num (list split
                                         (list (floor split 2)
                                               (ceiling split 2)))))
        (values num nil))))

(defun reduce-snailfish-number (n)
  (multiple-value-bind (reduced was-reduced-p) (reduce-explode n)
    (if (not was-reduced-p)
        (multiple-value-bind (reduced was-reduced-p) (reduce-split n)
          (if (not was-reduced-p) n
              (reduce-snailfish-number reduced)))
        (reduce-snailfish-number reduced))))

(defun add (n1 n2)
  (reduce-snailfish-number (list n1 n2)))

;; (5am:def-test add (:suite :aoc-2021-18)
;;   (5am:is (equal (parse-snailfish-number "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
;;                  (add (parse-snailfish-number "[[[[4,3],4],4],[7,[[8,4],9]]]")
;;                       (parse-snailfish-number "[1,1]")))))

(defun part1 (input) (declare (ignore input)) 0)

;; TODO: complete 2021-18.1
(5am:def-test part1 (:suite :aoc-2021-18)
  (5am:skip ":aoc-2021-18.1 not implemented")
  ;; (5am:is (= -1 (part1 +example+)))
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

;; TODO: complete 2021-18.2
(5am:def-test part2 (:suite :aoc-2021-18)
  (5am:skip ":aoc-2021-18.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
