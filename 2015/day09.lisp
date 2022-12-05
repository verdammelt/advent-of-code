(defpackage #:aoc-2015-09
  (:use :cl))

(in-package #:aoc-2015-09)

(aoc:def-today-suite*)

(defun make-route (from to cost)
  ;; (list from to cost)
  (cons (list from to) cost)
  )

(defun lookup-route (from to routes)
  ;; (find-if #'(lambda (r) (or (and (eq from (first r))
  ;;                            (eq to (second r)))
  ;;                       (and (eq to (first r)) ;; should set up routes so this is not needed
  ;;                            (eq from (second r)))))
  ;;          routes)
  (or (assoc (list from to) routes :test #'equal)
      (assoc (list to from) routes :test #'equal)))

(defun route-cost (route)
  ;; (third route)
  (cdr route)
  )

(defun cities-in-route (route)
  ;; (list (first route) (second route))
  (car route)
  )

(defun process-edge-string (str)
  (let ((parts (aoc:split-string-on-char #\Space str)))
    (make-route (aoc:keywordize (first parts))
                (aoc:keywordize (third parts))
                (parse-integer (fifth parts)))))

(defun read-data (file) (aoc:read-data file
                                       :pre-process #'(lambda (ls) (remove-if #'aoc:empty-string-p ls))
                                       :line-parser #'process-edge-string))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun cities (routes)
  (remove-duplicates (apply #'append (mapcar #'cities-in-route routes))))

(defun all-permutations (list)
  "Return permutations of LIST
(Taken from https://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp)"
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

(defun cost-lookup (from to routes)
  (if (or (null from) (null to)) 0
      (route-cost (lookup-route from to routes))))

(defun measure-route (path routes)
  (loop for (from to) on path by #'cdr
        sum (cost-lookup from to routes)))

(defun all-routes-with-costs (routes)
  (mapcar #'(lambda (p) (list (measure-route p routes) p))
          (all-permutations (cities routes))))

(defun shortest-path (routes)
  (first (sort (all-routes-with-costs routes) #'< :key #'first)))

(defun part1 (input)
  (first (shortest-path input)))

(5am:def-test part1 (:suite :aoc-2015-09)
  (5am:is (= 605 (part1 +example+)))
  (5am:is (= 207 (part1 +input+))))

(defun longest-path (routes)
  (first (sort (all-routes-with-costs routes) #'> :key #'first)))

(defun part2 (input) (first (longest-path input)))

(5am:def-test part2 (:suite :aoc-2015-09)
  (5am:is (= 982 (part2 +example+)))
  (5am:is (= 804 (part2 +input+))))
