(defpackage #:aoc-2019-14
  (:use :cl))

(in-package #:aoc-2019-14)

(aoc:def-today-suite*)

(defun compose (&rest fns)
  (lambda (input)
    (reduce #'(lambda (v f) (funcall f v))
            (reverse fns)
            :initial-value input)))

(defun mk-keyword (str)
  (intern (string-upcase str) :keyword))

(defun make-term (amount chemical) (list amount chemical))
(defun term-amount (term) (first term))
(defun term-chemical (term) (second term))

(defun hash-table->list (hashtable)
  (let ((result (list)))
    (maphash #'(lambda (k v) (push (list k v) result)) hashtable)
    (nreverse result)))

(defun multiply-equation-by (eq factor)
  (mapcar #'(lambda (term) (make-term (* factor (term-amount term))
                                 (term-chemical term)))
          eq))

(defun load-equations (file)
  (labels ((prepare (eq) (remove #\> eq))
           (eq-delimeter-p (c) (member c '(#\= #\,) :test #'char=))
           (split-equals (eq) (aoc:split-string-on-char #'eq-delimeter-p eq))
           (cleanup-str (s) (string-trim '(#\Space) s))
           (cleanup-eq (eq) (mapcar #'cleanup-str eq))
           (split-terms (eq) (mapcar #'(lambda (s) (aoc:split-string-on-char #\Space s)) eq))
           (parse-term (term) (make-term (parse-integer (term-amount term))
                                         (mk-keyword (term-chemical term))))
           (parse-terms (eq) (mapcar #'parse-term eq))
           (store-equation (acc eq) (let ((output (first eq)))
                                      (setf (gethash (term-chemical output) acc)
                                            eq)
                                      acc)))

    (reduce #'store-equation
            (mapcar (compose #'reverse #'parse-terms #'split-terms
                             #'cleanup-eq #'split-equals #'prepare)
                    (aoc:read-data file))
            :initial-value (make-hash-table))))

(defun collect-terms (eq)
  (let* ((output (first eq))
         (inputs (rest eq))
         (term-hash
          (reduce #'(lambda (hash term)
                      (incf (gethash (term-chemical term) hash 0)
                            (term-amount term))
                       hash)
                  inputs :initial-value (make-hash-table)))
         (new-eq (list output)))
    (maphash #'(lambda (chemical amount) (push (make-term amount chemical) new-eq))
             term-hash)
    (nreverse  new-eq)))

(defun expand-terms (equations)
  (lambda (eq term)
    (let ((eq-for-term (gethash (term-chemical term) equations)))
      (collect-terms
       (if (null eq-for-term)
           (append eq (list term))
           (append eq (rest (multiply-equation-by
                             eq-for-term (term-amount term)))))))))

(defun reduce-to-ore (eq equations)
  (flet ((ore-p (term) (eq (term-chemical term) :ore)))
    (let ((output (first eq))
          (inputs (rest eq)))
      (cond ((every #'ore-p inputs) eq)
            (t (reduce-to-ore (reduce (expand-terms equations)
                                      inputs :initial-value (list output))
                              equations))))))

(defun how-much-ore (equations &optional (target-fuel 1))
  (let ((fuel-equation (gethash :fuel equations)))
    (reduce-to-ore (multiply-equation-by fuel-equation target-fuel) equations)))

;; TODO: complete 2019.14
;; problem not completed
;; (5am:is (= 31 (caadr (how-much-ore (load-equations "./example-31.txt")))))
