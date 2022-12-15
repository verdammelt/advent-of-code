(defpackage #:aoc-2015-07
  (:use :cl))

(in-package #:aoc-2015-07)

(aoc:def-today-suite*)

(defun number-or-keyword (str)
  (let ((number (parse-integer str :junk-allowed t)))
    (or number (aoc:keywordize str))))

(defun parse-target (target)
  "TARGET is of the form '> WIRE'. So simply return WIRE as a keyword"
  (aoc:keywordize (subseq target 2)))

(defun parse-input (input)
  "Parse the INPUT string. It could be a single value or a function call in the
form 'WIRE OP WIRE'"
  (let ((parts (aoc:split-string-on-char #\Space input)))
    (ecase (length parts)
      (1 (number-or-keyword (first parts)))                   ;; VALUE
      (2 (list (aoc:keywordize (first parts)) (aoc:keywordize (second parts)))) ;; NOT
      (3 (let ((op (aoc:keywordize (second parts)))                         ;; OTHER
               (args (list (first parts) (third parts))))
           (append (list op) (ecase op
                               (:lshift (list (aoc:keywordize (first args))
                                              (parse-integer (second args))))
                               (:rshift (list (aoc:keywordize (first args))
                                              (parse-integer (second args))))
                               (:and (list (number-or-keyword (first args))
                                           (number-or-keyword (second args))))
                               (:or (list (number-or-keyword (first args))
                                          (number-or-keyword (second args)))))))))))

(defun parse-connection (str)
  (destructuring-bind (input target) (aoc:split-string-on-char #\- (string-upcase str))
    (cons (parse-target target) (parse-input input))))

(defun read-data (file) (aoc:read-data file
                                       :pre-process #'(lambda (ls) (remove-if #'aoc:empty-string-p ls))
                                       :line-parser #'parse-connection))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defclass circuit () ((wires :reader wires :initform (make-hash-table))))

(defun load-circuit (circuit connections)
  (reduce #'(lambda (wires conn) (setf (gethash (car conn) wires) (cdr conn)) wires)
          connections
          :initial-value (wires circuit))
  circuit)

(defun is-value-p (value)
  (numberp value))

(defun get-value (wire circuit)
  (if (is-value-p wire) wire
      (multiple-value-bind (value present-p)
          (gethash wire (wires circuit))
        (when (not present-p) (error "Unknown wire ~S in circuit ~S" wire circuit))
        (cond ((is-value-p value) value)
              ((keywordp value) (get-value value circuit))
              (t nil)))))

(defun dump-circuit (circuit)
  (let ((wires (list)))
    (maphash #'(lambda (k v) (let ((value (if (is-value-p v) (mod v (expt 2 16)) v)))
                               (push (cons k value) wires))) (wires circuit))
    (sort wires #'string< :key #'(lambda (w) (symbol-name (car w))))))

(defun circuit-complete (circuit)
  (let ((all-resolved-p t))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (not (is-value-p v)) (setf all-resolved-p nil)))
             (wires circuit))
    all-resolved-p))

(defun step-circuit (circuit)
  (let ((wires (wires circuit)))
    (maphash #'(lambda (w v)
                 (cond ((is-value-p v) nil)
                       ((listp v)
                        (let ((new-value (resolve (car v) (cdr v) circuit)))
                          (when new-value (setf (gethash w wires) new-value))))
                       (t (let ((new-value (get-value v circuit)))
                            (when new-value (setf (gethash w wires) new-value))))))
             (wires circuit))))

(defgeneric resolve (op args curcuit))
(defmethod resolve ((op number) args circuit) nil)
(defmethod resolve ((op (eql :and)) args circuit)
  (let ((values (mapcar #'(lambda (w) (get-value w circuit)) args)))
    (when (every #'is-value-p values)
      (apply #'logand values))))
(defmethod resolve ((op (eql :or)) args circuit)
  (let ((values (mapcar #'(lambda (w) (get-value w circuit)) args)))
    (when (every #'is-value-p values)
      (apply #'logior values))))
(defmethod resolve ((op (eql :lshift)) args circuit)
  (let ((value (get-value (first args) circuit)))
    (when value (ash value (second args)))))
(defmethod resolve ((op (eql :rshift)) args circuit)
  (let ((value (get-value (first args) circuit)))
    (when value (ash value (* -1 (second args))))))
(defmethod resolve ((op (eql :not)) args circuit)
  (let ((value (get-value (first args) circuit)))
    (when value (lognot value))))

(defun part1 (input &optional (limit nil))
  (do ((circuit (load-circuit (make-instance 'circuit) input))
       (count 0 (incf count)))
      ((or (circuit-complete circuit)
           (and limit (> count limit))) ;; safety check
       (dump-circuit circuit))
    (step-circuit circuit)))

(5am:def-test part1 (:suite :aoc-2015-07)
  (5am:is (equal '((:d . 72)
                   (:e . 507)
                   (:f . 492)
                   (:g . 114)
                   (:h . 65412)
                   (:i . 65079)
                   (:x . 123)
                   (:y . 456))
                 (part1 +example+)))
  (5am:is (equal 956 (cdr (assoc :a (part1 +input+))))))

(defun part2 (input)
  (let ((first-signal-on-a (cdr (assoc :a (part1 input))))
        (copy-input (copy-tree input)))
    (setf (cdr (assoc :b copy-input)) first-signal-on-a)
    (part1 copy-input)))

(5am:def-test part2 (:suite :aoc-2015-07)
  (5am:is (= 40149(cdr (assoc :a (part2 +input+))))))
