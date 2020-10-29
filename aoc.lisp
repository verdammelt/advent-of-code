(ql:quickload "uiop")

(defun read-data (file)
  (with-open-file (stream file :direction :input)
    (uiop:slurp-stream-string stream)))

(defun read-data-lines (file)
  (with-open-file (stream file :direction :input)
    (uiop:slurp-stream-lines stream)))

(defun day-data-file (day)
  (make-pathname :name (format nil "day~D" day) :type "dat"))

(defun parsed-day-data (day parser)
  (mapcar parser (read-data-lines (day-data-file day))))

(defun with-only-first-input (fn)
  (lambda (inputs) (funcall fn (first inputs))))

(defun perform-day-task (day task &optional (parser #'identity))
  (format t "Performing task ~S for Day ~D... " task day)
  (format t "~&~A~&" (funcall task (parsed-day-data day parser))))

(defun what-floor (input)
  (let ((char-hash (reduce #'(lambda (acc c) (incf (gethash c acc 0)) acc) input
                           :initial-value (make-hash-table))))
    (- (gethash #\( char-hash 0) (gethash #\) char-hash 0))))

(let ((test-data '(("(())" . 0)
                   ("()()" . 0)
                   ("(((" . 3)
                   ("(()(()(" . 3)
                   ("))(((((" . 3)
                   ("())" . -1)
                   ("))(" . -1)
                   (")))" . -3)
                   (")())())" . -3))))
  (loop for (input . floor) in test-data
        do (assert (= (what-floor input) floor))))

(perform-day-task 1 (with-only-first-input #'what-floor))

(defun when-floor (input &optional (target-floor -1))
  (let ((current-floor 0))
    (1+ (position-if #'(lambda (delta) (= target-floor (incf current-floor delta)))
                     input
                     :key (lambda (c) (case c (#\( 1) (#\) -1)))))))

(let ((test-data '((")" . 1)
                   ("()())" . 5))))
  (loop for (input . position) in test-data
        do (assert (= (when-floor input) position))))

(perform-day-task 1 (with-only-first-input #'when-floor))

(ql:quickload "split-sequence")

(defun surface-area (l w h)
  (+ (* 2 l w)
     (* 2 w h)
     (* 2 h l)))

(defun volume (l w h)
  (* l w h))

(defun smallest-side-dimensions (dimensions)
  (subseq (sort dimensions #'<) 0 2))

(defun a-little-extra (dimensions)
  (apply #'* (smallest-side-dimensions dimensions)))

(defun square-feet-of-paper (dimensions)
  (+ (apply #'surface-area dimensions)
     (a-little-extra dimensions)))

(defun parse-to-dimensions (line)
  (mapcar #'parse-integer (split-sequence:split-sequence #\x line)))

(defun total-square-feet (inputs)
  (apply #'+ (mapcar #'square-feet-of-paper inputs)))

;; (let ((test-data '((("2x3x4") . 58)
;;                    (("1x1x10") . 43)
;;                    (("2x3x4" "1x1x10") . #.(+ 58 43)))))
;;   (loop for (line . sqft-paper) in test-data
;;         do (assert (= (total-square-feet (mapcar parse-to-dimensions line)) sqft-paper))))

(defun smallest-perimeter (dimensions)
  (* 2 (apply #'+ (smallest-side-dimensions dimensions))))

(defun perfect-bow-size (dimension)
  (apply #'volume dimension))

(defun ribbon-length (dimension)
  (+ (smallest-perimeter dimension)
     (perfect-bow-size dimension)))

(defun total-ribbon-size (lines)
  (apply #'+ (mapcar #'ribbon-length (mapcar #'parse-to-dimensions lines))))

;; (let ((test-data '((("2x3x4") . 34)
;;                    (("1x1x10") . 14)
;;                    (("2x3x4" "1x1x10") . #.(+ 34 14)))))
;;   (loop for (line . ribbon-size) in test-data
;;         do (assert (= (total-ribbon-size line) ribbon-size))))
