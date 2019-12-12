(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../computer")
  (load "../string-utils")
  (load "../file-utils"))

(defpackage :painting
  (:use :cl)
  (:export :monet :how-much-painting))

(in-package :painting)

(defparameter *program*
  (mapcar #'parse-integer
          (string-utils:split
           (first (file-utils:read-lines "./input.txt"))
           #\,)))

(defun make-point (x y) (cons x y))
(defun x (point) (car point))
(defun y (point) (cdr point))

(defclass painter ()
  ((brain :reader brain)
   (canvas :reader canvas :initform (make-hash-table :test #'equal))
   (facing :accessor facing :initform :up :type (member :up :down :left :rigth))
   (location :reader location :initform (make-point 0 0) :type point)))

(defmethod print-object ((obj painter) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~S ~S"
            (slot-value obj 'location)
            (slot-value obj 'facing))))

(defun halted-p (painter)
  (eq :halt (computer:get-state (brain painter))))

(defmethod initialize-instance :after ((instance painter) &key program &allow-other-keys)
  (setf (slot-value instance 'brain)
          (make-instance 'computer:computer
                         :memory program
                         :input-stream (make-string-input-stream "")
                         :output-stream (make-string-output-stream)))
  instance)

(defparameter *turns*
  `(((:up . :left) :left)
    ((:left . :left) :down)
    ((:down . :left) :right)
    ((:right . :left) :up)
    ((:up . :right) :right)
    ((:right . :right) :down)
    ((:down . :right) :left)
    ((:left . :right) :up)))

(defun turn (facing direction)
  (cadr (assoc (cons facing direction) *turns* :test #'equal)))

(defun step-forward (facing location)
  (ecase facing
    (:up (make-point (1+ (x location)) (y location)))
    (:down (make-point (1- (x location)) (y location)))
    (:right (make-point (x location) (1+ (y location))))
    (:left (make-point (x location) (1- (y location))))))

(defun move (painter direction)
  (with-slots (facing location) painter
    (setf facing (turn facing direction)
          location (step-forward facing location))
    painter))

(defun apply-paint (painter color)
  (with-slots (canvas location) painter
    (setf (gethash location canvas) color)))

(defun look (painter)
  (with-slots (canvas location) painter
    (gethash location canvas :black)))

(defun read-color (stream)
  (let ((color (read stream)))
    (ecase color
      (0 :black)
      (1 :white))))

(defun write-color (color)
  (ecase color
    (:black "0")
    (:white "1")))

(defun read-direction (stream)
  (let ((direction (read stream)))
    (ecase direction
      (0 :left)
      (1 :right))))

(defun make-color-stream (color)
  (make-string-input-stream (write-color color)))

(defun handle-io (painter)
  (with-input-from-string (stream (computer:get-output (brain painter)))
    (when (peek-char t stream nil nil)
      (let* ((color-to-paint (read-color stream))
             (direction-to-move (read-direction stream)))
        (apply-paint painter color-to-paint)
        (move painter direction-to-move))))
  (look painter))

(defmethod computer:run-program ((painter painter) &optional additional-input)
  (declare (ignore additional-input))
  (loop
     :for count = 0 then (incf count)
     :until (halted-p painter)
     :do (computer:run-program (brain painter) (make-color-stream (handle-io painter)))
     :finally (return painter)))

(defun monet (&key (program *program*) (starting-color :black))
  (let ((painter (make-instance 'painter :program program)))
    (apply-paint painter starting-color)
    (computer:run-program painter)))

(defun how-much-painting (&key (program *program*))
  (hash-table-count (slot-value (monet :program program) 'canvas)))

;; part I
(assert (= (how-much-painting) 2478))

;; ---------- part II ----------

(defun find-limits (canvas)
  (let ((min-x 0) (min-y 0) (max-x 0) (max-y 0))
    (maphash
     #'(lambda (key value) (declare (ignore value))
          (setf min-x (min min-x (x key))
                min-y (min min-y (y key))
                max-x (max max-x (x key))
                max-y (max max-y (y key))))
     canvas)
    (values min-x min-y max-x max-y)))

(defun calc-dimensions (canvas)
  (multiple-value-bind (min-x min-y max-x max-y)
      (find-limits canvas)
    (list (1+ (- max-x min-x))
          (1+ (- max-y min-y)))))

(defun draw-on-screen (screen)
  #'(lambda (location color)
      (setf (aref screen (abs (x location)) (abs (y location))) color)))

(defun render-color (color)
  (case color
    (:black #\.)
    (:white #\#)))

(defun format-line (size)
  (format t "~&~v@{~A~:*~}~&" size "-"))

(defun render (painter)
  (let* ((canvas (slot-value painter 'canvas))
         (dimensions (calc-dimensions canvas))
         (screen (make-array dimensions :initial-element :black)))
    (maphash (draw-on-screen screen) canvas)
    (loop
       :initially (format-line (second dimensions))
       :for x :below (first dimensions)
       :do (progn (loop
                     :for y :below (second dimensions)
                     :do (format t "~C" (render-color (aref screen x y))))
                  (format t "~&"))
         :finally (format-line (second dimensions)))))

;; -------------------------------------------
;; .#..#..##..####.###..#..#..##...##..####...
;; .#..#.#..#....#.#..#.#..#.#..#.#..#....#...
;; .####.#......#..#..#.#..#.#....#..#...#....
;; .#..#.#.....#...###..#..#.#.##.####..#.....
;; .#..#.#..#.#....#.#..#..#.#..#.#..#.#......
;; .#..#..##..####.#..#..##...###.#..#.####...
;; -------------------------------------------
;; HCZRUGAZ
