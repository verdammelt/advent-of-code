;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (load "../computer")
;;   (load "../file-utils")
;;   (load "../string-utils"))

(defpackage :arcade
  (:use :common-lisp)
  (:export :play))

(in-package :arcade)

(defun load-program (file)
  (mapcar #'parse-integer
          (string-utils:split (first (file-utils:read-lines file)) #\,)))

(defun triples (seq)
  (labels ((grab (n seq acc)
             (cond ((null seq) acc)
                   ((< (length seq) n) (push seq acc))
                   (t (grab n (subseq seq n) (push (subseq seq 0 n) acc))))))
    (nreverse (grab 3 seq (list)))))

(defun parse-output (output)
  (mapcar #'parse-integer (string-utils:split (string-trim '(#\Space) output) #\Space)))

(defclass arcade ()
  ((game :reader game)
   (score :accessor score)
   (screen-state :accessor screen-state)
   (display :accessor display :initform nil)))

(defmethod initialize-instance ((obj arcade) &key program)
  (setf (slot-value obj 'game)
        (computer:make-computer program
                                (make-string-input-stream "")
                                (make-string-output-stream))
        (slot-value obj 'display) nil)
  obj)

(defun score-p (triple)
  (and (= (first triple) -1)
       (= (second triple) 0)))

(defun update-game-state (arcade)
  (let ((game-state (triples (parse-output (computer:get-output (game arcade))))))

    (setf (score arcade) (or (third (find-if #'score-p game-state)) 0)
          (screen-state arcade) (remove-if #'score-p game-state))

    arcade))

(defparameter *display-elements* '(#\Space #\W #\# #\= #\O))
(defun display-element (n) (nth n *display-elements*))

(defun init-display (arcade)
  (let* ((max-x (reduce #'max (screen-state arcade) :key #'first))
         (max-y (reduce #'max (screen-state arcade) :key #'second))
         (output (make-array (list (1+ max-x) (1+ max-y))
                             :initial-element (display-element 0))))
    (setf (display arcade) output)))

(defun to-display-buffer (arcade)
  (unless (display arcade) (init-display arcade))
  (let* ((screen-state (screen-state arcade)))
    (when (> 10 (length screen-state)) (format t "~& short screen state: ~A" screen-state))
    (dolist (cell screen-state)
      (destructuring-bind (x y c) cell
        (setf (aref (display arcade) x y) (display-element c))))))

(defun refresh-screen (arcade)
  (to-display-buffer arcade)
  (let ((display-buffer (display arcade)))
    (format t "~&~&SCORE: ~6,'0D" (score arcade))
    (format t "~&")
    (dotimes (i (array-dimension display-buffer 1))
      (dotimes (j (array-dimension display-buffer 0))
        (format t "~A" (aref display-buffer j i)))
      (format t "~&"))))

(defun get-next-input (arcade)
  (declare (ignore arcade))
  (let ((char (read-char)))
    (case char
      (#\a "-1")
      (#\l "1")
      (t "0"))))

(defmethod computer:run-program ((arcade arcade) &optional additional-input)
  (declare (ignore additional-input))
  (do ((input nil (get-next-input arcade)))
      ((computer:halted-p (game arcade)) arcade)
    (computer::run-program (game arcade)
                           (when input (make-string-input-stream input)))
    (update-game-state arcade)
    (refresh-screen arcade)))

(defun play (game-file &optional free-play)
  (let ((program (load-program game-file)))
    (when free-play (setf (nth 0 program) 2))
    (computer:run-program (make-instance 'arcade :program program))))

(defun max-score (game-file)
  (let* ((screen (triples
                  (parse-output (computer:get-output
                                 (computer:compute (load-program game-file)
                                                   :output-stream (make-string-output-stream))))))
         (blocks (remove-if-not #'(lambda (c) (= 2 c)) screen :key #'third)))
    (reduce #'(lambda (acc cell) (+ acc (first cell) (* 2 (second cell)))) blocks :initial-value 0)))
