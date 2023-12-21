(defpackage #:aoc-2023-20
  (:use :cl))

(in-package #:aoc-2023-20)

(aoc:def-today-suite*)

(defparameter *module-table* (make-hash-table :test #'equalp)
  "A hash table of NAME -> MODULE")

(defun create-module (type name inputs outputs)
  (make-instance type :name name :inputs inputs :outputs outputs))

(defun load-modules-into-table (modules)
  (clrhash *module-table*)
  (mapcar #'(lambda (m) (setf (gethash (name m) *module-table*) m)) modules)

  ;; check for any 'missing' output modules
  (let ((all-outputs (aoc:flatten (mapcar #'outputs modules)))
        (input-map (collate-inputs (mapcar #'(lambda (m) (list (name m) (outputs m))) modules))))
    (dolist (o all-outputs)
      (unless (gethash o *module-table*)
        (setf (gethash o *module-table*)
              (create-module 'output o (gethash o input-map) nil))))))

(defparameter *pulse-queue* (list))
(defun push-on-queue (pulse from to)
  (setf *pulse-queue* (nconc *pulse-queue* (list (list pulse from to)))))
(defun clear-pulse-queue () (setf *pulse-queue* (list)))

(defparameter *pulse-counts*
  (let ((counts (make-hash-table)))
    (setf (gethash :low counts) 0
          (gethash :high counts) 0)
    counts))

(defun reset-counts ()
  (setf (gethash :low *pulse-counts*) 0
        (gethash :high *pulse-counts*) 0))

(defun incf-pulse (pulse)
  (incf (gethash pulse *pulse-counts*)))

(defclass module ()
  ((name :initarg :name :reader name)
   (inputs :initarg :inputs :reader inputs)
   (outputs :initarg :outputs :reader outputs)))

(defmethod print-object ((object module) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "\"~A\"" (slot-value object 'name))))

(defgeneric reset-module (module)
  (:method ((module module))))

(defclass broadcaster (module)
  ())
(defclass conjunction (module)
  ((last-pulse-per-input :accessor last-pulse-per-input)))
(defmethod initialize-instance :after ((instance conjunction) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (last-pulse-per-input instance)
        (reduce #'(lambda (map i) (setf (gethash i map) :low) map)
                (inputs instance)
                :initial-value (make-hash-table :test #'equalp))))
(defmethod reset-module ((instance conjunction))
  (loop for k being the hash-keys of (last-pulse-per-input instance)
        do (setf (gethash k (last-pulse-per-input instance)) :low)))

(defclass flip-flop (module)
  ((state :accessor state :initform :off)))
(defmethod reset-module ((instance flip-flop))
  (setf (state instance) :off))

(defclass output (module)
  ((last-pulse :accessor last-pulse :initform nil)))
(defmethod reset-module ((instance output))
  (setf (pulses instance) nil))

(defgeneric recv-pulse (module pulse from)
  (:documentation
   "Tells MODULE to receive and process a PULSE from module named FROM. This should
return a list of pulses to send or nil. The elements of this list are of the
form: (PULSE FROM TO)"))

(defparameter *trace-pulse* nil)

(defmethod recv-pulse :around ((module module) pulse from)
  (when *trace-pulse* (format *trace-output*
                              "~&SEND ~S FROM ~S TO ~S"
                              pulse from (name module)))
  (incf-pulse pulse)
  (let ((result-pulses (call-next-method)))
    (mapc #'(lambda (pulse-info) (apply #'push-on-queue pulse-info)) result-pulses)
    result-pulses))

(defmethod recv-pulse ((module module) pulse from)
  (mapcar #'(lambda (dest) (list pulse (name module) dest))
          (outputs module)))

(defmethod recv-pulse ((module flip-flop) (pulse (eql :high)) from)
  nil)

(defmethod recv-pulse ((module flip-flop) (pulse (eql :low)) from)
  (if (eq (state module) :off)
      (progn (setf (state module) :on)
             (call-next-method module :high from))
      (progn (setf (state module) :off)
             (call-next-method module :low from))))

(defmethod recv-pulse ((module conjunction) pulse from)
  (let ((last-pulses (last-pulse-per-input module)))
    (setf (gethash from last-pulses) pulse)
    (flet ((is-high-p (p) (eq p :high)))
      (if (every #'is-high-p (alexandria:hash-table-values last-pulses))
          (call-next-method module :low from)
          (call-next-method module :high from)))))

(defmethod recv-pulse ((module output) pulse from)
  (setf (last-pulse module) (list pulse from))
  nil)

(defun find-module (name)
  (or (gethash name *module-table*)
      (error "No module named ~A" name)))

(defun send-pulse (pulse from &rest destinations)
  (mapcar #'(lambda (to) (recv-pulse to pulse from))
          (mapcar #'find-module destinations)))

(defun press-button (&optional *trace-pulse*)
  (push-on-queue :low "button" "broadcaster")
  (loop for pulse-info = (pop *pulse-queue*) then (pop *pulse-queue*)
        while pulse-info
        do (apply #'send-pulse pulse-info)))

(defun get-module-type (name)
  (or (find-class (find-symbol (string-upcase name)) nil)
      (ecase (char name 0)
        (#\% (find-class 'flip-flop))
        (#\& (find-class 'conjunction)))))

(defun parse-module (str)
  (destructuring-bind (name &rest outputs)
      (aoc:split-string-on-chars '(#\Space #\- #\> #\,) str)
    (let ((type (get-module-type name))
          (clean-name (string-trim '(#\& #\%) name)))
      (list clean-name type outputs))))

(defun collate-inputs (name-and-outputs)
  (let ((map (make-hash-table :test #'equalp)))
    (loop for (name outputs) in name-and-outputs
          do (loop for o in outputs
                   do (push name (gethash o map))))
    map))

(defun create-modules (module-data)
  "MODULE-DATA is a list of (NAME TYPE OUTPUTS) tuples. First find mapping of NAME
-> INPUTS and then create the modules. Special case for BUTTON->BROADCASTER
relationship."
  (let ((input-map (collate-inputs
                    (mapcar #'(lambda (d) (list (first d) (third d))) module-data))))
    (setf (gethash "broadcaster" input-map) '("button"))
    (mapcar #'(lambda (n-t-os)
                (create-module (second n-t-os) (first n-t-os)
                               (gethash (first n-t-os) input-map) (third n-t-os)))
            module-data)))

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'parse-module
                 :post-process #'create-modules))

(defparameter +example1+
  (read-data (aoc:today-data-pathname "example1")))

(defparameter +example2+
  (read-data (aoc:today-data-pathname "example2")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

;; design data structure for modules and sub-types of modules
;; queue of pulses to deliver
;; button press causes low pulse to be sent into broadcaster
;; count all pulses sent between modules (separate high and low)
;; press button 1000 times
;; multiple high pulses and low pulses for answer

(defun setup (modules)
  (reset-counts)
  (clear-pulse-queue)
  (mapc #'reset-module modules)
  (load-modules-into-table modules))

(defun part1 (input)
  (setup input)
  (let ((button-count 1000))
    (dotimes (i button-count)
      (press-button)))
  (* (gethash :low *pulse-counts*)
     (gethash :high *pulse-counts*)))

(5am:def-test part1 (:suite :aoc-2023-20)
  (5am:is (= 32000000 (part1 +example1+)))
  (5am:is (= 11687500 (part1 +example1+)))
  (5am:is (= 814934624 (part1 +input+))))

;; do not! use this... very bad idea
;; part1 implement is likely all wrong for this sort of thing.
;; may want to analyze the graph - maybe a least-common-multiple type thing?
(defun part2 ()
  (setup +input+)
  (let ((rx (find-module "rx")))
    (loop for last-pulse = (car (last-pulse rx)) then (car (last-pulse rx))
          until (eq last-pulse :low)
          do (press-button)
          count t into press-count
          when (zerop (mod press-count 100000))
            do (format t "~&~D" press-count)
          finally (return press-count))))

(5am:def-test part2 (:suite :aoc-2023-20)
  (5am:skip ":aoc-2023-20.2 not implemented - need better implementation")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
