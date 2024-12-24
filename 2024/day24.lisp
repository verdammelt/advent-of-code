(defpackage #:aoc-2024-24
  (:use :cl))

(in-package #:aoc-2024-24)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'(lambda (s) (aoc:split-string-on-chars
                                        '(#\: #\Space #\- #\>) s))
                 :post-process #'aoc:split-lines-on-empty-line))

(defun parse-monitor-device (data)
  (list :inputs
        (mapcar #'(lambda (pair) (destructuring-bind (sym value) pair
                                (list (aoc:keywordize sym) (parse-integer value))))
                (first data))
        :formulas
        (mapcar #'(lambda (formula) (destructuring-bind (in1 op in2 out) formula
                                 (mapcar #'aoc:keywordize (list op in1 in2 out))))
                (second data))))

(defparameter +example+
  (parse-monitor-device (read-data (aoc:today-data-pathname "example"))))

(defparameter +input+
  (parse-monitor-device (read-data (aoc:today-data-pathname))))

(defun sym-sort (s1 s2)
  (> (parse-integer (subseq (symbol-name s1) 1))
     (parse-integer (subseq (symbol-name s2) 1))))

(defun sym-starts-with (char sym)
  (char= char (char (symbol-name sym) 0)))

(defun op (gate) (first gate))
(defun inputs (gate) (list (second gate) (third gate)))
(defun output (gate) (fourth gate))

(defun number->inputs (char n num)
  (do ((bits (nreverse (mapcar #'digit-char-p (coerce (format nil "~v,'0B" num n) 'list)))
             (cdr bits))
       (num 0 (incf num))
       (inputs (list)))
      ((null bits) inputs)
    (push (list (aoc:keywordize (format nil "~C~2,'0D" char num)) (car bits))
          inputs)))

(defun num-output-digits-in-machine (device)
  (length (remove-if-not #'(lambda (sym) (sym-starts-with #\Z sym))
                         (mapcar #'output (getf device :formulas)))))

(defun compute-inputs (x y num)
  (append (number->inputs #\X x num) (number->inputs #\Y y num)))

(defun run-computation (inputs formulas)
  (let ((z-outputs (remove-if-not #'(lambda (sym) (sym-starts-with #\Z sym))
                                  (mapcar #'output formulas)))
        (values (make-hash-table))
        (z-values (list)))

    (dolist (initial-value inputs)
      (setf (gethash (first initial-value) values) (second initial-value)))

    (flet ((all-zs-settled ()
             (every #'(lambda (z-sym) (gethash z-sym values)) z-outputs)))
      (setf z-values
            (do ((done (all-zs-settled) (all-zs-settled)))
                (done (mapcar #'(lambda (z-sym) (list z-sym (gethash z-sym values)))
                              z-outputs))

              (mapc #'(lambda (formula)
                        (destructuring-bind (op in1 in2 out) formula
                          (unless (or (gethash out values)
                                      (not (gethash in1 values))
                                      (not (gethash in2 values)))
                            (setf (gethash out values)
                                  (funcall (case op
                                             (:xor #'logxor)
                                             (:and #'logand)
                                             (:or #'logior))
                                           (gethash in1 values)
                                           (gethash in2 values))))))
                    formulas))))

    (parse-integer
     (format nil "~{~D~}" (mapcar #'second (sort z-values #'sym-sort :key #'first)))
     :radix 2)
    ))

(defun part1 (input)
  (run-computation (getf input :inputs) (getf input :formulas)))

(5am:def-test part1 (:suite :aoc-2024-24)
  (5am:is (= 2024 (part1 +example+)))
  (5am:is (= 42410633905894 (part1 +input+))))

(defun find-bad-adders (device)
  (do ((max (num-output-digits-in-machine device))
       (n 0 (incf n))
       (bads (list)))
      ((>= n max) (nreverse bads))
    (when (/= (expt 2 n)
              (run-computation (compute-inputs (expt 2 n) 0 (1- max))
                               (getf device :formulas)))
      (push n bads))))

(defun zs-not-from-xor (formulas)
  (remove-if-not #'(lambda (f) (and (not (eq :xor (op f)))
                               (sym-starts-with #\Z (output f))))
                 formulas))

(defun bad-xors (formulas)
  (remove-if-not
   #'(lambda (f) (and (eq :xor (op f))
                 (not (sym-starts-with #\Z (output f)))
                 (not (or (and (sym-starts-with #\X (first (inputs f)))
                               (sym-starts-with #\Y (second (inputs f))))
                          (and (sym-starts-with #\Y (first (inputs f)))
                               (sym-starts-with #\X (second (inputs f))))))))
   formulas))

(defun op->shape (op)
  (case op
    (:xor "invhouse")
    (:and "invtrapezium")
    (:or "invtriangle")))

(defun device->dot (device output-pathname)
  (let ((formulas (getf device :formulas)))
    (with-open-file (str output-pathname :direction :output :if-exists :supersede)
      (format str "digraph device {~&")
      (dolist (formula formulas)
        (let ((op-node (symbol-name (gensym (symbol-name (op formula))))))
          (format str "~A [label=\"~A\",shape=\"~A\"];~&"
                  op-node (symbol-name (op formula)) (op->shape (op formula)))
          (format str "{~{~A ~}} -> ~A;~&"
                  (mapcar #'symbol-name (inputs formula))
                  op-node)
          (format str "~A -> ~A;~&" op-node (symbol-name (output formula)))))
      (format str "}~&"))
    output-pathname))

(defun show-dot (dotfile)
  (uiop:run-program (list "dot" "-T" "png" "-O" (namestring dotfile)))
  (uiop:run-program (list "open" (format nil "~A.png" (namestring dotfile)))))

;; with lots of help from
;; 1. https://www.reddit.com/r/adventofcode/comments/1hla5ql/2024_day_24_part_2_a_guide_on_the_idea_behind_the/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;; 2. https://en.wikipedia.org/wiki/Adder_(electronics)#Ripple-carry_adder
(defun part2 (input)
  (let* ((formulas (getf input :formulas))
         (zs (remove-if-not #'(lambda (sym) (sym-starts-with #\Z sym))
                            (mapcar #'output formulas)))
         (max-z (aoc:keywordize (format nil "Z~2,'0D" (1- (length zs)))))
         (zs-not-from-xors (zs-not-from-xor formulas))
         (bad-adders (remove-if
                     #'(lambda (n) (member (aoc:keywordize (format nil "Z~2,'0D" n))
                                      zs-not-from-xors :key #'output))
                     (find-bad-adders input)))
         (bad-x-adders (mapcar #'(lambda (n) (aoc:keywordize (format nil "X~2,'0D" n))) bad-adders))
         (bad-y-adders (mapcar #'(lambda (n) (aoc:keywordize (format nil "Y~2,'0D" n))) bad-adders)))

    (format nil
            "~{~A~^,~}"
            (sort
             (mapcar
              #'string-downcase
              (mapcar
               #'symbol-name
               (mapcar
                #'output
                (append
                 ;; any formula with Z as output must be an XOR
                 (remove max-z  zs-not-from-xors :key #'output)

                 ;; any formula without Z as output *and* not X/Y input must NOT be XOR
                 (bad-xors formulas)

                 ;; if X = 2^n and Y = 0 then Z = 2^n find all x where this doesn't work
                 (remove-if-not #'(lambda (f) (or (member (first (inputs f)) bad-x-adders)
                                             (member (first (inputs f)) bad-y-adders)))
                                formulas)))))
             #'string<))))

(5am:def-test part2 (:suite :aoc-2024-24)
  (5am:is (string= "cqm,mps,vcv,vjv,vwp,z13,z19,z25" (part2 +input+))))
