(in-package #:aoc-2015)

(defun parse-list (lines) lines)

(defun vowel-p (c)
  (member c '(#\a #\e #\i #\o #\u) :test 'char=))

(defun has-enough-vowels-p (string)
  (<= 3 (length (remove-if-not #'vowel-p string))))

(defun has-doubled-letter (string)
  (let ((all-letters (remove-duplicates string)))
    (loop
       for c across all-letters
       when (search (make-string 2 :initial-element c) string)
       do (return t))))

(defparameter *verboten-words* '("ab" "cd" "pq" "xy"))

(defun has-verboten-words-p (string)
  (some #'(lambda (sub-string) (search sub-string string)) *verboten-words*))

(defun is-nice-p (string)
  (and (has-enough-vowels-p string)
       (has-doubled-letter string)
       (not (has-verboten-words-p string))))

(let ((inputs '("ugknbfddgicrmopn" "aaa" "jchzalrnumimnmhp"
                "haegwjzuvuyypxyu" "dvszwmarrgswjxmb"))
      (naughty-list '("jchzalrnumimnmhp" "haegwjzuvuyypxyu" "dvszwmarrgswjxmb")))
  (assert (equal naughty-list (remove-if #'is-nice-p inputs))))

(defun count-if-nice (strings)
  (count-if #'is-nice-p strings))

(defun group-by (seq &key (key #'identity) (hash-test #'eql))
  (let ((hash (make-hash-table :test hash-test)))
    (loop for x across (coerce seq 'vector)
          do (push x (gethash (funcall key x) hash (list))))
    hash))

(defun hash-reduce (reducer hash-table)
  (let ((new-hash (make-hash-table :test (hash-table-test hash-table)
                                   :size (hash-table-size hash-table)
                                   :rehash-size (hash-table-rehash-size hash-table)
                                   :rehash-threshold (hash-table-rehash-threshold hash-table))))
    (maphash #'(lambda (k v) (setf new-hash (funcall reducer new-hash k v))) hash-table)
    new-hash))

(defun has-pair-of-pair-not-overlapping-p (string)
  " 'xxyzxx' => T but 'xxx' => NIL (overlapping)"

  ;; (let* ((pairs-with-index
  ;;          (loop for i from 0
  ;;                and x across string
  ;;                and y across (subseq string 1)
  ;;                collect (list i (concatenate 'string (list x y)))))
  ;;        (grouped-pairs (group-by pairs-with-index :key #'second :hash-test #'equal))
  ;;        (just-the-indexes (hash-reduce
  ;;                           #'(lambda (h k v) (setf (gethash k h (list)) (mapcar #'first v)) h)
  ;;                           grouped-pairs)))
  ;;   (let ((nice nil))
  ;;     (maphash #'(lambda (pair indexes)
  ;;                  (when (find-if #'(lambda (idx) (> (- (first indexes) idx) 2)) (rest indexes))
  ;;                    (push pair nice)))
  ;;              just-the-indexes)
  ;;     nice))
  (loop for i from 0 upto (- (length string) 2)
        for pair = (subseq string i (+ i 2)) then (subseq string i (+ i 2))
        when (search pair (subseq string (+ i 2))) do (return :pair-found)))

(defun eql-with-null-wildcard (c1 c2)
  (if (member #\Null (list c1 c2)) t
      (eql c1 c2)))

(defun has-repeat-with-one-space-gap-p (string)
  (let ((all-letters (remove-duplicates string)))
    (loop for c across all-letters
          for pattern = (format nil "~C~C~C" c #\Null c)
          when (search pattern string :test #'eql-with-null-wildcard)
            do (return t))))

(defun new-is-nice-p (string)
  (and (has-pair-of-pair-not-overlapping-p string)
       (has-repeat-with-one-space-gap-p string)))

(defun count-if-nice-2 (strings)
  (count-if #'new-is-nice-p strings))

(let ((inputs '("qjhvhtzxzqqjkmpb" "xxyxx" "uurcxstgmygtbstg" "ieodomkazucvgmuy"))
      (nice-list '("qjhvhtzxzqqjkmpb" "xxyxx"))
      (naughty-list '("uurcxstgmygtbstg" "ieodomkazucvgmuy")))

  (assert (new-is-nice-p "xyxy"))

  (assert (equal naughty-list (remove-if #'new-is-nice-p inputs)))
  (assert (equal nice-list (remove-if-not #'new-is-nice-p inputs))))
