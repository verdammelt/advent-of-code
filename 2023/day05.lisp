(defpackage #:aoc-2023-05
  (:use :cl))

(in-package #:aoc-2023-05)

(aoc:def-today-suite*)

(defun parse-map (raw-map)
  "RAW-MAP is in the form (\"SRC-to-DEST-map: \" MAP-ROW+)
MAP-ROW is a list of three integers: DEST-START SRC-START RANGE-lENGTH."
  (destructuring-bind (name &rest rows) raw-map
    (let ((parsed-name (aoc:split-string-on-chars '(#\Space #\-) name))
          (parsed-rows (mapcar #'aoc:string-of-numbers->list-of-numbers rows)))
        `(:from ,(aoc:keywordize (first parsed-name))
          :to ,(aoc:keywordize (third parsed-name))
          :map ,(mapcar #'(lambda (row)
                            (destructuring-bind (dest src range) row
                              (list :src-range (list src (+ src range))
                                    :dest-start dest)))
                        parsed-rows)))))

(defun map-from (map) (getf map :from))
(defun map-to (map) (getf map :to))
(defun map-map (map) (getf map :map))
(defun map-find-row-containing (map value)
  (find-if
   #'(lambda (row)
       (<= (first (getf row :src-range)) value (second (getf row :src-range))))
   (map-map map)))

(defun row-map-value (row value)
  (if row
      (+ (getf row :dest-start) (- value (first (getf row :src-range))))
      value))

(defun map-lookup (map src)
  (let ((row (map-find-row-containing map src)))
    (row-map-value row src)))

(defun parse-almanac (lines)
  "The LINES are ((SEED-DATA) MAP-DATA+)"
  (destructuring-bind ((seeds) &rest maps) (aoc:split-lines-on-empty-line lines)
    `(:seeds ,(aoc:string-of-numbers->list-of-numbers (subseq seeds (length "seeds: ")))
             :maps ,(mapcar #'parse-map maps))))

(defun almanac-seeds (almanac) (getf almanac :seeds))
(defun almanac-maps (almanac) (getf almanac :maps))
(defun almanac-find-map-from (almanac from)
  (or (find-if #'(lambda (map) (and (eq from (map-from map)))) (almanac-maps almanac))
      (error "no map found to ~S" from)))

(defun read-data (file) (aoc:read-data file :post-process #'parse-almanac))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun almanac-lookup (almanac from to item)
  (let* ((map (almanac-find-map-from almanac from))
         (dest (map-to map))
         (dest-item (map-lookup map item)))
    (if (eq dest to)
        dest-item
        (almanac-lookup almanac dest to dest-item))))

(defun almanac-find-seed-locations (almanac &optional (get-seeds #'almanac-seeds))
  (mapcar #'(lambda (seed) (almanac-lookup almanac :seed :location seed))
          (funcall get-seeds almanac)))

(defun part1 (input)
  (apply #'min (almanac-find-seed-locations input)))

(5am:def-test part1 (:suite :aoc-2023-05)
  (5am:is (= 35 (part1 +example+)))
  (5am:is (= 26273516 (part1 +input+))))

(defun almanac-find-seed-locations-super-slow (almanac)
  "WILL NOT WORK ON INPUT FILE (actually will work... eventually...)"
  (time
   (let ((base-seeds (almanac-seeds almanac)))
     (loop for (start length) on base-seeds by #'cddr
           do (format *debug-io* "~2%checking ~A - ~A~%" start (+ start length))
           minimizing
           (prog1 (time (loop for seed from start repeat length
                              minimizing (almanac-lookup almanac :seed :location seed)))
                  (sb-ext:gc))))))

;; TODO: [2023-12-06] Need to implement better solution. It has to do with not
;; mapping individual values but mapping the seed ranges through the maps. At
;; each step a seed range may need to be split to fit entirely into one of the
;; ranges (or entirely out of all ranges)
;; (cf https://www.reddit.com/r/adventofcode/comments/18bimer/comment/kc5ae35/?utm_source=share&utm_medium=web2x&context=3)

(defun part2 (input)
  (almanac-find-seed-locations-super-slow input))

(5am:def-test part2 (:suite :aoc-2023-05)
  (5am:is (= 46 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+))) ;; don't run this until better algorithm coded up.
  )
