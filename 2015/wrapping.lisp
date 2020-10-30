(in-package :aoc-2015)

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

(defun parse-to-dimensions (lines)
  (loop for line in lines
     collect (mapcar #'parse-integer (split-sequence:split-sequence #\x line))))

(defun total-square-feet (inputs)
  (apply #'+ (mapcar #'square-feet-of-paper inputs)))

(let ((test-data '((("2x3x4") . 58)
                   (("1x1x10") . 43)
                   (("2x3x4" "1x1x10") . #.(+ 58 43)))))
  (loop for (lines . sqft-paper) in test-data
     do (assert (= (total-square-feet (parse-to-dimensions lines))
                   sqft-paper))))

(defun smallest-perimeter (dimensions)
  (* 2 (apply #'+ (smallest-side-dimensions dimensions))))

(defun perfect-bow-size (dimension)
  (apply #'volume dimension))

(defun ribbon-length (dimension)
  (+ (smallest-perimeter dimension)
     (perfect-bow-size dimension)))

(defun total-ribbon-size (inputs)
  (apply #'+ (mapcar #'ribbon-length inputs)))

(let ((test-data '((("2x3x4") . 34)
                   (("1x1x10") . 14)
                   (("2x3x4" "1x1x10") . #.(+ 34 14)))))
  (loop for (lines . ribbon-size) in test-data
        do (assert (= (total-ribbon-size (parse-to-dimensions lines))
                      ribbon-size))))
