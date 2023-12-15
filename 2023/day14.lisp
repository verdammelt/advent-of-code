(defpackage #:aoc-2023-14
  (:use :cl))

(in-package #:aoc-2023-14)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +tilted-example+
  (aoc:lists->2d-array
   '("OOOO.#.O.."
     "OO..#....#"
     "OO..O##..O"
     "O..#.OO..."
     "........#."
     "..#....#.#"
     "..O..#.O.O"
     "..O......."
     "#....###.."
     "#....#....")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defconstant +round-rock+ #\O)
(defconstant +square-rock+ #\#)
(defconstant +empty+ #\.)

(defun platform-num-rows (platform) (array-dimension platform 0))
(defun platform-num-cols (platform) (array-dimension platform 1))
(defun platform-at (platform row col) (aref platform row col))
(defun (setf platform-at) (new-value platform row col)
       (setf (aref platform row col) new-value))
(defun round-rock-p (rock-or-space) (char= rock-or-space +round-rock+))

(defun copy-platform (platform)
  "Duplicate the PLATFORM"
  (alexandria:copy-array platform))

(defun roll-rocks (array start finder setter)
  "ARRAY is a slice of a platform in the direction of movement. Rocks will be
rolled to the lower indices as possible. ARRAY will be modified"
  (if (<= (array-dimension array 0) start)
      array
      (let ((round-rock-at (funcall finder array +round-rock+ start))
            (square-rock-at (funcall finder array +square-rock+ start))
            (empty-spot-at (funcall finder array +empty+ start)))
        (cond ((< empty-spot-at round-rock-at square-rock-at)
               (funcall setter array empty-spot-at +round-rock+)
               (funcall setter array round-rock-at +empty+)
               (roll-rocks array (1+ start) finder setter))
              ((< round-rock-at empty-spot-at square-rock-at)
               (roll-rocks array (1+ start) finder setter))
              ((or (< empty-spot-at square-rock-at)
                   (< round-rock-at square-rock-at))
               (roll-rocks array (1+ square-rock-at) finder setter))
              ((or (< square-rock-at round-rock-at)
                   (< square-rock-at empty-spot-at))
               (roll-rocks array (min round-rock-at empty-spot-at) finder setter))
              (t array)))))

(defun tilt (platform direction)
  "TILT the PLATFORM to send rocks in DIRECTION"
  (cond ((eq direction :north)
         (loop for col below (platform-num-cols platform)
               do (flet ((finder (arr rock start)
                           (or (loop for idx from start below (platform-num-rows arr)
                                     when (char= (platform-at arr idx col) rock)
                                       return idx)
                               most-positive-fixnum))
                         (setter (arr idx rock)
                           (setf (platform-at arr idx col) rock)))
                    (roll-rocks platform 0 #'finder #'setter))
               finally (return platform)))
        ((eq direction :west)
         (loop for row below (platform-num-rows platform)
               do (flet ((finder (arr rock start)
                           (or (loop for idx from start below (platform-num-cols arr)
                                     when (char= (platform-at arr row idx) rock)
                                       return idx)
                               most-positive-fixnum))
                         (setter (arr idx rock)
                           (setf (platform-at arr row idx) rock)))
                    (roll-rocks platform 0 #'finder #'setter))
               finally (return platform)))
        ((eq direction :south)
         (loop for col below (platform-num-cols platform)
               do (labels ((mod-idx (arr idx) (- (1- (platform-num-rows arr)) idx))
                           (finder (arr rock start)
                             (or (loop for idx from start below (platform-num-rows arr)
                                       when (char= (platform-at arr (mod-idx arr idx) col) rock)
                                       return idx)
                               most-positive-fixnum))
                         (setter (arr idx rock)
                           (setf (platform-at arr (mod-idx arr idx) col) rock)))
                    (roll-rocks platform 0 #'finder #'setter))
               finally (return platform)))
        ((eq direction :east)
         (loop for row below (platform-num-rows platform)
               do (labels ((mod-idx (arr idx) (- (1- (platform-num-cols arr)) idx))
                         (finder (arr rock start)
                           (or (loop for idx from start below (platform-num-cols arr)
                                     when (char= (platform-at arr row (mod-idx arr idx)) rock)
                                       return idx)
                               most-positive-fixnum))
                         (setter (arr idx rock)
                           (setf (platform-at arr row (mod-idx arr idx)) rock)))
                    (roll-rocks platform 0 #'finder #'setter))
               finally (return platform)))))

(defun measure-load (platform)
  "The load on a PLATFORM is the sum of the load of each round rock.
A round rock's load is the distance from its row to the southern most
row (inclusive)"
  (let ((max-rows (platform-num-rows platform))
        (max-cols (platform-num-cols platform)))
    (loop for row-idx below max-rows
          sum (* (- max-rows row-idx)
                 (loop for col-idx below max-cols
                       count (round-rock-p
                              (platform-at platform row-idx col-idx)))))))

(defun part1 (input)
  (measure-load (tilt (copy-platform input) :north)))

(5am:def-test part1 (:suite :aoc-2023-14)
  (5am:is (= 136 (measure-load +tilted-example+)))
  (5am:is (equalp +tilted-example+ (tilt (copy-platform +example+) :north)))
  (5am:is (= 136 (part1 +example+)))
  (5am:is (= 110090 (part1 +input+))))

(defparameter +spun-example+
  (mapcar #'aoc:lists->2d-array
          '((".....#...."
             "....#...O#"
             "...OO##..."
             ".OO#......"
             ".....OOO#."
             ".O#...O#.#"
             "....O#...."
             "......OOOO"
             "#...O###.."
             "#..OO#....")
            (".....#...."
             "....#...O#"
             ".....##..."
             "..O#......"
             ".....OOO#."
             ".O#...O#.#"
             "....O#...O"
             ".......OOO"
             "#..OO###.."
             "#.OOO#...O")
            (".....#...."
             "....#...O#"
             ".....##..."
             "..O#......"
             ".....OOO#."
             ".O#...O#.#"
             "....O#...O"
             ".......OOO"
             "#...O###.O"
             "#.OOO#...O"))))

(let ((memoize-cache (make-hash-table :test #'equalp)))
  (defun clear-tilt-cache ()
    (setf memoize-cache (make-hash-table :test #'equalp)))
  (defun get-cache () memoize-cache)
  (defun memoized-tilt (platform direction)
    (let ((cached-result (gethash (list platform direction) memoize-cache)))
      (if cached-result (progn ; (format *debug-io* "~&CACHE HIT on ~S" direction)
                               cached-result)
          (progn ;; (format *debug-io* "~&CACHE MISS")
                 (setf (gethash (list (copy-platform platform) direction) memoize-cache)
                       (copy-platform (tilt platform direction))))))))

(defun one-full-spin (platform)
  (setf platform (memoized-tilt platform :north)
        platform (memoized-tilt platform :west)
        platform (memoized-tilt platform :south)
        platform (memoized-tilt platform :east)))

(defun spin-cycle (platform num-spins &optional (allow-skips-p t))
  (clear-tilt-cache)
  (let ((cycle-hash (make-hash-table :test #'equalp)))
    (loop for idx = 0 then idx
          ; do (format T "~&IDX: ~D" idx)
          until (>= idx num-spins)
          do (setf platform (one-full-spin platform))
          if (gethash platform cycle-hash)
            do (let ((previously-seen-at (gethash platform cycle-hash)))
                 ;; (format t "~&DEJA-VU: ~D -> ~D" (+ idx 1) previously-seen-at)
                 (if allow-skips-p
                     (let* ((cycle-len (1+ (- (+ 1 idx) previously-seen-at)))
                            (skip cycle-len))
                       ;; (format t "can we skip to: ~D" (* cycle-len (floor (- num-spins idx) cycle-Zen)))
                       (if (> num-spins (+ idx skip))
                         (progn ;; (format t "~&SKIPPING ahead to ~D" (+ idx skip))
                                (incf idx skip))
                         (incf idx))
                     )
                   (incf idx)))
          else
            do (progn (setf (gethash (copy-platform platform) cycle-hash) (+ 1 idx))
                      (incf idx))
          finally (return platform))))

(defun part2 (input &optional (num-spins 1000000))
  (measure-load (spin-cycle (copy-platform input) num-spins)))

(5am:def-test part2 (:suite :aoc-2023-14)
  (5am:is (equalp (nth 0 +spun-example+) (spin-cycle (copy-platform +example+) 1)))
  (5am:is (equalp (nth 1 +spun-example+) (spin-cycle (copy-platform +example+) 2)))
  (5am:is (equalp (nth 2 +spun-example+) (spin-cycle (copy-platform +example+) 3)))
  ;; (5am:is (= 64 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
