(in-package #:aoc-2015)

(assert (= 280 (aoc:perform-day-task 2015 1 #'what-floor)))
(assert (= 1797 (aoc:perform-day-task 2015 1 #'when-floor)))

(assert (= 1588178 (aoc:perform-day-task 2015 2 #'total-square-feet #'parse-to-dimensions)))
(assert (= 3783758 (aoc:perform-day-task 2015 2 #'total-ribbon-size #'parse-to-dimensions)))

(assert (= 2572 (aoc:perform-day-task 2015 3 #'count-houses-visited #'delivery-parse)))
(assert (= 2631 (aoc:perform-day-task 2015 3 #'share-route-with-robosanta #'delivery-parse)))

(assert (= 117946 (aoc:perform-day-task 2015 4 #'first-advent-coin-idx #'parse-secret-key)))
(assert (= 3938038 (aoc:perform-day-task 2015 4 #'first-index-with-six-zeros #'parse-secret-key)))

(assert (= 255 (aoc:perform-day-task 2015 5 #'count-if-nice #'parse-list)))
(assert (= 55 (aoc:perform-day-task 2015 5 #'count-if-nice-2 #'parse-list)))
