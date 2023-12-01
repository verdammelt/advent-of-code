(in-package :aoc-tests)
(5am:in-suite aoc-tests)

(5am:test keywordize
  (5am:is (eq :keyword (aoc:keywordize "keyword"))))

(5am:test number-or-keyword
  (5am:is (= 13 (aoc:number-or-keyword "13")))
  (5am:is (eq :foo (aoc:number-or-keyword "foo")))
  (5am:is (eq :13foo (aoc:number-or-keyword "13foo"))))
