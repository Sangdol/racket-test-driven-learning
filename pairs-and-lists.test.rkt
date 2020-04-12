#|

https://docs.racket-lang.org/reference/pairs.html

|#

#lang racket/base

(require rackunit)

(test-case
  "build-list"
  (check-equal? (build-list 3 (lambda (x) (* x x)))
                '(0 1 4)))

