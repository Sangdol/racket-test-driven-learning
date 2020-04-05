#lang racket/base

; single line comment

#| Block comment
  #| can be nested
  |#
|#

(require rackunit)

(test-case
  "define variable"
  (define a 1)
  (check-equal? a 1))

(test-case
  "define function"
  (define (return-five)
    5)
  (check-equal? (return-five) 5)

  ; parametered
  (define (return n)
    n)
  (check-equal? (return 3) 3))

(test-case
  "set bang"
  (define a 1)
  (set! a 2)
  (check-equal? a 2))

(test-case
  "sub1"
  (define a 2)
  (check-equal? (sub1 a) 1)
  (check-equal? a 2)) ; sub1 didn't change a.

(test-case
  "add1"
  (define a 1)
  (check-equal? (add1 a) 2)
  (check-equal? a 1))  ; add1 didn't change a.
