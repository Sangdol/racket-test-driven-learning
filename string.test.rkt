#lang racket/base

(require rackunit)

(test-case
  "basic functions"
  (check-true (string=? "a" "a")))

(test-case
  "type casting"
  (check-equal? (number->string 1) "1")
  (check-equal? (string->number "1") 1))

