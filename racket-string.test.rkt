#lang racket/base

(require rackunit)

(test-case
  "basic functions"
  (check-true (string=? "a" "a")))

