#lang racket/base

; single line comment

#| Block comment
  #| can be nested
  |#
|#

(require rackunit)

(check-eq? (+ 1 1) 2 "check-eq")
