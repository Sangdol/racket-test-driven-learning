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

(test-case
  "if and cond - only #f is #f"
  (check-true (if (= (+ 1 2) 3)
                  #t
                  #f))
  (check-true (if 1
                #t
                #f))
  (check-true (if 0
                #t
                #f))
  (check-true (if `()
                #t
                #f))
  (check-false (if #f
                #t
                #f))

  (check-true (cond [(= 1 1) #t]
                    [(odd? 1) #f]
                    [else 'else]))
  )

(test-case
  "and or"
  ; short-circuit
  (define is-it-even #f)
  (or (odd? 2) (set! is-it-even #t))
  (check-true is-it-even)

  (set! is-it-even #f)
  (and (even? 2) (set! is-it-even #t))
  (check-true is-it-even))

(test-case
  "when / unless"
  (define is-it-even #f)
  (when (even? 2)  ; 'when' is 'if' without else
    (set! is-it-even #t))
  (check-true is-it-even)

  (define is-it-odd #f)
  (unless (even? 1)
    (set! is-it-odd #t))
  (check-true is-it-odd))
