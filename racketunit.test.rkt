#lang racket/base

#|

https://docs.racket-lang.org/rackunit/api.html#%28part._.Checks%29
https://beautifulracket.com/explainer/unit-testing.html

check < test case < test suite

eq vs. eqv vs. equal
https://stackoverflow.com/questions/16299246/what-is-the-difference-between-eq-eqv-equal-and-in-scheme

|#

(require rackunit)

(check-eq? 1 (* 1 1))
(check-eq? 1 (* 1 1) "1 * 1 = 1")

(check-not-eq? 1 (* 1 2))
(check-not-eq? (list 1) (list 1))

(check-eq? 1.0 1.0)

(check-eqv? 1 1)
(check-not-eqv? 1 1.0)

(check-equal? (list 1) (list 1))
(check-not-equal? (list 1) (list 2))

(check-pred string? "Hello")
(check-pred number? 100)

; number, number, epsilon = exclusive range
(check-= 1.0 1.01 0.011)
;(check-= 1.0 1.01 0.01) ; fails

; what does c mean?
; any/c, any/c, epsilon
(check-within (list 6 10) (list 6.02 9.99) 0.05)

(check-true (> 1 0))
(check-false (< 1 0))
(check-not-false 1)
(check-not-false #t)

; What is current-continuation-marks?
; https://docs.racket-lang.org/reference/contmarks.html#%28def._%28%28quote._~23~25kernel%29._current-continuation-marks%29%29
;
; expcetion?
; https://docs.racket-lang.org/reference/exns.html#%28def._%28%28quote._~23~25kernel%29._exn~3afail~3f%29%29
(check-exn
  exn:fail?
  (lambda ()
    (raise (make-exn:fail "Hi"
                          (current-continuation-marks)))))

(check-not-exn (lambda () 1))

(check-regexp-match "a+bba" "aaaabba")

;
; what is _? syntax pattern
; https://docs.racket-lang.org/reference/stx-patterns.html#%28form._%28%28lib._racket%2Fprivate%2Fstxcase-scheme..rkt%29.__%29%29
;
; (check-match v pattern pred)
(check-match (list 1 2 3) (list _ _ 3))
(check-match (list 1 2 3) (list _ 2 _))

(check-match (list 1 (list 4)) (list x (list _)) (odd? x))

(check > 1 0)

;(fail [message])
