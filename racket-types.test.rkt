#lang racket

(require rackunit)

(test-case
  "symbols"
  (check-true (symbol=? 'foo 'foo))
  (check-false (symbol=? 'foo 'Foo)))

(test-case
  "numbers"
  (check = (expt 53 10) 174887470365513049)
  (check = (sqrt -1) 0+1i))

; Numbers https://docs.racket-lang.org/guide/numbers.html
(test-case
  "number categories"
  (check-true (integer? 5))
  (check-true (complex? 0+1i))
  (check-true (integer? 5.0)))

(test-case
  "cons cells"
  (check-equal? (cons 1 2) '(1 . 2))
  (define cell (cons 'a 'b))
  ;  CAR (car) /kɑːr/ and CDR (cdr) /ˈkʌdər/
  (check-equal? (car cell) 'a)
  (check-equal? (cdr cell) 'b)
  (check-equal? (cons 'chicken empty) '(chicken))
  (check-equal? (cons 'chicken '()) '(chicken))
  ; consing
  (check-equal? (cons 'a '(b c)) '(a b c)))



