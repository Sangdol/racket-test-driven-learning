#lang racket

(require rackunit)

(test-case
  "boolean"
  (check-equal? #t #true)
  (check-equal? #true true))

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

(test-case
  "list"
  (check-equal? (list 'a 'b 'c) (cons 'a (cons 'b (cons 'c empty))))
  (check-equal? (list 'a 'b 'c) '(a b c))
  (check-equal? (first (rest '(a b c))) 'b)
  (check-equal? (second (range 0 10)) 1)
  (check-equal? (tenth (range 0 10)) 9))

(test-case
  "structure (usually for fixed number of items)"
  (struct person (name age#))
  (define sang (person 'Sang 36))
  (check-equal? (person-name sang) 'Sang)  ; person-name: (field) selector
  (check-equal? (person-age# sang) 36)
  (check-true (person? sang))  ; person, huh?: type predicate
  (check-false (struct? sang))  ; because it's opaque by default for encapulation

  (struct naked (name age#) #:transparent)
  (define n (naked 'n 0))
  (check-true (naked? n))
  (check-true (struct? n)))
