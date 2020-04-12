#lang racket

; single line comment

#| Block comment
  #| can be nested
  |#
|#

(require rackunit)

; module level definition
(test-case
  "define variable"
  ; local definition
  (define a 1)
  (check-equal? a 1)

  ; Form
  ; ( define ‹id› ‹expr› )
  ; ( define ( ‹id› ‹id›* ) ‹expr›+ )
  ;
  ; Note that multiple expressions are allowed in the second case.
  ; https://docs.racket-lang.org/guide/syntax-overview.html
  (define (bake flavor)
    string-append flavor "jello")  ; they are separate expressions without ()
  (check-equal? (bake "ignore") "jello")

  ; define multiple values conditionally
  (define-values (x y)
    (if (string=? "a" "a")
      (values 1 2)
      (values 3 4)))
  (check-equal? (list x y) (list 1 2)))

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

  ; using functions that return more than just the truth
  (check-equal? (member 4 (list 1 4 3)) (list 4 3))
  (check-true (if (member 4 (list 1 4 3)) #t #f)))

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

(test-case
  "eq and equal (identity and equality)"
  (struct person (name))
  (define p1 (person 'Sang))
  (define p2 (person 'Sang))
  (check-false (equal? p1 p2))  ; opaque
  (check-false (eq? p1 p2))

  (struct naked (name) #:transparent)
  (define n1 (naked 'n))
  (define n2 (naked 'n))
  (check-true (equal? n1 n2))
  (check-false (eq? n1 n2)))

; https://stackoverflow.com/questions/41417892/what-is-the-difference-between-and-brackets-in-racket-lisp-programming
(test-case
  "no functional differences between (), [], and {}"
  (check-pred = (+ 1 1) [+ 1 1]))

(test-case
  "for loop"
  (check-equal? (for/list ([i '(1 2 3)]) (/ 1 i))
                (map (lambda (x) (/ 1 x)) '(1 2 3)))

  ; (for/fold ([accum-id init-expr] ...)
  ;           (clause ...)
  ;   body ...+)
  (check-equal? (for/fold ([sqrs 0])
                          ([i '(1 2)])
                  (+ (sqr i) sqrs))
                (foldl (lambda (i sqrs) (+ (sqr i) sqrs))
                       0
                       '(1 2)))
  (check-equal? (for/list ([i '(1 2 3)]
                           #:when (odd? i))
                  i)
                '(1 3))

  ; nested loops
  (check-equal? (for*/list ([i '(1 2)]
                            [j '(3 4)])
                  (+ i j))
                '(4 5 5 6))
  (check-equal? (for/list ([i '(1 2)])
                  (for/list ([j '(3 4)])
                    (+ i j)))
                '((4 5) (5 6)))

  (check-equal? (for*/list ([k '((1 2) (3 4))]
                            [n k])
                  n)
                '(1 2 3 4))

  (check-equal? (for/list ([k (in-naturals)]
                           [n (in-range 3)])
                  (list k n))
                '((0 0) (1 1) (2 2))))

; https://docs.racket-lang.org/reference/quasiquote.html
(test-case
  "Quasiquoting - backtick `"
  (check-equal? (quasiquote (0 1)) `(0 1))
  (check-equal? `(0 1) '(0 1))
  (check-equal? `(0 ,(+ 1 1) 4) '(0 2 4))  ; ',': unquote
  (check-equal? `(0 ,@(list 1 2) 3) '(0 1 2 3))  ; ',@': unquote-slicing
  )
