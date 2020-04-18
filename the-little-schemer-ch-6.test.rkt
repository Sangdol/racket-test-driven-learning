#lang errortrace racket

(require rackunit)

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(test-case
  "Ch6. Shadows"

  ; aexp = (a)tom (exp)ression?
  ; well, the spec of this is poor
  (define numbered?
    (lambda (aexp)
      (cond [(atom? aexp) (number? aexp)]
            [(eq? (car (cdr aexp)) '+)
             (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
            [(eq? (car (cdr aexp)) '*)
             (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
            [(eq? (car (cdr aexp)) '^)
             (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))])))

  (check-false (numbered? 'a))
  (check-false (numbered? '(a + 1)))
  (check-true (numbered? 1))
  (check-true (numbered? '(1 + (1 + 1))))
  (check-true (numbered? '(1 + (1 * 1))))

  ; well, this doesn't do the same as the previous one
  (define numbered?.v2
    (lambda (aexp)
      (cond [(atom? aexp) (number? aexp)]
            [else (and (numbered?.v2 (car aexp))
                       (numbered?.v2 (car (cdr (cdr aexp)))))])))

  (check-false (numbered?.v2 'a))
  (check-false (numbered?.v2 '(a + 1)))
  (check-true (numbered?.v2 1))
  (check-true (numbered?.v2 '(1 + (1 + 1))))
  (check-true (numbered?.v2 '(1 + (1 * 1))))

  (define value
    (lambda (nexp)
      (cond [(atom? nexp) nexp]
            [(eq? (car (cdr nexp)) '+)
             (+ (value (car nexp)) (value (car (cdr (cdr nexp)))))]
            [(eq? (car (cdr nexp)) '*)
             (* (value (car nexp)) (value (car (cdr (cdr nexp)))))]
            [(eq? (car (cdr nexp)) '^)
             (expt (value (car nexp)) (value (car (cdr (cdr nexp)))))])))

  (check-equal? (value '(1 + 1)) 2)
  (check-equal? (value '((2 * 3) + 1)) 7)
  (check-equal? (value '((2 + 3) * 3)) 15)
  (check-equal? (value '((2 ^ 3) * 3)) 24)

  ; this doesn't work (from the third s-exp
  (check-equal? (value '((2 + 3) * 3 * 2)) 15)

  (define value.v2
    (lambda (nexp)
      (cond [(atom? nexp) nexp]
            [(eq? (car nexp) '+)
             (+ (value.v2 (car (cdr nexp)))
                (value.v2 (car (cdr (cdr nexp)))))]
            [(eq? (car nexp) '*)
             (* (value.v2 (car (cdr nexp)))
                (value.v2 (car (cdr (cdr nexp)))))]
            [(eq? (car nexp) '^)
             (expt (value.v2 (car (cdr nexp)))
                   (value.v2 (car (cdr (cdr nexp)))))])))

  (check-equal? (value.v2 '(+ 1 1)) 2)
  (check-equal? (value.v2 '(+ (* 2 3) 1)) 7)
  (check-equal? (value.v2 '(* (+ 2 3) 3)) 15)

  (define 1st-sub-exp
    (lambda (aexp)
      (car (cdr aexp))))

  (define 2nd-sub-exp
    (lambda (aexp)
      (car (cdr (cdr aexp)))))

  (define operator
    (lambda (aexp)
      (car aexp)))

  ; We can change this to the first version if we change the helper functions.
  (define value.v3
    (lambda (nexp)
      (cond [(atom? nexp) nexp]
            [(eq? (operator nexp) '+)
             (+ (value.v3 (1st-sub-exp nexp))
                (value.v3 (2nd-sub-exp nexp)))]
            [(eq? (operator nexp) '*)
             (* (value.v3 (1st-sub-exp nexp))
                (value.v3 (2nd-sub-exp nexp)))]
            [else (expt (value.v3 (1st-sub-exp nexp))
                        (value.v3 (2nd-sub-exp nexp)))])))

  (check-equal? (value.v3 '(+ 1 1)) 2)
  (check-equal? (value.v3 '(+ (* 2 3) 1)) 7)
  (check-equal? (value.v3 '(* (+ 2 3) 3)) 15)

  ; (s)-exp z(ero)
  (define sero?
    (lambda (n)
      (null? n)))

  (check-false (sero? '(())))
  (check-true (sero? '()))

  ; (e)mpty a(dd)
  (define edd1
    (lambda (n)
      (cons '() n)))

  (check-equal? (edd1 '()) '(()))

  ; (z)?? s(ub)
  (define zub1
    (lambda (n)
      (cdr n)))

  (check-equal? (zub1 '(())) '())

  (define my+
    (lambda (n m)
      (cond [(sero? m) n]
            [else (edd1 (my+ n (zub1 m)))])))

  (check-equal? (my+ '(()) '(() ())) '(() () ())))

