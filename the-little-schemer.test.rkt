#lang racket

(require rackunit)

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(test-case
  "Ch 2. Do it, do it again, and again, and again..."
  #|
  This should also work.

  (define (lat? l)
    (cond [(null? l) #t]
          [(atom? (car l)) (lat? (cdr l))]
          [else #f])))
  |#
  (define lat?
    (lambda (l)
      (cond [(null? l) #t]
            [(atom? (car l)) (lat? (cdr l))]
            [else #f])))

  (check-true (lat? '(1 2 3)))
  (check-false (lat? '(() 2 3)))
  (check-false (lat? '((1) 2 3)))

  (define member?
    (lambda (a lat)
      (cond [(null? lat) #f]
            [else (or (eq? a (car lat))
                      (member? a (cdr lat)))])))

  (check-true (member? 1 '(1 2)))
  (check-false (member? 'a '(1 2))))
