#lang errortrace racket

(require rackunit)

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define member?
  (lambda (a lat)
    (cond [(null? lat) #f]
          [else (or (eq? a (car lat))
                    (member? a (cdr lat)))])))

(define multirember
  (lambda (a lat)
    (cond [(null? lat) lat]
          [(eq? a (car lat)) (multirember a (cdr lat))]
          [else (cons (car lat) (multirember a (cdr lat)))])))

(test-case
  "Ch7. Friends and Relations"

  (define set?
    (lambda (lat)
      (cond [(null? lat) #t]
            [(member? (car lat) (cdr lat)) #f]
            [else (set? (cdr lat))])))

  (check-false (set? '(a a)))
  (check-false (set? '(a a b)))
  (check-true (set? '()))
  (check-true (set? '(a b)))
  (check-true (set? '(a b c)))

  (define makeset
    (lambda (lat)
      (cond [(null? lat) lat]
            [(member? (car lat) (cdr lat)) (makeset (cdr lat))]
            [else (cons (car lat) (makeset (cdr lat)))])))

  (check-equal? (makeset '()) '())
  (check-equal? (makeset '(a)) '(a))
  (check-equal? (makeset '(a a)) '(a))
  (check-equal? (makeset '(a a a)) '(a))
  (check-equal? (makeset '(a a a b)) '(a b))
  (check-equal? (makeset '(a a a b a)) '(b a))
  (check-equal? (makeset '(a c a a b a)) '(c b a))

  ; with multirember
  (set! makeset
   (lambda (lat)
     (cond [(null? lat) lat]
           [else (cons (car lat)
                  (makeset (multirember (car lat) (cdr lat))))])))

  (check-equal? (makeset '()) '())
  (check-equal? (makeset '(a)) '(a))
  (check-equal? (makeset '(a a)) '(a))
  (check-equal? (makeset '(a a a)) '(a))
  (check-equal? (makeset '(a a a b)) '(a b))
  (check-equal? (makeset '(a a a b a)) '(a b))
  (check-equal? (makeset '(a c a a b a)) '(a c b))

  (define subset?
    (lambda (set1 set2)
      (cond [(null? set1) #t]
            [else (and (member? (car set1) set2)
                       (subset? (cdr set1) set2))])))

  (check-false (subset? '(a) '(b)))
  (check-true (subset? '(a) '(a)))
  (check-true (subset? '(a b c) '(a b c d)))
  (check-true (subset? '(a b c) '(z a b c d)))
  (check-true (subset? '(a b c) '(z a c d b)))

  (define eqset?
    (lambda (set1 set2)
      (and (subset? set1 set2)
           (subset? set2 set1))))

  (check-false (eqset? '() '(b)))
  (check-false (eqset? '(a) '(b)))
  (check-false (eqset? '(a) '(a b)))
  (check-true (eqset? '() '()))
  (check-true (eqset? '(a) '(a)))
  (check-true (eqset? '(a b) '(b a)))
  (check-true (eqset? '(a b) '(a b)))

  (define intersect?
    (lambda (set1 set2)
      (cond [(null? set1) #f]
            [(member? (car set1) set2) #t]
            [else (intersect? (cdr set1) set2)])))

  (check-false (intersect? '(a) '(c b)))
  (check-true (intersect? '(a) '(a b)))

  ; 'or' version - it doesn't look any better
  (set! intersect?
    (lambda (set1 set2)
      (cond [(null? set1) #f]
            [else (or (member? (car set1) set2)
                      (intersect? (cdr set1) set2))])))

  (check-false (intersect? '(a) '(c b)))
  (check-true (intersect? '(a) '(a b))))
