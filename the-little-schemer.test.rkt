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

(test-case
  "Ch3. Cons the Magnificent"
  (define rember
    (lambda (a lat)
      (cond [(null? lat) lat]
            [(eq? a (car lat)) (cdr lat)]
            [else (cons (car lat) (rember a (cdr lat)))])))

  (check-equal? (rember 'b '()) '())
  (check-equal? (rember 'b '(a b c)) '(a c))
  (check-equal? (rember 'd '(a b c)) '(a b c))
  (check-equal? (rember 'b '(a b c b)) '(a c b))

  (define firsts
    (lambda (l)
      (cond [(null? l) l]
            [else (cons (car (car l)) (firsts (cdr l)))])))

  (check-equal? (firsts '((1 2) (2 3))) '(1 2))
  (check-equal? (firsts '()) '())
  (check-equal? (firsts '(((1)) (2))) '((1) 2))

  (define insertR
    (lambda (new old lat)
      (cond [(null? lat) lat]
            [(eq? old (car lat)) (cons old (cons new (cdr lat)))]
            [else (cons (car lat) (insertR new old (cdr lat)))])))

  (check-equal? (insertR 'n 'o '()) '())
  (check-equal? (insertR 'n 'o '(a o b)) '(a o n b))
  (check-equal? (insertR 'n 'o '(a b)) '(a b))

  (define insertL
    (lambda (new old lat)
      (cond [(null? lat) lat]
            [(eq? old (car lat)) (cons new lat)]
            [else (cons (car lat) (insertL new old (cdr lat)))])))

  (check-equal? (insertL 'n 'o '()) '())
  (check-equal? (insertL 'n 'o '(a o b)) '(a n o b))
  (check-equal? (insertL 'n 'o '(a b)) '(a b))

  (define subst
    (lambda (new old lat)
      (cond [(null? lat) lat]
            [(eq? old (car lat)) (cons new (cdr lat))]
            [else (cons (car lat) (subst new old (cdr lat)))])))

  (check-equal? (subst 'n 'o '()) '())
  (check-equal? (subst 'n 'o '(a o b)) '(a n b))
  (check-equal? (subst 'n 'o '(a b)) '(a b))

  (define subst2
    (lambda (new o1 o2 lat)
      (cond [(null? lat) lat]
            [(or (eq? o1 (car lat))
                 (eq? o2 (car lat)))
             (cons new (cdr lat))]
            [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))])))

  (check-equal? (subst2 'n 'o1 'o2 '()) '())
  (check-equal? (subst2 'n 'o1 'o2 '(a o1 b)) '(a n b))
  (check-equal? (subst2 'n 'o1 'o2 '(o2 o1 b)) '(n o1 b))
  (check-equal? (subst2 'n 'o1 'o2 '(a b)) '(a b))

  (define multirember
    (lambda (a lat)
      (cond [(null? lat) lat]
            [(eq? a (car lat)) (multirember a (cdr lat))]
            [else (cons (car lat) (multirember a (cdr lat)))])))

  (check-equal? (multirember 'a '()) '())
  (check-equal? (multirember 'a '(a a a)) '())
  (check-equal? (multirember 'a '(b a a a)) '(b))
  (check-equal? (multirember 'a '(b a a a d)) '(b d))

  (define multiinsertR
    (lambda (new old lat)
      (cond [(null? lat) lat]
            [(eq? old (car lat))
             (cons old (cons new (multiinsertR new old (cdr lat))))]
            [else (cons (car lat) (multiinsertR new old (cdr lat)))])))

  (check-equal? (multiinsertR 'a 'o '()) '())
  (check-equal? (multiinsertR 'a 'o '(b c o o)) '(b c o a o a))

  (define multiinsertL
    (lambda (new old lat)
      (cond [(null? lat) lat]
            [(eq? old (car lat))
             (cons new (cons old (multiinsertL new old (cdr lat))))]
            [else (cons (car lat) (multiinsertL new old (cdr lat)))])))

  (check-equal? (multiinsertL 'a 'o '()) '())
  (check-equal? (multiinsertL 'a 'o '(b c o o)) '(b c a o a o))

  (define multisubst
    (lambda (new old lat)
      (cond [(null? lat) lat]
            [(eq? old (car lat))
             (cons new (multisubst new old (cdr lat)))]
            [else (cons (car lat) (multisubst new old (cdr lat)))])))

  (check-equal? (multisubst 'a 'o '()) '())
  (check-equal? (multisubst 'a 'o '(b c o o)) '(b c a a)))

