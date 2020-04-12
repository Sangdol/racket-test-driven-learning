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

(test-case
  "4. Numbers Games"
  (define +
    (lambda (n m)
      (cond [(zero? m) n]
            ; This is not "natural recursion"
            ; https://stackoverflow.com/questions/32260444/what-is-the-definition-of-natural-recursion
            ; But this can be tail call optimized
            ; https://stackoverflow.com/questions/310974/what-is-tail-call-optimization
            ;[else (+ (add1 n) (sub1 m))]))) ; mine
            [else (add1 (+ n (sub1 m)))])))

  (check-equal? (+ 1 2) 3)

  (define -
    (lambda (n m)
      (cond [(zero? m) n]
            [else (sub1 (- n (sub1 m)))])))

  (check-equal? (- 3 2) 1)

  (define addtup
    (lambda (tup)
      (cond [(null? tup) 0]
            [else (+ (car tup) (addtup (cdr tup)))])))

  (check-equal? (addtup '()) 0)
  (check-equal? (addtup '(1 2 3)) 6)

  (define x
    (lambda (n m)
      (cond [(zero? m) 0]
            [else (+ n (x n (sub1 m)))])))

  (check-equal? (x 3 2) 6)

  (define tup+
    (lambda (t1 t2)
      (cond [(and (null? t1) (null? t2)) '()]
            [else (cons (+ (car t1) (car t2))
                        (tup+ (cdr t1) (cdr t2)))])))

  (check-equal? (tup+ '(1 2) '(3 4)) '(4 6))

  (set! tup+
    (lambda (t1 t2)
      (cond [(null? t1) t2]
            [(null? t2) t1]
            [else (cons (+ (car t1) (car t2))
                        (tup+ (cdr t1) (cdr t2)))])))

  (check-equal? (tup+ '(1 2 9 9) '(3 4)) '(4 6 9 9))
  (check-equal? (tup+ '(1 2 8 8) '(3 4)) '(4 6 8 8))

  (define >
    (lambda (n m)
      (cond [(zero? n) #f]
            [(zero? m) #t]
            [else (> (sub1 n) (sub1 m))])))

  (check-true (> 10 5))
  (check-false (> 5 5))
  (check-false (> 4 5))

  (define <
    (lambda (n m)
      (cond [(zero? m) #f]
            [(zero? n) #t]
            [else (< (sub1 n) (sub1 m))])))

  (check-false (< 10 5))
  (check-false (< 5 5))
  (check-true (< 4 5))

  (define (= n m)
    (and (not (> n m)) (not (< n m))))

  (check-true (= 1 1))
  (check-false (= 2 1))

  (define pow
    (lambda (n m)
      (cond [(zero? m) 1]
            [else (* n (pow n (sub1 m)))])))

  (check-equal? (pow 1 1) 1)
  (check-equal? (pow 2 3) 8)

  (define length
    (lambda (lat)
      (cond [(null? lat) 0]
            [else (add1 (length (cdr lat)))])))

  (check-equal? (length '()) 0)
  (check-equal? (length '(1)) 1)
  (check-equal? (length '(1 2)) 2)

  (define pick
    (lambda (n lat)
      (cond [(zero? (sub1 n)) (car lat)]
            [else (pick (sub1 n) (cdr lat))])))

  (check-equal? (pick 1 '(a b)) 'a)
  (check-equal? (pick 2 '(a b c)) 'b)

  (define rempick
    (lambda (n lat)
      (cond [(null? lat) lat]  ; this is not considered in the book
            [(zero? (sub1 n)) (cdr lat)]
            [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

  (check-equal? (rempick 1 '()) '())
  (check-equal? (rempick 1 '(a b)) '(b))
  (check-equal? (rempick 2 '(a b c)) '(a c))

  (define no-nums
    (lambda (lat)
      (cond [(null? lat) lat]
            [(number? (car lat)) (no-nums (cdr lat))]
            [else (cons (car lat) (no-nums (cdr lat)))])))

  (check-equal? (no-nums '()) '())
  (check-equal? (no-nums '(a 1 b)) '(a b))
  (check-equal? (no-nums '(1)) '())
  (check-equal? (no-nums '(a 1 b 2)) '(a b))


  #|

  ; my first version
  (define all-nums
    (lambda (lat)
      (cond [(null? lat) lat]
            [(number? (car lat)) (all-nums (cdr lat))]
            [else (cons (car lat) (all-nums (cdr lat)))])))
  |#

  (define all-nums
    (lambda (lat)
      (cond [(null? lat) lat]
            [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
            [else (all-nums (cdr lat))])))

  (check-equal? (all-nums '()) '())
  (check-equal? (all-nums '(a 1 b)) '(1))
  (check-equal? (all-nums '(1)) '(1))
  (check-equal? (all-nums '(a 1 b 2)) '(1 2))

  ; eqan: (eq)ual (a)tom (n)umber
  (define eqan?
    (lambda (a b)
      (cond [(and (number? a) (number? b)) (= a b)]
            [(and (atom? a) (atom? b)) (eq? a b)]
            [else #f])))

  (check-true (eqan? 1 1))
  (check-true (eqan? 'a 'a))
  (check-false (eqan? 'a 'b))
  (check-false (eqan? 'a 2))

  (define occur
    (lambda (a lat)
      (cond [(null? lat) 0]
            [(eq? a (car lat)) (add1 (occur a (cdr lat)))]
            [else (occur a (cdr lat))])))


  (check-equal? (occur 'a '()) 0)
  (check-equal? (occur 'a '(b)) 0)
  (check-equal? (occur 'a '(a b a)) 2)

  (define (one? n)
    (= n 1))

  (check-true (one? 1))
  (check-false (one? 0))
  (check-false (one? 100))

  (set! rempick
    (lambda (n lat)
      (cond [(null? lat) lat]  ; this is not considered in the book
            [(one? n) (cdr lat)]
            [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

  (check-equal? (rempick 1 '()) '())
  (check-equal? (rempick 1 '(a b)) '(b))
  (check-equal? (rempick 2 '(a b c)) '(a c)))

