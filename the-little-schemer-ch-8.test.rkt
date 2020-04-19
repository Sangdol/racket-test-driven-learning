#lang errortrace racket

(require rackunit)

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(test-case
  "Ch8. Lambda the ultimate"

  (define rember-f
    (lambda (test? a l)
      (cond [(null? l) l]
            [(test? a (car l)) (cdr l)]
            [else (cons (car l) (rember-f test? a (cdr l)))])))

  (check-equal? (rember-f equal? '(a b) '(a (a b) c)) '(a c))

  ; eq?-(c)urrying
  (define eq?-c
    (lambda (a)
      (lambda (x)
        (eq? x a))))

  (set! rember-f
    (lambda (test?)
      (lambda (a l)
        (cond [(null? l) l]
              [(test? a (car l)) (cdr l)]
              [else (cons (car l) ((rember-f test?) a (cdr l)))]))))

  (check-equal? ((rember-f equal?) '(a b) '(a (a b) c)) '(a c))

  (define insertL-f
    (lambda (test?)
      (lambda (new old l)
        (cond [(null? l) l]
              [(test? old (car l))
               (cons new l)]
              ;(cons new (cons old (cdr l)))] ; book version
              [else (cons (car l) ((insertL-f test?) new old l))]))))

  (check-equal? ((insertL-f equal?) 'a 'b '(b)) '(a b))

  (define insertR-f
    (lambda (test?)
      (lambda (new old l)
        (cond [(null? l) l]
              [(test? old (car l)) (cons old (cons new (cdr l)))]
              [else (cons (car l) ((insertR-f test?) new old l))]))))

  (check-equal? ((insertR-f equal?) 'a 'b '(b)) '(b a))

  (define seqL
    (lambda (new old l)
      (cons new (cons old l))))

  (check-equal? (seqL 'a 'b '(c)) '(a b c))

  (define seqR
    (lambda (new old l)
      (cons old (cons new l))))

  (check-equal? (seqR 'a 'b '(c)) '(b a c))

  (define insert-g
    (lambda (seq)
      (lambda (new old l)
        (cond [(null? l) l]
              [(eq? old (car l)) (seq new old (cdr l))]
              [else (cons (car l) ((insert-g seq) new old (cdr l)))]))))

  (check-equal? ((insert-g seqL) 'a 'b '(b)) '(a b))

  (define atom-to-function
    (lambda (x)
      (cond [(eq? x '+) +]
            [(eq? x '*) *]
            [else expt])))

  (check-equal? (atom-to-function '+) +)

  ; col: collector aka continuation
  (define multirember&co
    (lambda (a lat col)
      (cond [(null? lat) (col '() '())]
            [(eq? (car lat) a)
             (multirember&co
               a
               (cdr lat)
               (lambda (newlat seen)  ; new-friend
                 (col newlat (cons (car lat) seen))))]
            [else
              (multirember&co
                a
                (cdr lat)
                (lambda (newlat seen)  ; latest-friend
                  (col (cons (car lat) newlat) seen)))])))

  (define a-friend
    (lambda (x y)
      (null? y)))

  #|

  (define new-friend
    (lambda (newlat seen)
      (col newlat (cons (car lat) seen))))

  (define latest-friend
    (lambda (newlat seen)
      (col (cons (car lat) newlat) seen)))

  |#

  (check-true (multirember&co 'a '() a-friend))
  (check-true (multirember&co 'a '(b) a-friend))
  (check-false (multirember&co 'a '(a) a-friend))
  (check-false (multirember&co 'a '(a b) a-friend))

  ; why is this called multirember&co? because it can split elements.
  (check-equal? (multirember&co 'a '(a b) (lambda (x y) y)) '(a))
  (check-equal? (multirember&co 'a '(a a b) (lambda (x y) y)) '(a a))
  (check-equal? (multirember&co 'a '(a a b) (lambda (x y) x)) '(b))
  (check-equal? (multirember&co 'a '(a a b c) (lambda (x y) x)) '(b c))

  (define multiinsertLR
    (lambda (new oldL oldR lat)
      (cond [(null? lat) '()]
            [(eq? (car lat) oldL)
             (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))]
            [(eq? (car lat) oldR)
             (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))]
            [else (cons (car lat)
                        (multiinsertLR new oldL oldR (cdr lat)))])))

  (check-equal? (multiinsertLR 'a 'ol 'or '(b ol c or)) '(b a ol c or a))

  (define multiinsertLR&co
    (lambda (new oldL oldR lat col)
      (cond [(null? lat) (col '() 0 0)]
            [(eq? (car lat) oldL)
             (multiinsertLR&co
               new oldL oldR (cdr lat)
               (lambda (newlat L R)
                 (col (cons new (cons oldL newlat))
                      (add1 L) R)))]
            [(eq? (car lat) oldR)
             (multiinsertLR&co
               new oldL oldR (cdr lat)
               (lambda (newlat L R)
                 (col (cons oldR (cons new newlat))
                      L (add1 R))))]
            [else
              (multiinsertLR&co
                new oldL oldR (cdr lat)
                (lambda (newlat L R)
                  (col (cons (car lat) newlat) L R)))])))

  (check-equal? (multiinsertLR&co 'n 'ol 'or '(a ol b or) (lambda (newlat L R) newlat))
                '(a n ol b or n))

  (check-equal? (multiinsertLR&co 'n 'ol 'or '(a ol b or) (lambda (newlat L R) L))
                1)

  (check-equal? (multiinsertLR&co 'n 'ol 'or '(a ol b or or) (lambda (newlat L R) R))
                2)

  (define evens-only*
    (lambda (l)
      (cond [(null? l) '()]
            [(atom? (car l))
             (cond [(even? (car l)) (cons (car l) (evens-only* (cdr l)))]
                   [else (evens-only* (cdr l))])]
            [else (cons (evens-only* (car l)) (evens-only* (cdr l)))])))

  (check-equal? (evens-only* '(1)) '())
  (check-equal? (evens-only* '(1 2)) '(2))
  (check-equal? (evens-only* '(1 4 (1 2))) '(4 (2)))

  ; even numbers, mul of evens, sum of evens
  (define evens-only*&co
    (lambda (l col)
      (cond [(null? l) (col '() 1 0)]
            [(atom? (car l))
             (cond [(even? (car l))
                    (evens-only*&co
                      (cdr l)
                      (lambda (newl p s)
                        (col (cons (car l) newl)
                             (* p (car l)) (+ s (car l)))))]
                   [else
                     (evens-only*&co
                       (cdr l)
                       (lambda (newl p s)
                         (col newl p s)))])]
            [else
              (evens-only*&co
                (car l)
                (lambda (al ap as)
                  (evens-only*&co
                    (cdr l)
                    (lambda (dl dp ds)
                      (col (cons al dl) (* ap dp) (+ as ds))))))])))

  (define the-last-friend
    (lambda (newl product sum)
      (cons sum (cons product newl))))

  (check-equal? (evens-only*&co '((1 2 3) 4) the-last-friend) '(6 8 (2) 4)))


