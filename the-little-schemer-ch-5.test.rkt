#lang racket

(require rackunit)

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(test-case
  "Ch5. Oh My Gawd*: It's Full of Stars"

  ; mine (the book doesn't use `list?`
  (define rember*
    (lambda (a l)
      (cond [(null? l) l]
            [(list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l)))]
            [(eq? a (car l)) (rember* a (cdr l))]
            [else (cons (car l) (rember* a (cdr l)))])))

  (check-equal? (rember* 'a '(b)) '(b))
  (check-equal? (rember* 'a '(a b)) '(b))
  (check-equal? (rember* 'a '((b))) '((b)))
  (check-equal? (rember* 'a '((a b))) '((b)))
  (check-equal? (rember* 'a '((a b) b a)) '((b) b))
  (check-equal? (rember* 'a '((a b) a (a) ((a c)))) '((b) () ((c))))

  ; answer from the book
  (define rember*.v2
    (lambda (a l)
      (cond [(null? l) l]
            [(atom? (car l))
             (cond [(eq? a (car l)) (rember* a (cdr l))]
                   [else (cons (car l) (rember* a (cdr l)))])]
            [else (cons (rember* a (car l)) (rember* a (cdr l)))])))

  (check-equal? (rember*.v2 'a '(b)) '(b))
  (check-equal? (rember*.v2 'a '(a b)) '(b))
  (check-equal? (rember*.v2 'a '((b))) '((b)))
  (check-equal? (rember*.v2 'a '((a b))) '((b)))
  (check-equal? (rember*.v2 'a '((a b) b a)) '((b) b))
  (check-equal? (rember*.v2 'a '((a b) a (a) ((a c)))) '((b) () ((c))))

  (define insertR*
    (lambda (new old l)
      (cond [(null? l) l]
            [(atom? (car l))
             (cond [(eq? old (car l)) (cons old (cons new (insertR* new old (cdr l))))]
                   [else (cons (car l) (insertR* new old (cdr l)))])]
            [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))])))

  (check-equal? (insertR* 'a 'b '()) '())
  (check-equal? (insertR* 'a 'b '(b)) '(b a))
  (check-equal? (insertR* 'a 'b '((b))) '((b a)))
  (check-equal? (insertR* 'a 'b '((b c) b d d)) '((b a c) b a d d))

  (define occur*
    (lambda (a l)
      (cond [(null? l) 0]
            [(atom? (car l))
             (cond [(eq? a (car l)) (add1 (occur* a (cdr l)))]
                   [else (occur* a (cdr l))])]
            [else (+ (occur* a (car l)) (occur* a (cdr l)))])))

  (check-equal? (occur* 'a '()) 0)
  (check-equal? (occur* 'a '(a)) 1)
  (check-equal? (occur* 'a '((a))) 1)
  (check-equal? (occur* 'a '((a b) a b)) 2)

  (define subst*
    (lambda (new old l)
      (cond [(null? l) l]
            [(atom? (car l))
             (cond [(eq? old (car l)) (cons new (subst* new old (cdr l)))]
                   [else (cons (car l) (subst* new old (cdr l)))])]
            [else (cons (subst* new old (car l)) (subst* new old (cdr l)))])))

  (check-equal? (subst* 'a 'b '()) '())
  (check-equal? (subst* 'a 'b '(b)) '(a))
  (check-equal? (subst* 'a 'b '((b))) '((a)))
  (check-equal? (subst* 'a 'b '(c (b c) b)) '(c (a c) a))

  (define insertL*
    (lambda (new old l)
      (cond [(null? l) l]
            [(atom? (car l))
             (cond [(eq? old (car l)) (cons new (cons old (insertL* new old (cdr l))))]
                   [else (cons (car l) (insertL* new old (cdr l)))])]
            [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))])))

  (check-equal? (insertL* 'a 'b '()) '())
  (check-equal? (insertL* 'a 'b '(b)) '(a b))
  (check-equal? (insertL* 'a 'b '((b))) '((a b)))
  (check-equal? (insertL* 'a 'b '(c (b) a b)) '(c (a b) a a b)))

