#lang errortrace racket

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
  (check-equal? (insertL* 'a 'b '(c (b) a b)) '(c (a b) a a b))

  (define member*
    (lambda (a l)
      (cond [(null? l) #f]
            [(atom? (car l)) (or (eq? a (car l)) (member* a (cdr l)))]
            [else (or (member* a (car l)) (member* a (cdr l)))])))

  (check-false (member* 'a '()))
  (check-false (member* 'a '(b)))
  (check-false (member* 'a '((b))))
  (check-true (member* 'a '(a)))
  (check-true (member* 'a '((a))))
  (check-true (member* 'a '((b a))))
  (check-true (member* 'a '(b (b a))))

  (define leftmost
    (lambda (l)
      (cond [(atom? (car l)) (car l)]
            [else (leftmost (car l))])))

  (check-equal? (leftmost '(a)) 'a)
  (check-equal? (leftmost '((a))) 'a)
  (check-equal? (leftmost '((a b))) 'a)

  (define eqan?
    (lambda (a b)
      (cond [(and (number? a) (number? b)) (= a b)]
            [(and (atom? a) (atom? b)) (eq? a b)]
            [else #f])))

  (define eqlist?
    (lambda (l1 l2)
      (cond [(and (null? l1) (null? l2)) #t]
            [(or (null? l1) (null? l2)) #f]
            [(and (atom? (car l1)) (atom? (car l2)))
             (and (eqan? (car l1) (car l2))
                  (eqlist? (cdr l1) (cdr l2)))]
            [(or (atom? (car l1)) (atom? (car l2))) #f]
            [else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))

  (check-false (eqlist? '(a) '(b)))
  (check-false (eqlist? '(a a) '(b a)))
  (check-false (eqlist? '(a a) '(b a)))
  (check-false (eqlist? '(a a) '(a (a))))
  (check-false (eqlist? '(a (a b)) '(a (a c))))
  (check-true (eqlist? '() '()))
  (check-true (eqlist? '(a) '(a)))
  (check-true (eqlist? '(a (a b)) '(a (a b))))

  (define my-equal?
    (lambda (s1 s2)
      (cond [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
            [(or (atom? s1) (atom? s2)) #f]
            [else (eqlist? s1 s2)])))

  (check-false (my-equal? 'a 'b))
  (check-false (my-equal? '(a) '(b)))
  (check-false (my-equal? '(a a) '(b a)))
  (check-false (my-equal? '(a a) '(b a)))
  (check-false (my-equal? '(a a) '(a (a))))
  (check-false (my-equal? '(a (a b)) '(a (a c))))
  (check-true (my-equal? 'a 'a))
  (check-true (my-equal? '() '()))
  (check-true (my-equal? '(a) '(a)))
  (check-true (my-equal? '(a (a b)) '(a (a b))))

  (define eqlist?.v2
    (lambda (l1 l2)
      (cond [(and (null? l1) (null? l2)) #t]
            [(or (null? l1) (null? l2)) #f]
            [else (and (my-equal? (car l1) (car l2))
                       (eqlist?.v2 (cdr l1) (cdr l2)))])))

  (check-false (eqlist?.v2 '(a) '(b)))
  (check-false (eqlist?.v2 '(a a) '(b a)))
  (check-false (eqlist?.v2 '(a a) '(b a)))
  (check-false (eqlist?.v2 '(a a) '(a (a))))
  (check-false (eqlist?.v2 '(a (a b)) '(a (a c))))
  (check-true (eqlist?.v2 '() '()))
  (check-true (eqlist?.v2 '(a) '(a)))
  (check-true (eqlist?.v2 '(a (a b)) '(a (a b)))))
