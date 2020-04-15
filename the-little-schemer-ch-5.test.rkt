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
  (check-equal? (insertR* 'a 'b '((b c) b d d)) '((b a c) b a d d)))

