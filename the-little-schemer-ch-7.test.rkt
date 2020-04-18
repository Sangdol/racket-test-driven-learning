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

(define firsts
  (lambda (l)
    (cond [(null? l) l]
          [else (cons (car (car l)) (firsts (cdr l)))])))

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
  (check-true (intersect? '(a) '(a b)))

  (define intersect
    (lambda (set1 set2)
      (cond [(null? set1) '()]
            [(member? (car set1) set2)
             (cons (car set1) (intersect (cdr set1) set2))]
            [else (intersect (cdr set1) set2)])))

  (check-equal? (intersect '(a) '(b)) '())
  (check-equal? (intersect '(a b) '(b)) '(b))
  (check-equal? (intersect '(a b) '(b a)) '(a b))
  (check-equal? (intersect '(a b c) '(b a)) '(a b))

  (define union
    (lambda (set1 set2)
      (cond [(null? set1) set2]
            [(member? (car set1) set2) (union (cdr set1) set2)]
            [else (cons (car set1) (union (cdr set1) set2))])))

  (check-equal? (union '() '()) '())
  (check-equal? (union '(a) '()) '(a))
  (check-equal? (union '(a) '(a)) '(a))
  (check-equal? (union '(a b) '(a)) '(b a))

  (define diffset
    (lambda (set1 set2)
      (cond [(null? set1) '()]
            [(member? (car set1) set2) (diffset (cdr set1) set2)]
            [else (cons (car set1) (diffset (cdr set1) set2))])))

  (check-equal? (diffset '() '()) '())
  (check-equal? (diffset '(a) '()) '(a))
  (check-equal? (diffset '(a) '(a)) '())
  (check-equal? (diffset '(a b) '(a)) '(b))

  ; for non-empty set
  ; l-set: (l)ist of (set)
  (define intersectall
    (lambda (l-set)
      (cond [(null? (cdr l-set)) (car l-set)]
            [else (intersect (car l-set)
                             (intersectall (cdr l-set)))])))

  (check-equal? (intersectall '((a) (b))) '())
  (check-equal? (intersectall '((a b) (b))) '(b))
  (check-equal? (intersectall '((a b) (b) (a b c))) '(b))

  (define a-pair?
    (lambda (x)
      (and (not (null? x))
           (not (null? (car x)))
           (not (null? (cdr x)))
           (null? (cdr (cdr x))))))

  ; book version
  (set! a-pair?
    (lambda (x)
      (cond [(atom? x) #f]              ; '(a)
            [(null? x) #f]              ; '()
            ; [(null? (cdr x)) #f]        ; '(a) - why this is needed?
            [(null? (cdr (cdr x))) #t]  ; '(a b)
            [else #f])))                ; '(a b c ...)

  (check-false (a-pair? '()))
  (check-false (a-pair? '(a b c)))
  (check-true (a-pair? '(a b)))
  (check-true (a-pair? '(a (b))))

  (define (first p) (car p))
  (define (second p) (car (cdr p)))
  (define (third p) (car (cdr (cdr p))))

  (check-equal? (first '(a b c)) 'a)
  (check-equal? (second '(a b c)) 'b)
  (check-equal? (third '(a b c)) 'c)

  (define (build s1 s2) (cons s1 (cons s2 '())))

  (check-equal? (build 'a 'b) '(a b))

  ; rel: set of pairs (relation)
  ; fun: function - what does 'function' has to do with this function?
  ; finite function: a list of pairs in which no first element of any pair is the same
  ;                  as any other first element - why?
  (define fun?
    (lambda (rel)
      (set? (firsts rel))))

  (check-false (fun? '((a b) (a c))))
  (check-true (fun? '((a b) (e c))))

  ; (rev)erse (rel)ation
  (define revrel
    (lambda (rel)
      (cond [(null? rel) '()]
            [else (cons (build (second (car rel)) (first (car rel)))
                        (revrel (cdr rel)))])))

  (check-equal? (revrel '((a b) (e c))) '((b a) (c e)))

  (define revpair
    (lambda (pair)
      (build (second pair) (first pair))))

  (check-equal? (revpair '(a b)) '(b a))

  (set! revrel
    (lambda (rel)
      (cond [(null? rel) '()]
            [else (cons (revpair (car rel)) (revrel (cdr rel)))])))

  (check-equal? (revrel '((a b) (e c))) '((b a) (c e)))

  (define seconds
    (lambda (l)
      (cond [(null? l) l]
            [else (cons (car (cdr (car l))) (seconds (cdr l)))])))

  (define fullfun?
    (lambda (fun)
      (and (set? (seconds fun)))))

  (check-false (fullfun? '((a b) (e b))))
  (check-true (fullfun? '((a b) (e c)))))
