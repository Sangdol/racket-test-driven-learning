#lang errortrace racket

(require rackunit)

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define pick
  (lambda (n lat)
    (cond [(zero? (sub1 n)) (car lat)]
          [else (pick (sub1 n) (cdr lat))])))

(define (build s1 s2) (cons s1 (cons s2 '())))

(define a-pair?
  (lambda (x)
    (cond [(atom? x) #f]              ; '(a)
          [(null? x) #f]              ; '()
          ; [(null? (cdr x)) #f]        ; '(a) - why this is needed?
          [(null? (cdr (cdr x))) #t]  ; '(a b)
          [else #f])))                ; '(a b c ...)

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define (one? n)
  (= n 1))

(test-case
  "Ch9. ...and Again, and Again, and Again,..."

  ; partial function - (because this could keep looking?)
  (define looking
    (lambda (a lat)
      (keep-looking a (pick 1 lat) lat)))

  ; what about a cycle? that's why it's a partial function.
  ; sorn: symbol or number
  ; This is "unnatural" recursion.
  (define keep-looking
    (lambda (a sorn lat)
      (cond [(number? (pick sorn lat)) (keep-looking a (pick sorn lat) lat)]
            [else (eq? a (pick sorn lat))])))

  (check-true (looking 'a '(3 a 2)))
  (check-false (looking 'a '(3 b 2)))

  (define shift
    (lambda (pair)
      (build (first (first pair))
             (build (second (first pair))
                    (second pair)))))

  (check-equal? (shift '((a b) c)) '(a (b c)))
  (check-equal? (shift '((a b) (c d))) '(a (b (c d))))

  ; pora: pair or atom
  (define align
    (lambda (pora)
      (cond [(atom? pora) pora]
            [(a-pair? (first pora))
             (align (shift pora))]
            [else (build (first pora)
                         (align (second pora)))])))

  (check-equal? (align '((a b) c)) '(a (b c)))
  (check-equal? (align '((a b) ((c d) e))) '(a (b (c (d e)))))

  (define length*
    (lambda (pora)
      (cond [(atom? pora) 1]
            [else (+ (length* (first pora))
                     (length* (second pora)))])))

  (check-equal? (length* '((a b) c)) 3)
  (check-equal? (length* '((a b) (c d))) 4)

  ; `align` is not a partial function
  ; as it yields a value for every argument
  ; based on the weight* function.
  ; -> it means that there won't be an argument which causes an infinite loop.
  (define weight*
    (lambda (pora)
      (cond [(atom? pora) 1]
            [else (+ (* (weight* (first pora)) 2)
                     (weight* (second pora)))])))

  (check-equal? (weight* '((a b) c)) 7)
  (check-equal? (weight* '(a (b c))) 5)

  ; this is partial (not total) for '((a b) (c d))
  (define shuffle
    (lambda (pora)
      (cond [(atom? pora) pora]
            [(a-pair? (first pora))
             (shuffle (revpair pora))]
            [else (build (first pora)
                         (shuffle (second pora)))])))

  (check-equal? (shuffle '(a (b c))) '(a (b c)))
  (check-equal? (shuffle '(a b)) '(a b))

  ; The conjecture of Lothar Collatz
  ; the 3n + 1 problem which is not solved.
  (define C
    (lambda (n)
      (cond [(one? n) 1]
            [(even? n) (C (/ n 2))]
            [else (C (add1 (* n 3)))])))

  (check-equal? (C 3) 1)
  (check-equal? (C 4) 1)
  (check-equal? (C 5) 1)
  (check-equal? (C 1000) 1))
