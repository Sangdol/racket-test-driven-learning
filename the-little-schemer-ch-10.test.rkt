#lang errortrace racket

(require rackunit)

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (build s1 s2) (cons s1 (cons s2 '())))

(test-case
  "Ch10. What is the Value of All of This?"

  (define new-entry build)

  (check-equal? (new-entry '(a b c) '(1 2 3)) '((a b c) (1 2 3)))

  (define lookup-in-entry
    (lambda (name entry entry-f)
      (lookup-in-entry-help name
                            (first entry)
                            (second entry)
                            entry-f)))

  (define lookup-in-entry-help
    (lambda (name names values entry-f)
      (cond [(null? names) (entry-f name)]
            [(equal? name (car names))
             (car values)]
            [else (lookup-in-entry-help name
                                        (cdr names)
                                        (cdr values)
                                        entry-f)])))

  (check-equal? (lookup-in-entry 'b '((a b) (1 2)) void) 2)

  (define extend-table cons)

  (check-equal? (extend-table 't1 '()) '(t1))

  (define lookup-in-table
    (lambda (name table table-f)
      (cond [(null? table) (table-f name)]
            [else (lookup-in-entry
                    name
                    (car table)
                    (lambda (name)
                      (lookup-in-table name (cdr table) table-f)))])))

  (define table '(((a b) (1 2)) ((c) (3))))
  (check-equal? (lookup-in-table 'a table void) 1)

  (define expression-to-action
    (lambda (e)
      (cond [(atom? e) (atom-to-action e)]
            [else (list-to-action e)])))

  (define atom-to-action
    (lambda (e)
      (cond [(number? e) *const]
            [(eq? e #t) *const]
            [(eq? e #f) *const]
            [(eq? e 'cons) *const]
            [(eq? e 'car) *const]
            [(eq? e 'cdr) *const]
            [(eq? e 'null?) *const]
            [(eq? e 'eq?) *const]
            [(eq? e 'atom?) *const]
            [(eq? e 'zero?) *const]
            [(eq? e 'add1) *const]
            [(eq? e 'sub1) *const]
            [(eq? e 'number?) *const]
            [else *identifier])))

  (define list-to-action
    (lambda (e)
      (cond [(atom? (car e))
             (cond [(eq? (car e) 'quote) *quote]
                   [(eq? (car e) 'lambda) *lambda]
                   [(eq? (car e) 'cond) *cond]
                   [else *application])]
            [else *application])))

  (define *application
    (lambda (e table)
      (apply (meaning (function-of e) table)
             (evlis (arguments-of e) table))))

  (define apply
    (lambda (fun vals)
      (cond [(primitive? fun)
             (apply-primitive (second fun) vals)]
            [(non-primitive? fun)
             (apply-closure (second fun) vals)])))

  (define meaning
    (lambda (e table)
      ((expression-to-action e) e table)))

  (define *const
    (lambda (e table)
      (cond [(number? e) e]
            [(eq? e #t) #t]
            [(eq? e #f) #f]
            [else (build 'primitive e)])))

  (check-equal? (meaning 'car '()) '(primitive car))
  (check-equal? (*const 1 '()) 1)
  (check-equal? (*const #t '()) #t)
  (check-equal? (*const 'car '()) '(primitive car))

  (define *quote
    (lambda (e table)
      (text-of e)))

  (define text-of second)

  (check-equal? (*quote '(quote a) '()) 'a)

  (define *identifier
    (lambda (e table)
      (lookup-in-table e table initial-table)))

  ; Let's hope it will never be used.
  ; Why don't we use `void`? -> it could swallow an error.
  (define initial-table
    (lambda (name)
      (car '())))

  (define *lambda
    (lambda (e table)
      (build 'non-primitive
             (cons table (cdr e)))))

  ;
  ; (*lambda (lambda (x) (cons x y)) (((y z) ((8) 9)))
  (define table-of first)
  (define formals-of second)
  (define body-of third)

  (define e '(lambda (x) (cons x y)))
  (set! table '(1 2))

  ; (non-primitive
  ;   (table formals body))
  (check-equal? (*lambda e table)
                '(non-primitive ((1 2) (x) (cons x y))))

  ; eval cond
  (define evcon
    (lambda (lines table)
      (cond [(else? (question-of (car lines)))
             (meaning (answer-of (car lines)) table)]
            [(meaning (question-of (car lines)) table)
             (meaning (answer-of (car lines)) table)]
            [else (evcon (cdr lines) table)])))

  (define else?
    (lambda (x)
      (cond [(atom? x) (eq? x 'else)]
            [else #f])))

  (define question-of first)
  (define answer-of second)

  (define *cond
    (lambda (e table)
      (evcon (cond-lines-of e) table)))

  (define cond-lines-of cdr)

  (set! table '(((coffee) (#t)) ((klatsch party) (5 (6)))))
  (set! e '(cond (coffee klatsch) (else party)))
  ;(check-equal? (*cond e table)) TODO

  ; eval list?
  (define evlis
    (lambda (args table)
      (cond [(null? args) '()]
            [else (cons (meaning (car args) table)
                        (evlis (cdr args) table))])))

  (define function-of car)
  (define arguments-of cdr)

  ; representation: (primitive primitive-name)
  (define primitive?
    (lambda (l)
      (eq? (first l) 'primitive)))

  ; presentation: (primitive (table formals body))
  (define non-primitive?
    (lambda (l)
      (eq? (first l) 'non-primitive)))

  (define apply-primitive
    (lambda (name vals)
      (cond [(eq? name 'cons)
             (cons (first vals) (second vals))]
            [(eq? name 'car)
             (car (first vals))]
            [(eq? name 'cdr)
             (cdr (first vals))]
            [(eq? name 'null?)
             (null? (first vals))]
            [(eq? name 'eq?)
             (eq? (first vals) (second vals))]
            [(eq? name 'atom?)
             (:atom? (first vals))]
            [(eq? name 'zero?)
             (zero? (first vals))]
            [(eq? name 'add1)
             (add1 (first vals))]
            [(eq? name 'sub1)
             (sub1 (first vals))]
            [(eq? name 'number?)
             (number? (first vals))])))

  (define :atom?
    (lambda (x)
      (cond [(atom? x) #t]
            [(null? x) #f]
            [(eq? (car x) 'primitive) #t]
            [(eq? (car x) 'non-primitive) #t]
            [else #f])))

  (check-equal? (apply-primitive 'car '((a b))) 'a)

  ; what is closure here? what does it look like? why is it called closure?
  ; -> function body - lambda
  ; -> evaluated lambda: a lambda fun
  ; -> as it's a function with env (table)?
  ;
  ; what are vals?
  ; -> table
  (define apply-closure
    (lambda (closure vals)
      (meaning (body-of closure)
               ; build arguments->values map (entry)
               (extend-table (new-entry (formals-of closure) vals)
                             (table-of closure)))))

  (check-equal? (expression-to-action '(car (a b))) *application)
  (check-equal? (expression-to-action '(cond (a b))) *cond)
  (check-equal? (list-to-action '(car (a b))) *application)
  (check-equal? (list-to-action '(cond (a b))) *cond)

  (check-equal? (expression-to-action 'car) *const)

  ;
  ; *application tests
  ;
  ; application: ((lambda (x) (add1 x)) 3)
  (check-equal? (meaning '(lambda (x) (add1 x)) '()) '(non-primitive (() (x) (add1 x))))  ; function
  (check-equal? (evlis '(3) '()) '(3))  ; arguments

  ; evlis tests
  (check-equal? (evlis '((lambda (x) (add1 x))
                         (lambda (y) (add1 y))) '())
                '((non-primitive (() (x) (add1 x)))
                  (non-primitive (() (y) (add1 y)))))

  ;
  ; apply tests
  ;
  (check-true (non-primitive? '(non-primitive (() (x) (add1 x)))))

  (define closure '(() (x) (add1 x)))
  (check-equal? (body-of closure) '(add1 x))
  (check-equal? (table-of closure) '())
  (check-equal? (formals-of closure) '(x))
  (check-equal? (new-entry '(x) '(3)) '((x) (3)))
  (check-equal? (extend-table '((x) (3)) '()) '(((x) (3))))
  (check-equal? (apply-primitive 'add1 '(3)) 4)
  (check-equal? (apply-closure '(() (x) (add1 x)) '(3)) 4)
  (check-equal? (apply '(non-primitive (() (x) (add1 x))) '(3)) 4)

  ;
  ; meaning tests
  ;
  (check-equal? (meaning 'x '(((x y) (1 2)))) 1)
  (check-equal? (meaning 'y '(((x y) (1 2)))) 2)

  ;
  ; evcon tests
  ;
  (check-true (else? 'else))
  (check-equal? (question-of '((null? l) l)) '(null? l))
  (check-equal? (answer-of '((null? l) l)) 'l)
  (check-equal? (meaning '(null? '()) '()) #t)
  (check-equal? (evcon '(((null? '()) 1)) '()) 1)

  (define value
    (lambda (e)
      (meaning e '())))

  (check-equal? (value 'car) '(primitive car))
  (check-equal? (value '(sub1 1)) 0))

