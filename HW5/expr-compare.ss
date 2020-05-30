#lang racket
(provide (all-defined-out))
(define ns (make-base-namespace))

(define (is-keyword? w)
  (if (or (equal? w 'quote) (equal? w 'let) (equal? w 'if) (equal? w 'lambda) (equal? w 'λ)) #t #f))

(define (simple-comp x y)
  (cond [(equal? x y) x]
	[(and (boolean? x) (boolean? y) (if x '% '(not %)))]
	[else `(if % ,x ,y)]))

(define (parse-lambda x y hashmp)
  (cond [(or (empty? x) (empty? y)) hashmp]
	[(equal? (car x) (car y))
	 (define hash1 (hash-set hashmp `(,(car x) "x") (car x)))
	 (define hash2 (hash-set hash1 `(,(car y) "y") (car y)))
	 (parse-lambda (cdr x) (cdr y) hash2)]
	[else (define hash1 (hash-set hashmp `(,(car x) "x") (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))))
	      (define hash2 (hash-set hash1 `(,(car y) "y") (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))))
	      (parse-lambda (cdr x) (cdr y) hash2)]))

(define (parse-comp x y)
  (cond [(or (empty? x) (empty? y)) '()]
        [(equal? (car x) (car y)) (cons (car x) (parse-comp (cdr x) (cdr y)))]
	[else (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y)))) (parse-comp (cdr x) (cdr y)))]))

(define (lambda-body-comp x y hashmp)
  (cond 
        [(and (not (list? x)) (not (list? y))) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
        [(or (not (list? x)) (not (list? y))) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
        [(not (= (length x) (length y))) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
        [(or (empty? x) (empty? y)) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
        [(xor (list? (car x)) (list? (car y))) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
        [(and (list? (car x)) (list? (car y))) (cons (lambda-list-body-comp (car x) (car y) hashmp) (lambda-body-comp (cdr x) (cdr y) hashmp))]
        [else (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)])
		(cons (lambda-body-comp (car x1) (car y1) hashmp) (lambda-body-comp (cdr x1) (cdr y1) hashmp)))]))

(define (lambda-list-body-comp x y hashmp)
  (cond 
        [(and (not (list? x)) (not (list? y))) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
        [(or (not (list? x)) (not (list? y))) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
        [(not (= (length x) (length y))) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
        [(or (empty? x) (empty? y)) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
        [(xor (list? (car x)) (list? (car y))) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
	[(and (equal? (car x) 'quote) (equal? (car y) 'quote)) (simple-comp x y)]
	[(or (and (equal? 'lambda (car x)) (equal? 'λ (car y))) (and (equal? 'lambda (car y)) (equal? 'λ (car x))))
	 (lambda-comp x y hashmp)]
	[(or (and (equal? 'lambda (car x)) (equal? 'lambda (car y))) (and (equal? 'λ (car x)) (equal? 'λ (car y))))
	 (lambda-comp x y hashmp)]
	[(xor (is-keyword? (car x)) (is-keyword? (car y))) (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)]) (simple-comp x1 y1))]
	[(and (list? (car x)) (list? (car y))) (cons (lambda-list-body-comp (car x) (car y) hashmp) (lambda-body-comp (cdr x) (cdr y) hashmp))]
	[else (let ([x1 (hash-ref hashmp `(,x "x") x)] [y1 (hash-ref hashmp `(,y "y") y)])
		(cons (lambda-body-comp (car x1) (car y1) hashmp) (lambda-body-comp (cdr x1) (cdr y1) hashmp)))]))

(define (lambda-comp x y hashmp)
  (cond [(not (= (length (cadr x)) (length (cadr y)))) (simple-comp x y)]
	[else (let ([l (if (or (equal? 'λ (car x)) (equal? 'λ (car y))) 'λ 'lambda)])  
		(cons l (cons (parse-comp (cadr x) (cadr y)) (lambda-body-comp (cddr x) (cddr y) (parse-lambda (cadr x) (cadr y) hashmp)))))]))


(define (nonlist-comp x y)
  (cond [(equal? x y) x]
        [(and (not (list? x)) (not (list? y))) (simple-comp x y)]
        [(or (not (list? x)) (not (list? y))) (simple-comp x y)]
        [(not (= (length x) (length y))) (simple-comp x y)]
        [(or (empty? x) (empty? y)) (simple-comp x y)]
        [(and (list? (car x)) (list? (car y))) (cons (expr-compare (car x) (car y)) (nonlist-comp (cdr x) (cdr y)))]
        [else (cons (nonlist-comp (car x) (car y)) (nonlist-comp (cdr x) (cdr y)))]))

(define (expr-compare x y)
  (cond [(equal? x y) x]
        [(and (not (list? x)) (not (list? y))) (simple-comp x y)]
        [(or (not (list? x)) (not (list? y))) (simple-comp x y)]
	[(not (= (length x) (length y))) (simple-comp x y)]
        [(or (empty? x) (empty? y)) (simple-comp x y)]
	[(and (equal? (car x) 'quote) (equal? (car y) 'quote)) (simple-comp x y)]
	[(or (and (equal? 'lambda (car x)) (equal? 'λ (car y))) (and (equal? 'lambda (car y)) (equal? 'λ (car x))))
	 (define hashmp (hash-set (hash) 'INIT 'INIT))
	 (lambda-comp x y hashmp)]
	[(or (and (equal? 'lambda (car x)) (equal? 'lambda (car y))) (and (equal? 'λ (car x)) (equal? 'λ (car y))))
	 (define hashmp (hash-set (hash) 'INIT 'INIT))
	 (lambda-comp x y hashmp)]
	[(xor (is-keyword? (car x)) (is-keyword? (car y))) (simple-comp x y)]
	[(and (list? (car x)) (list? (car y))) (cons (expr-compare (car x) (car y)) (nonlist-comp (cdr x) (cdr y)))]
	[else (cons (nonlist-comp (car x) (car y)) (nonlist-comp (cdr x) (cdr y)))]))



(define (test-expr-compare x y)
  (and (equal? (eval x ns) (eval (list 'let '([% #t]) (expr-compare x y)) ns))
       (equal? (eval y ns) (eval (list 'let '([% #f]) (expr-compare x y)) ns))))

(define test-expr-x
  '((lambda (a b c lambda l) a b c lambda l (and (equal? a b) (equal? lambda l)) '(5 6) (quote (5 6)) (length (list 1 3)) l)
    10 15 #f (list 1 2 3) (cons 1 2)))
(define test-expr-y '((lambda (b a c l lambda) a b c l lambda (and (equal? b a) (equal? l c)) '(5 7) (quote (5 7)) (length (list 1 2)) lambda)
    10 20 #t (list 1 2 4) 1))



#|
; TEST CASES FROM SPEC AND FROM PIAZZA
(display 1)
(if (equal? (expr-compare 12 20) '(if % 12 20)) "true" (expr-compare 12 20))
(display 2)
(if (equal? (expr-compare 12 12)    '12) "true" (expr-compare 12 12))
(display 3)
(if (equal? (expr-compare 12 20)   '(if % 12 20)) "true"  (expr-compare 12 20))
(display 4)
(if (equal? (expr-compare #t #t)    '#t) "true" (expr-compare #t #t) )
(display 5)
(if (equal? (expr-compare #f #f)    '#f) "true" (expr-compare #f #f))
(display 6)
(if (equal? (expr-compare #t #f)    '%) "true" (expr-compare #t #f))
(display 7)
(if (equal? (expr-compare #f #t)    '(not %)) "true" (expr-compare #f #t))
(display 8)
(if (equal? (expr-compare 'a '(cons a b))    '(if % a (cons a b))) "true" (expr-compare 'a '(cons a b)))
(display 9)
(if (equal? (expr-compare '(cons a b) '(cons a b))    '(cons a b)) "true" (expr-compare '(cons a b) '(cons a b)))
(display 10)
(if (equal? (expr-compare '(cons a lambda) '(cons a λ))    '(cons a (if % lambda λ))) "true" (expr-compare '(cons a lambda) '(cons a λ)))
(display 11)
(if (equal? (expr-compare '(cons (cons a b) (cons b c)) '(cons (cons a c) (cons a c)))
   '(cons (cons a (if % b c)) (cons (if % b a) c))) "true" (expr-compare '(cons (cons a b) (cons b c)) '(cons (cons a c) (cons a c))))
(display 12)
(if (equal? (expr-compare '(cons a b) '(list a b))  '((if % cons list) a b)) "true" (expr-compare '(cons a b) '(list a b)))
(display 13)
(if (equal? (expr-compare '(list) '(list a))    '(if % (list) (list a))) "true" (expr-compare '(list) '(list a)))
(display 14)
(if (equal? (expr-compare ''(a b) ''(a c))  '(if % '(a b) '(a c))) "true" (expr-compare ''(a b) ''(a c)))
(display 15)
(if (equal? (expr-compare '(quote (a b)) '(quote (a c)))    '(if % '(a b) '(a c))) "true" (expr-compare '(quote (a b)) '(quote (a c))))
(display 16)
(if (equal? (expr-compare '(quoth (a b)) '(quoth (a c)))    '(quoth (a (if % b c)))) "true" (expr-compare '(quoth (a b)) '(quoth (a c))))
(display 17)
(if (equal? (expr-compare '(if x y z) '(if x z z))    '(if x (if % y z) z)) "true" (expr-compare '(if x y z) '(if x z z)))
(display 18)
(if (equal? (expr-compare '(if x y z) '(g x y z))
	'(if % (if x y z) (g x y z))) "true" (expr-compare '(if x y z) '(g x y z)))
(display 19)
(if (equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) 
	'((lambda (a) ((if % f g) a)) (if % 1 2))) "true" (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)))
(display 20)
(if (equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
	'((λ (a) ((if % f g) a)) (if % 1 2))) "true" (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2)))


(display 21)
(if (equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
	    '((lambda (a!b) a!b) (if % c d))) "true" (expr-compare '((lambda (a) a) c) '((lambda (b) b) d)))


(display 22)
(if (equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d)) 
	'(if % '((λ (a) a) c) '((lambda (b) b) d))) "true" (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))	)
(display 23)
(if (equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
   '(+
     (not %)
     ((λ (a b!c) (f a b!c)) 1 2))
   ) "true" (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2))))
(display 24)
(if (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
	'((λ (a b) (f (if % a b) (if % b a))) 1 2)) "true" (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2)))
(display 25)
(if (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
   '((λ (a b!c) (f (if % a b!c) (if % b!c a)))
    1 2)) "true" (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2)))

(display 27)
(if (equal? (expr-compare '((lambda (a) (eq? a  ((λ      (a b) ((λ      (a b) (a b)) b a)) a (lambda (a) a)))) (lambda (b a) (b a)))
                          '((λ      (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a)) a (λ (b) a))))      (lambda (a b) (a b))))
   '((λ (a) ((if % eq? eqv?) a ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a))) a (λ (a!b) (if % a!b a))))) (lambda (b!a a!b) (b!a a!b)))
) "true" (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b)))))



(define-syntax-rule (assert output expected)
  (when  (not (equal? output expected))
    (display "Assertion `") (displayln "' failed.")
    (display "  Result: ") (displayln output)
    (display "Expected: ") (displayln expected)))

(assert (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
(assert (expr-compare '(lambda (a) a) '(lambda (b) b)) '(lambda (a!b) a!b))
(assert (expr-compare '(lambda (a) b) '(cons (c) b)) '(if % (lambda (a) b) (cons (c) b)))
(assert (expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3)) '((λ (if!fi) (+ if!fi 1)) 3))
(assert (expr-compare '(lambda (lambda) lambda) '(λ (λ) λ)) '(λ (lambda!λ) lambda!λ))
(assert (expr-compare ''lambda '(quote λ)) '(if % 'lambda 'λ))
(assert (expr-compare '(lambda (a b) a) '(λ (b) b)) '(if % (lambda (a b) a) (λ (b) b)))
(assert (expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b))) '(if % (λ (a b) (lambda (b) b)) (lambda (b) (λ (b) b))))
(assert (expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y))) '(λ (let) (let (((if % x y) 1)) (if % x y))))
(assert (expr-compare '(λ (x) ((λ (x) x) x))
              '(λ (y) ((λ (x) y) x))) '(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x))))
(assert (expr-compare '(((λ (g)
                   ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (λ () (n n))))
                    (λ (r) (x (λ () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9))
	'(((λ (g!x)
                    ((λ (x!n) (g!x (λ () (x!n x!n))))
                     (λ (x!r) (g!x (λ () (x!r x!r))))))
                  (λ (r!g)
                    (λ (n!x) (if (= n!x 0)
                                 1
                                 (* n!x ((r!g) (- n!x 1)))))))
                 (if % 10 9)))
(assert (expr-compare '(lambda (x y) (+ (x y) '(y x)))
              '(lambda (y x) (+ (y x) '(x y)))) '(lambda (x!y y!x) (+ (x!y y!x) (if % '(y x) '(x y)))))


|#
