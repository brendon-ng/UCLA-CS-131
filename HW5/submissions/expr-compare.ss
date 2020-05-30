#lang racket

(define (is-keyword? w)
  (if (or (equal? w 'quote) (equal? w 'let) (equal? w 'if)) #t #f))

(define (simple-comp x y)
  (cond [(equal? x y) x]
	[(and (boolean? x) (boolean? y) (if x '% '(not %)))]
	[else `(if % ,x ,y)]))

(define (parse-lambda x y hashmp)
  (cond [(or (empty? x) (empty? y)) hashmp]
	[(equal? (car x) (car y)) (parse-lambda (cdr x) (cdr y) hashmp)]
	[else (define hash1 (hash-set hashmp `(,(car x) "x") (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))))
	      (define hash2 (hash-set hash1 `(,(car y) "y") (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))))
	      (parse-lambda (cdr x) (cdr y) hash2)]))

(define (parse-comp x y)
  (cond [(or (empty? x) (empty? y)) '()]
        [(equal? (car x) (car y)) (cons (car x) (parse-comp (cdr x) (cdr y)))]
	[else (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y)))) (parse-comp (cdr x) (cdr y)))]))

(define (lambda-body-comp x1 y1 hashmp)
  (let ([x (hash-ref hashmp `(,x1 "x") x1)] [y (hash-ref hashmp `(,y1 "y") y1)])
    (cond [(equal? x y) x]
	   [(and (not (list? x)) (not (list? y))) (simple-comp x y)]
	   [(or (not (list? x)) (not (list? y))) (simple-comp x y)]
	   [(not (= (length x) (length y))) (simple-comp x y)]
	   [(or (empty? x) (empty? y)) (simple-comp x y)]
	   [(and (equal? (car x) 'quote) (equal? (car y) 'quote)) (simple-comp x y)]
	   [(xor (list? (car x)) (list? (car y))) (simple-comp x y)]
	   [(and (list? (car x)) (list? (car y))) (cons (lambda-body-comp (car x) (car y) hashmp) (lambda-body-comp (cdr x) (cdr y) hashmp))]
	   [else (cons (lambda-body-comp (car x) (car y) hashmp) (lambda-body-comp (cdr x) (cdr y) hashmp))])))

(define (lambda-comp x y)
  (define hashmp (hash-set (hash) 'INIT 'INIT))
  (cons (parse-comp (car x) (car y)) (lambda-body-comp (cdr x) (cdr y) (parse-lambda (car x) (car y) hashmp))))


(define (list-comp x y)
  (cond [(equal? x y) x]
        [(and (not (list? x)) (not (list? y))) (simple-comp x y)]
        [(or (not (list? x)) (not (list? y))) (simple-comp x y)]
	[(not (= (length x) (length y))) (simple-comp x y)]
        [(or (empty? x) (empty? y)) (simple-comp x y)]
	[(or (and (equal? 'lambda (car x)) (equal? 'λ (car y))) (and (equal? 'lambda (car y)) (equal? 'λ (car x))))
	 (cons 'λ (lambda-comp (cdr x) (cdr y)))]
	[(or (and (equal? 'lambda (car x)) (equal? 'lambda (car y))) (and (equal? 'λ (car x)) (equal? 'λ (car y))))
	 (cons (car x) (lambda-comp (cdr x) (cdr y)))]
	[(xor (is-keyword? (car x)) (is-keyword? (car y))) (simple-comp x y)]
        [(xor (list? (car x)) (list? (car y))) (simple-comp x y)]
	[(and (list? (car x)) (list? (car y))) (cons (list-comp (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
	[else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]))

  
(define (expr-compare x y)
  (cond [(equal? x y) x]
	[(and (not (list? x)) (not (list? y))) (simple-comp x y)]
	[(or (not (list? x)) (not (list? y))) (simple-comp x y)]
	[(not (= (length x) (length y))) (simple-comp x y)]
	[(or (empty? x) (empty? y)) (simple-comp x y)]
	[(and (equal? (car x) 'quote) (equal? (car y) 'quote)) (simple-comp x y)]
	[(xor (is-keyword? (car x)) (is-keyword? (car y))) (simple-comp x y)]
	[(xor (list? (car x)) (list? (car y))) (simple-comp x y)]
	[(and (list? (car x)) (list? (car y))) (cons (list-comp (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
	[else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]))


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
(display 26)
(if (equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
                          '((lambda (if)     (+ if     if (f λ     ))) 3))
	'((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ))) 3))) "true" (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3)))
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




