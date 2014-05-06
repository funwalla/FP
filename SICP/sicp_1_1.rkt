#lang planet neil/sicp

;; Exercise 1.1
; 10                                    10
; (+ 5 3 4)                             12
; (- 9 1)                                8
; (/ 6 2)                                3
; (+ (* 2 4) (- 4 6))                    6
; (define a 3)                           no output
; (define b (+ a 1))                     no output
; (+ a b (* a b))                        19
; (= a b)                                #f
; (if (and (> b a) (< b (* a b)))        4
;     b
;     a)
; (cond ((= a 4) 6)                      16
;       ((= b 4) (+ 6 7 a))
;       (else 25))
; (+ 2 (if (> b a) b a))                 6
; (* (cond ((> a b) a)                   16
;          ((< a b) b)
;          (else -1))
;    (+ a 1))

;; Exercise 1.3
;(define (square x) (* x x))
;
;(define (sum-of-squares x y)
;  (+ (square x) (square y)))
;
;(define (f a b c) 
;  (cond ( (and (<= a b) (<= a c))  (sum-of-squares b c))
;        ( (and (<= b a) (<= b c)) (sum-of-squares a c))
;        (else (sum-of-squares a b))
;        )
;  )
;
;(f 2 2 3)

;; Exercise 1.4
;(define (a-plus-abs-b a b)
;  ( (if (> b 0) + -) a b) )
;
;(a-plus-abs-b 3 4)
;
;; The function returns a + |b|. If b > 0 then the "if" expression
;; returns "+" giving a + b. If b < 0 then the "if" expression
;; returns "-" giving a - b.

;; Exercise 1.5
;
;(define (p) (p))
;
; (define (test x y)
;   (if (= x 0)
;       0
;       y))
; 
; ;(test 0 (p))
; 
; ; Applicative-order evaluation: the function arguments will be
; ; evaluated before they are passed to the test function causing
; ; causing the function to fail.
; ; Normal-order evaluation: the function arguments are not evaluated
; ; prior to being passed to the test function whichm since x = 0,
; ; does not evaluate y = (p) and so does not hang.
 
;;; 1.1.7 Example: Square Roots by Newton's Method
;
;(define (square x) (* x x))
;
;(define (average x y) (/ (+ x y) 2))
;
;(define (improve guess x) (average guess (/ x guess)))
;
;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))
;
;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x) x)))
;
;(define (sqrt x) (sqrt-iter 1.0 x))


;;; Exercise 1.6
;
;(define (new-if predicate then-clause else-clause)
;  (cond (predicate then-clause)
;        (else else-clause)))
;
;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x) x)))
;
;; Since Scheme/Racket is using applicative evaluation, both
;; arguments (guess and sqrt-iter(...) are evaluated prior
;; to being passed to the new-if function, sending sqrt-iter into
;; an infinite loop. "if" is a special form so the "then" and "else"
;; clauses are not evaluated before the predicate.

;; Exercise 1.7

; The decimal precision of the good-enough? test is fixed precision
; (currently 0.001), and would not be able to differentiate between
; its arguments if they are so small that they differ by less than 
; the precision (e.g., guess = 0.0001 and x = 0.0002). For example,
; the functions defined above give (sqrt 0.0001) = 0.03230844833048122
; rather than 0.01.

; Very large limited precision numbers could overflow the precision 
; before they reach the decimal place of the predicate and so give
; unpredicatable results that would not satisfy the predicate,
; resulting in a potentially infinite loop. This happens on my 
; interpreter when the number reaches 999999999999999999999

;(define (square x) (* x x))
;
;(define (average x y) (/ (+ x y) 2))
;
;(define (improve guess x) (average guess (/ x guess)))
;
;(define (good-enough? guess priorguess)
;  (< (abs (- guess priorguess)) 0.001))
;
;(define (sqrt-iter guess x priorguess)
;  (if (good-enough? guess priorguess)
;      guess
;      (sqrt-iter (improve guess x) x guess)))
;
;(define (sqrt x) (sqrt-iter 1.0 x 5.0))

; This implementation gives (sqrt 0.0001) = 0.010000714038711746
; which is correct to the precision required and considerably more
; accurate than the original implementation. 
;
; It also works better for large numbers with 
; (sqrt 999999999999999999999) converging to a "correct" answer
; of 31622776601.683792 and (sqrt 9999999999999999999999) 
; (i.e., one more 9) correctly giving 1.0 x 10^11.

;; Exercise 1.8
;
;(define (square x) (* x x))
;
;(define (improve guess x) 
;  ( / (+ (/ x (square guess)) (* 2 guess)) 3))
;
;(define (good-enough? guess priorguess)
;  (< (abs (- guess priorguess)) 0.00000001))
;
;(define (cuberoot-iter guess x priorguess)
;  (if (good-enough? guess priorguess)
;      guess
;      (cuberoot-iter (improve guess x) x guess)))
;
;(define (cuberoot x) (cuberoot-iter 1.0 x 5.0))









 

 
 