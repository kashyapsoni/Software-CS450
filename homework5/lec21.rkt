#lang racket
; McCarthy 91
(define (M n)
  (cond [(> n 100) (- n 10)]
        [(<= n 100)
         (define x (M (+ n 11)))
         (define y (M x))
         y]))

; Version 1
#|
(define (M n)
  (cond [(> n 100) (- n 10)]
        [(<= n 100) (M (M (+ n 11)))]))
|#

; (M 101) ; Hits the first branch
; (M 100) ; Hits the second branch

(define (qs lst)
    (cond [(empty? lst) empty]
          [else
            (define p (first lst))
            (define l (rest lst))
            (define l1 (qs (filter (lambda (x) (< x p)) l)))
            (define l2 (qs (filter (lambda (x) (>= x p)) l)))
            (append l1 (cons p l2))
          ]))

; (qs (list 50 3 42))

(require "lambdaf.rkt")
(require rackunit)
(check-equal?
  (f:void)
  (f:parse1 '(void)))

(check-equal? (f:number 10) (f:parse1 10))

; (closure (hash) (lambda (x) 1))
#|
(check-equal?
    (f:parse1 '(closure [] (lambda (x) 1)))
    (f:closure (hash)
        (f:lambda
            (list (f:variable 'x))
            (list (f:number 1)))))
|#
(check-equal?
    (f:parse1 '(lambda (x) 1))
    (f:lambda
        (list (f:variable 'x))
        (f:number 1)))

; (define x 10)
(check-equal?
    (f:define (f:variable 'x) (f:number 10))
    (f:parse1 '(define x 10)))
; (define x 10)
; x
(check-equal?
    (f:parse '[(define x 10)  x])
    (f:seq
        (f:define (f:variable 'x) (f:number 10))
        (f:variable 'x)))

; ---------
(define b (lambda (x) a))
(define a 20)
(b 1)
;(check-equal? (b 1) 20)


















;