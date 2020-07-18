#lang racket
;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

(define ex1

  (+ (+ (* 9 10) (/ 3 14)) (/ 3 14)))

(define ex2
  (list
   (+ (+ (* 9 10) (/ 3 14)) (/ 3 14))
   (+ (+ 90 (/ 3 14)) (/ 3 14))
   (+ (+ 90 3/14) (/ 3 14))
   (+ 1263/14 (/ 3 14))
   (+ 1263/14 3/14)
   633/7
   )
  )
 
(define (ex3 x y)
  (<= (+ (* x y) (+ x 6)) (- x (+ 15 14))))

;; Constructs a tree from two trees and a value
(define (tree left value right)
  (list left value right))
;; Constructs a tree with a single node
(define (tree-leaf value) (list '() value '()))

;; Accessors
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value) (tree (tree-left self) value (tree-right self)))
(define (tree-set-left self left) (tree left (tree-value self) (tree-right self)))
(define (tree-set-right self right) (tree (tree-left self) (tree-value self) right))

;; Function that inserts a value in a BST
(define (bst-insert self value)
  (cond [(null? self) (tree '() value '())]
        [(equal? value (tree-value self)) (tree-set-value self value)]
        [(< value (tree-value self)) (tree-set-left self (bst-insert (tree-left self) value))]
        [else (tree-set-right self (bst-insert (tree-right self) value))]
    )
 )
; (define x (tree (tree 1 2 3) 7 (tree 3 4 5)))
;(define x (tree (tree '() 6 '()) 7 (tree '() 8 '())))
;(bst-insert x 8)
;'((() 6 ()) 7 (() 8 ()))
; (bst-insert x 7)
;'((() 6 ()) 7 (() 8 ()))
;(bst-insert x 6)
;'(6 7 (() 8 ()))
;; lambda
(define (lambda? node)
  (cond [(and (list? node)
              (>= (length node) 3)
              (equal? (first node) 'lambda)
              (list? (second node))
              (andmap symbol? (second node))
              (not (empty? (third node))) 
              )#t]
        [else #f] ))


;  (cond [(equal? (list (first node) (second node) (third node)) (list 'lambda '(node) 'node)) #t]
   ;     [else #f])

(require rackunit)
;(list 'lambda '(x) 'x)
;(check-true (lambda? (quote(lambda (x) x))))
;(check-false (lambda? 3))
;(check-false (lambda? '(lambda x 3)))
;(check-false (lambda? '(lambda 1 1)))
;(check-false (lambda? '(lambda 1 '())))


;(check-false (lambda? (quote 3)))
;;; args not symbols and 1+ terms
;(check-false (lambda? (quote (lambda (3)))))
;;; args not symbols but has 3 datums
;(check-false (lambda? (quote (lambda (3) ()))))
;;; hits all the cases
;(check-true (lambda? (quote (lambda (x y z) 5))))
;;; args not symbols
;(check-false (lambda? (quote (lambda (x 3 z) 5))))
;;; doesn't have 1+ terms
;(check-false (lambda? (quote (lambda (x)))))
;;; not a lambda
;(check-false (lambda? (quote (- 3 1))))
;;; hits all the cases
;(check-true (lambda? (quote (lambda (a b c) x y z))))
;(check-false (lambda? '(lambda (x) ())))
;(check-true (lambda? '(lambda () (+ 1 2))))
;(check-true (lambda? '(lambda (x) (+ 1 2))))
;(check-false (lambda? '(lambda ())))
;(check-false (lambda? '(define ())))
;(check-false (lambda? (quote 3)))
;(check-false (lambda? '(lambda x 3)))
;

;;added the following:
;(check-false (lambda? '(lambda x 3)))
;(check-false (lambda? '(lambda 1 1)))
;(check-false (lambda? '(lambda 1 '())))





(define (lambda-params node)
  (cond [(equal? (first node) 'lambda)
         (second node)]

  ))

;(check-true (equal?  (list 'x) (lambda-params (quote (lambda (x) 1)))))
;(check-false (equal? () (lambda-params (quote (lambda (x))))))

;(define (lambda-body node)
 ; (cond [(and (equal? (first node) 'lambda)
  ;            (lambda? node)
   ;      (> (length node) 2)
    ;     (or
     ;     (not (empty? (second node)))
      ;    (not (empty? (third node)))
       ;   )
        ; 
         ;)
       ;  (list (third node))]
       ; [else '()]
       ; ))

(define (lambda-body node)
  (cond [(and
          (equal? (first node) 'lambda)
          ;(list? node)
          (> (length node) 2)
          )
         (list (third node)) ]
        [(equal? (length node) 2) '()]
        
        ))

(check-equal? '() (lambda-body (quote (lambda (x) ))))
(check-true (equal?  (list 1) (lambda-body (quote (lambda (x) 1)))))
(check-true (equal?  (list 'y) (lambda-body (quote (lambda (x) y)))))
(check-equal? '((y)) (lambda-body (quote(lambda (x) (y)))))
(check-equal?  '('(x y z)) (lambda-body (quote(lambda (x) '(x y z)))))
(check-equal? '((x)) (lambda-body (quote (lambda (x) (x) ))))




;; apply
(define (apply? l)
  (cond
    [(and
      (list? l)
      (not (empty? l))
      (andmap symbol? l) )#t]
    [else #f]
    )
  )

;(check-false (apply? (quote (lambda (x) x))))
;(check-true  (apply? (quote (x y))))
;(check-false (apply? (quote (3 x))))
;(check-false (apply? (quote 3)))
;(check-false (apply? (quote ())))


(define (apply-func node)
  (cond [(and
          (list? node)
          (andmap symbol? node)          
          )
         (first node)]))

(check-equal? 'x (apply-func (quote (x y))))
(check-equal? 'x (apply-func (quote (x z y))))

(define (apply-args node)
  (cond [(andmap symbol? node)
  (list (second node))]))

(check-equal? (list 'y) (apply-args (quote (x y))))

;; define
;(define (define? node)
 ; (cond [(define-func? node) #t]
  ;      [else #f]))

(define (define? node)
  (cond 
    [(and     (list? node)
              (not (empty? node))
              (equal? (first node) 'define)
              (>= (length node) 3)
              (not (empty? (second node)))
              (not (empty? (third node)))
              ;(not (symbol? (second node)))
              (or
               (symbol? (second node))
              ; (andmap symbol? (second node))
               (and (list? (second node))
                    (andmap symbol? (second node)) )
               )
              ;(or () ())
              ) #t]
        [else #f]
   ))

;(check-false (define? (quote (define 3 x))))
;(check-true (define? (quote (define (f x) (+ x 1)))))
;(check-true (define? (quote (define x 3))))
;(check-true (define? (quote (define (x) 3))))
;(check-true (define? (quote (define (x y z) 1))))
;(check-true (define? (quote (define (x y z) '1))))
;(check-true (define? (quote (define (x y z) (list 1 2 3)))))
;(check-false (define? (quote (void))))
;(check-false (define? (quote (define))))
;(check-false (define? (quote (define (void)))))
;(check-false (define? (quote (define ()))))
;(check-false (define? (quote (define x))))
;(check-false (define? (quote (define -))))
;(check-false (define? (quote (define 3))))
;(check-false (define? (quote ())))
;(check-false (define? (quote 3)))
;(check-false (define? (quote x)))
;(check-false (define? (quote -)))
;(check-false (define? (quote (define car))))
;(check-false (define? (quote (define define))))
;(check-false (define? (quote (define 3 x))))


(define (define-basic? node)
  (cond [(and (equal? (first node) 'define)
              (list? node)
              (> (length node) 2)
              ;(not (symbol? (third node)))
              (symbol? (second node))
             ; (or (symbol? (second node))
              ;    (andmap symbol? (second node)))
              #t)]
        [else #f]))

;(check-true (define-basic? (quote (define x 3))))
;(check-true (define-basic? (quote (define (x y) 3))))
;(check-false (define-basic? (quote (define x))))
;(check-true (define-basic? (quote (define x (lambda (x) x)))))




(define (define-func? node)
  (cond [(and
          (equal?  (first node) 'define)
          (>= (length node) 3)
          (and 
          (list? (second node))
          (not (empty? (second node))) )
          (or
          (symbol? (second node))
          (andmap symbol? (second node)) )
          )
            #t]
        [else #f])
  )

;(check-true (define-func? (quote (define (x) 3))))
;(check-false (define-func? (quote (define (x)))))
;(check-false (define-func? (quote (define (3) 3))))
;(check-false (define-func? (quote (define () 3))))
;(check-false (define-func? (quote (define))))
;(check-false (define-func? (quote (define (x y 3) 3))))

