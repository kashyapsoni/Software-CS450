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


(require rackunit)


(define (lambda-params node)
  (cond [(equal? (first node) 'lambda)
         (second node)]

        ))


(define (lambda-body node)
  (cond [(equal? (first node) 'lambda)
         (rest (rest node))
         ]))



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


(define (apply-func node)
  (cond [
         (not (empty? node))          
          
         (first node)]))

(check-equal? 'x (apply-func (quote (x y))))
(check-equal? 'x (apply-func (quote (x z y))))

(define (apply-args node)
  (cond [(andmap symbol? node)
         (rest node)]))

(check-equal? (list 'y) (apply-args (quote (x y))))

;; define
(define (define? node)
  (cond 
    [(and     (list? node)
              (not (empty? node))
              (equal? (first node) 'define)
              (>= (length node) 3)
              (not (empty? (second node)))
              (not (empty? (third node)))
              (or
               (symbol? (second node))
               (and (list? (second node))
                    (andmap symbol? (second node)) )
               )
              ) #t]
    [else #f]
    ))



(define (define-basic? node)
  (cond [(and (equal? (first node) 'define)
              (list? node)
              (> (length node) 2)
              (symbol? (second node))
              #t)]
        [else #f]))





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


