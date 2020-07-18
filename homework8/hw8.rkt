#lang racket
(require "hw8-util.rkt")
(require "interp.rkt")
(require rackunit)
(provide (all-defined-out))

;; Utility function that converts a variable into a string
;; Useful when translating from SimpleJS into LambdaJS
(define (mk-field x)
  (match x [(s:variable x) (k:string (symbol->string x))]))

;; Utility function that allocates a j:object.
;; (mk-object) allocates an empty object
;; (mk-object (cons "foo" (k:number 1)) (cons "bar" (j:variable 'x)))
;;  allocates an object with one field "foo" and one field "bar"
(define/contract (mk-object . args)
  (-> (cons/c string? j:expression?) ... j:alloc?)
  (define (on-elem pair)
    (cons (k:string (car pair)) (cdr pair)))
  (j:alloc (j:object (make-immutable-hash (map on-elem args)))))

;; End of testing API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "interp.rkt")
;; LambdaJS interpreter
(define (j:eval js [env empty-env])
  ((interp env) (j:quote js)))

;; SimpleJS interpreter
(define (s:eval x [env empty-env])
  (j:eval (translate (s:parse x)) env))

(define (s:eval+ x [env empty-env])
  (j:eval (translate (desugar (s:parse x))) env))

;; Translates SimpleJS into LambdaJS
(define (compile given)
  (j:quote (translate (s:parse given))))

(define-check (compiles? given expected)
  (check-equal? (compile given) expected))

;;;;;;;;;;;;;;;
;; Exercise 1

(define/contract (translate exp)
  (-> s:expression? j:expression?)
  (match exp
    [(? k:const? k) k]
    [(s:variable x) (j:variable x)]
    [(s:let (s:variable x) s1 s2)
     (j:let (j:variable x) (translate s1) (translate s2))]
    [(s:apply f ea) (j:apply (translate f) (map translate ea))]
    [(s:load (s:variable x) y)
     (j:get (j:deref (j:variable x)) (mk-field y))]
    [(s:assign (s:variable x) y e)
     (mk-let (translate e)
             (lambda (var)
               (j:seq
                (j:assign (j:variable x)
                          (j:set (j:deref (j:variable x))
                                 (mk-field y) var)) var)))]
    [(s:function (list x ...) y)
      (mk-object (cons (symbol->string '$code)
                       (j:lambda (cons (j:variable 'this) (map translate x))
                                 (translate y)))
                 (cons (symbol->string 'prototype) (mk-object)))]
    [(s:new (s:variable ef) args)
     (mk-let (j:deref (j:variable ef))
             (lambda (ctor)
               (mk-let (mk-object (cons (symbol->string '$proto)
                                        (j:get ctor (mk-field (s:variable 'prototype)))))
                       (lambda (o)
                         (j:seq
                          (j:apply
                           (j:get ctor (mk-field (s:variable '$code)))
                           (cons o (map translate args)))
                          o)))))]
    [(s:invoke (s:variable x) y n) 
     (j:apply (j:get (j:deref 
                      (j:get (j:deref (j:variable x))
                             (mk-field y))) 
                     (k:string (symbol->string '$code))) (cons (j:variable x) (map translate n)))]
    [_ (error"Error no match found " exp)]))


;; Function
(match
  (s:eval
    '(function () 99))
  [(box (hash-table ("$code" f) ("prototype" (box (hash-table)))))
   (check-equal? (f (list null)) 99)])

(match
  (s:eval
    '(function (x) x))
  [(box (hash-table ("$code" f) ("prototype" (box (hash-table)))))
   (check-equal? (f (list null 1)) 1)])

;;di Exercise 4

(define/contract (desugar exp)
  (-> s:expression? s:expression?)
  (match exp
    [(s:let x e1 e2) (s:let x (desugar e1) (desugar e2))]
    [(s:assign o f e) (s:assign o f (desugar e))]
    [(s:function xs e) (s:function xs (desugar e))]
    [(s:new ef ea) (s:new (desugar ef) (map desugar ea))]
    [(s:apply ef ea) (s:apply (desugar ef) (map desugar ea))]
    [(s:invoke o m ea) (s:invoke o m (map desugar ea))]
    [(s:class parent ms) 'todo]
    [_ exp]))


;;;;;;;;;;;;;;;
;; Exercise 2 (MANUALLY GRADED)
#|
PLEASE REPLACE THIS TEXT BY YOU ANSWER.
YOU MAY USE MULTIPLE LINES.
|#
