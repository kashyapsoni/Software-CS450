#lang racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at:

   https://www.umb.edu/life_on_campus/dean_of_students/student_conduct

|#
(require "hw7-util.rkt")
(require rackunit)
(provide (all-defined-out))

(define/contract (env-put env var val)
  (-> handle? d:variable? d:value? eff-op?)
  (eff-op (lambda (mem)
     (eff (environ-put mem env var val) (d:void)))))
 
(define/contract (env-push env var val)
  (-> handle? d:variable? d:value? eff-op?)
  (eff-op (lambda (mem)
    (environ-push mem env var val))))
  
(define/contract (env-get env var)
  (-> handle? d:variable? eff-op?)
  (eff-op (lambda (mem)
    (eff mem (environ-get mem env var)))))

(define/contract (d:eval-exp env exp)
  (-> handle? d:expression? eff-op?)
   (match exp
    [(d:number a) (eff-pure exp)]
    [(d:bool x) (eff-pure exp)]
    [(d:variable x) (do a <- (env-get env exp) (eff-pure a))]
    [(d:lambda (list x) y) (eff-pure (d:closure env exp))]
    [(d:closure E y) (eff-pure exp)]
    [(d:apply (d:apply (d:apply (d:variable 'if) (list exp)) (list then)) (list else))
     (do
      ay <- (d:eval-exp env exp)
     (match ay 
      [(d:bool #f) (d:eval-exp env else)]
      [_ (d:eval-exp env then)]))]
    [(d:apply ef (list ea1))    
        (do
         vf <- (d:eval-exp env ef)
          (match vf
            [(d:builtin x) (eff-pure (x ea1))]
            [(d:closure E (d:lambda (list x) v2)) 
             (do
               va <- (d:eval-exp env ea1)
               Eb <- (env-push E x va)
               vb <- ((d:eval-term-impl) Eb v2)
               (eff-pure vb))]))]
     [_ (error "Unknown expression:" exp)]))

(define/contract (d:eval-term env term)
  (-> handle? d:term? eff-op?)  
   (match term
    [(d:define x y)
      (do
        val <- (d:eval-term env y)
        E <- (env-put env x val)
        (eff-pure E))]
     [(d:seq t1 t2)
      (do
        v1 <- (d:eval-term env t1)
        v2 <- (d:eval-term env t2)
        (eff-pure v2))]
     [_ ((d:eval-exp-impl) env term)]))

;; Use this dynamic parameter in d:eval-term for improved testing (see Lecture 31)
(define d:eval-exp-impl (make-parameter d:eval-exp))
;; Use this dynamic parameter in d:eval-exp for improved testing (see Lecture 31)
(define d:eval-term-impl (make-parameter d:eval-term))

;; Parameter body *must* be a curried term already
(define/contract (break-lambda args body)
  (-> (listof d:variable?) d:term? d:lambda?)
  (match args
  [(list) (d:lambda (list (d:variable '_)) body)]
  [(list a) (d:lambda (list a) body)]
  [(list a t ...) (d:lambda (list a) (break-lambda t body))]))

;; ef is a curried expression and args is a list of curried expressions
(define/contract (break-apply ef args)
  (-> d:expression? (listof d:expression?) d:expression?)
  (match args
  [(list) (d:apply ef (list (d:void)))]
  [(list y) (d:apply ef (list y))]
  [(list y z) (d:apply (d:apply ef (list y)) (list z))]
  [(list y z ...) (break-apply (d:apply ef (list y)) z)]))

;; Use this dynamic parameter in d:curry for improved testing (see Lecture 31)
(define break-lambda-impl (make-parameter break-lambda))
;; Use this dynamic parameter in d:curry for improved testing (see Lecture 31)
(define break-apply-impl (make-parameter break-apply))

(define/contract (d:curry term)
  (-> d:term? d:term?)
  (match term
   [(d:variable x) term]
   [(list) term]
   [(list a b c ...) term]
   [(list ef) (list (d:curry ef))]
   [(d:apply ef args) ((break-apply-impl) ef (d:curry args))]
   [(d:lambda ef args) ((break-lambda-impl) ef (d:curry args))]
   [(d:define x y) (d:define x (d:curry y))]
   [(d:seq t1 t2) (d:seq (d:curry t1) (d:curry t2))]))
