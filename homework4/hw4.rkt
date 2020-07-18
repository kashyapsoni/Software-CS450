#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang racket
(provide (all-defined-out))
(require rackunit)
(require "hw4-util.rkt")
;; END OF REQUIRES

;; Utility functions
(define (s:apply-arg1 app)
  (first (s:apply-args app)))
(define (s:lambda-param1 lam)
  (first (s:lambda-params lam)))
(define (s:lambda-body1 lam)
  (first (s:lambda-body lam)))
;; Utility functions
(define (e:apply-arg1 app)
  (first (e:apply-args app)))
(define (e:lambda-param1 lam)
  (first (e:lambda-params lam)))
(define (e:lambda-body1 lam)
  (first (e:lambda-body lam)))

;; Exercise 1
(define (s:subst exp var val)
  (cond
    [(s:number? exp) exp]
    [(s:variable? exp)
     (cond [(equal? exp var) val]
           [else exp])]
    [(s:apply? exp)
     (s:apply (s:subst (s:apply-func exp) var val)
              (list (s:subst (s:apply-arg1 exp) var val)))]
    [(s:lambda? exp)
     (cond [(equal? (s:lambda-param1 exp) var) exp]
           [else (s:lambda
                  (list (s:subst (s:lambda-param1 exp) var val))
                  (list (s:subst (s:lambda-body1 exp) var val)))])]
))


;; Exercise 2
(define (s:eval subst exp)
  (cond
    [(s:value? exp) exp]
    [(s:variable? exp) (error "Error Variable Found")]
    [(s:apply? exp)
     (define ef (s:eval subst (s:apply-func exp)))
     (define va (s:eval subst (s:apply-arg1 exp)))
     (s:eval subst (subst (s:lambda-body1 ef)
                          (s:lambda-param1 ef)
                          va))]
))

;; Exercise 3
(define (e:eval env exp)
  (cond [(e:number? exp) exp]
        [(e:variable? exp)
         (cond [(hash-has-key? env exp) (hash-ref env exp)]
               [else (error "Variable not defined")] )]
        [(e:lambda? exp) (e:closure env exp)]
        [(e:apply? exp)    
         (define ef (e:eval env (e:apply-func exp)))
         (define ea (e:eval env (e:apply-arg1 exp)))         
         (define vb (e:eval (hash-set (e:closure-env ef)
                                      (e:lambda-param1 (e:closure-decl ef))
                                      ea)
                            (e:lambda-body1 (e:closure-decl ef))))        
         vb]
))


;; Exercise 4 (Manually graded)
#|

First, situation for without environment is better.
 -> We can say that it does not need any memory to save variables,
    as compared to With Environment.

Second, situation for With Environment is better
 -> With environment makes the run time evaluation faster, as
    compared to without environment, because the environment
    saves the variables and value in memory and can be used
    as needed, which is faster in speed at run time. As it uses
    hash tables for creating environment.

|#

;; Exercise 5 (Manually graded)
#|

Benefit 1.
 -> The formal specification helps to understand the software specification
    given that it gives the ability to show proof of the software application.
    Which can be a great benefit to see how better the implementation is, or
    how better it can get. Also, how correct it it, which is always a question
    when implementing anything in terms of software system.

Benefit 2.
 -> Also, the use of formal specification is a great way to communicate with
    each other without any limitations. And, one can help improve the specification
    of each other easily. Whcih can be a great benefit in terms of software system.

|#
