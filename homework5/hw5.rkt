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
(require "hw5-util.rkt")
(require rackunit)
(provide d:eval-exp d:eval-term)
(define (d:apply-arg1 app)
  (first (d:apply-args app)))
(define (d:lambda-param1 lam) 
  (first (d:lambda-params lam)))
;; END OF REQUIRES
 
;; Exercise 1
(define/contract (d:eval-exp mem env exp)  
  (-> mem? handle? d:expression? eff?)
  (cond
    [(d:number? exp) (eff mem exp)]
    [(d:variable? exp) (eff mem (environ-get mem env exp))]
    [(d:lambda? exp) (eff mem (d:closure env exp))]
    [(d:closure? exp) (eff mem exp)]
    [(d:apply? exp)
     (define vf (d:eval-exp mem env (d:apply-func exp)))
     (define va (d:eval-exp (eff-state vf) env (d:apply-arg1 exp)))
     (define Eb (environ-push
                 (eff-state va)
                 (d:closure-env (eff-result vf))
                 (d:lambda-param1 (d:closure-decl (eff-result vf)))
                 (eff-result va)))
     (define vb (d:eval-term
                 (eff-state Eb)
                 (eff-result Eb)
                 (d:lambda-body (d:closure-decl (eff-result vf)))))        
     vb
     ]
    [else (eff mem " Unkown Expression ")]))

;; Exercise 2
(define/contract (d:eval-term mem env term)
  (-> mem? handle? d:term? eff?)
  (cond
    [(d:expression? term) (d:eval-exp mem env term)]
    [(d:define? term)
     (define val (d:eval-term mem env (d:define-body term)))
     (define var (d:define-var term))
     (define E (environ-put mem env var (eff-result val)))
     (eff E (d:void))
     ]
    [(d:seq? term)
     (define v1 (d:eval-term mem env (d:seq-fst term)))
     (define v2 (d:eval-term (eff-state v1) env (d:seq-snd term)))
     (eff (eff-state v2) (eff-result v2))
     ]
    [else (eff mem " Unkown Expression ")] ))



;; Exercise 3 (Manually graded)
#|
> (define a 2)
> a
2
> (define a 3)
> a
3

I just run this two commands on the racket command prompt.
I see that racket allows you to re-define the variable binding
in the same environment. It updates the variable binding.
But, in the case of lambdaD lamguage I do not think it would
let us do that, as our Environment, lets say the heap will
store only one variable 'a' binding in one Environment
and not two. Our works when you define it in the second
environment and make the handle for first environment in
the second one.
But rackets has mutable structure where it lets you change
the variable.


|#
