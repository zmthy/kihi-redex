#lang racket

(require redex
         (except-in "base.rkt" eval if)
         (prefix-in base/ "base.rkt"))


(define-extended-language bound base
  (t .... = x)
  (x variable-not-otherwise-mentioned)
  #:binding-forms
  (t_l ... = (x) (t ...) #:refers-to x t_r ...))


(define eval
  (extend-reduction-relation
   base/eval
   bound
   #:domain p

   (--> (t ... = (x) (t_x ...) p_x p ...)
        (t ... ,@(term (substitute (t_x ...) x p_x)) p ...)
        Bind)))


(define-term bind ; Name (s → t) s → (s → t) s
  =)


(define-metafunction bound
  bind-all : t -> t
  [(bind-all (= (x) (t_x ...) t ...))
   (,@(term (bind-in (t_x ...) x)) ,@(term (bind-all (t ...))))]
  [(bind-all (t_e t ...))
   ((bind-all t_e) ,@(term (bind-all (t ...))))]
  [(bind-all t) t])

(define-metafunction bound
  bind-in : p x -> t
  [(bind-in p x) (↓ ,@(term (bind-with (bind-all p) x)))])

(define-metafunction bound
  bind-with : p x -> p
  [(bind-with (t_t t ...) x)
   (,@(term (bind-term t_t x)) ,@(term (bind-with (t ...) x)))]
  [(bind-with () x) ()])

(define-metafunction bound
  bind-term : t x -> t
  [(bind-term x x) (×)]
  [(bind-term p x) (· ← (→ (bind-in p x)) ×)]
  [(bind-term t x) (· ← (t))])


(define-metafunction bound
  rec : (x) p -> p
  [(rec (x) p) (,@(term Y) (= (x) p))])
