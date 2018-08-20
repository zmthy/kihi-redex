#lang racket

(require redex
         (except-in "base.rkt" eval if)
         (prefix-in base/ "base.rkt"))

(provide bound
         valid?
         eval
         expand
         expand-term)


(define-extended-language bound base
  (t .... = x)
  (x variable-not-otherwise-mentioned)
  #:binding-forms
  (t_l ... = (x) v #:refers-to x t_r ...))


(define-judgment-form bound
  #:mode (valid? I)
  #:contract (valid? v)

  [(valid? ())]

  [(valid? (t v ...))
   (side-condition (not (equal? (term t) (term =))))
   (valid? v) ...]

  [(valid? (= (x) v v_r ...))
   (valid? v)
   (valid? v_r) ...])


(define eval
  (extend-reduction-relation
   base/eval
   bound
   #:domain v

   (--> (t ... = (x) v_b v_x v ...)
        (t ... ,@(term (substitute v_b x v_x)) v ...)
        Bind)))


(define-term bind ; Name (s → t) s → (s → t) s
  =)


(define-metafunction bound
  expand : t ... -> (t ...)
  [(expand = (x) v t_tail ...)
   (↓ t_bound ... t_cont ...)
   (where (t_bound ...) (bind-body x (expand-term v)))
   (where (t_cont ...) (expand t_tail ...))]
  [(expand t t_tail ...)
   ((expand-term t) t_cont ...)
   (where (t_cont ...) (expand t_tail ...))]
  [(expand) ()])

(define-metafunction bound
  expand-term : t -> t
  [(expand-term (t ...)) (expand t ...)]
  [(expand-term t) t])

(define-metafunction bound
  bind-body : x v -> v
  [(bind-body x (t t_tail ...))
   (t_bound ... t_cont ...)
   (where (t_bound ...) (bind-name x t))
   (where (t_cont ...) (bind-body x (t_tail ...)))]
  [(bind-body x ()) ()])

(define-metafunction bound
  bind-name : x t -> (t ...)
  [(bind-name x x) (×)]
  [(bind-name x v)
   (,@(term swap) → (↓ t ...) ×)
   (where (t ...) (bind-body x v))]
  [(bind-name x t) (· ← (t))])


(define-metafunction bound
  rec : (x) v -> v
  [(rec (x) v) (,@(term Y) (= (x) v))])
