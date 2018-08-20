#lang racket

(require redex
         "base.rkt"
         (rename-in "base.rkt"
                    [eval base/eval])
         (only-in "bound.rkt"
                  bound
                  [valid? bound/valid?]
                  [eval bound/eval]
                  expand))

(define-syntax-rule (test a b)
  (test-->> base/eval (term a) (term b)))


(define (run red program)
  (match (apply-reduction-relation red (term ,program))
    [(list next) (run red next)]
    [(list) program]))


;; Empty program
(test () ())


;; Primitive programs
(test (· ()) ())

(test (← (·) ()) ((() ·)))

(test (→ (·) ()) ((· ())))

(test (↓ ()) ())

(test (× ()) (() ()))


;; Swapping inputs
(test (,@(term swap) (←) (→)) ((→) (←)))

(test (,@(term apply-with) (·) (×)) ((·) (·)))


;; Ignoring the first input
(test (,@(term under) (↓) (·) (×)) ((·)))

(test (,@(term under₂) (↓) (·) (·) (×)) ((·) (·)))


;; Ignoring the first input, but returning a value before it
(test (,@(term over) () (·) (×)) ((×) (·)))


;; Pre-processing name bindings is the same as running with substitution.
(test-equal (run bound/eval (term (= (x) (= (y) (y x) x) ()) #:lang bound))
            (run base/eval (term (expand = (x) (= (y) (y x) x) ()) #:lang bound)))


(test-results)
