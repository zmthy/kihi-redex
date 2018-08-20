#lang racket

(require redex
         "base.rkt")

(provide (all-defined-out))


(define-extended-language typed base
  ((S T U) (A ...))
  ((A B C) (⇒ S T)))

(define-judgment-form typed
  #:mode (shape I I I)
  #:contract (shape v S T)
  [(shape () S S)
   Identity]
  [(shape (t_s t ...) S U)
   (type t_s T U)
   (shape (t ...) S T)
   Operate])

(define-judgment-form typed
  #:mode (type I I I)
  #:contract (type t S T)
  [(type v (A ...) ((⇒ S T) A ...))
   (shape v S T)
   Abstraction]
  [(type · ((⇒ (A ...) (B ...)) A ...) (B ...))
   Apply]
  [(type ← ((⇒ S (A ...)) B C ...) ((⇒ S (B A ...)) C ...))
   Left]
  [(type → ((⇒ (B A ...) T) B C ...) ((⇒ (A ...) T) C ...))
   Right]
  [(type × (A B ...) (A A B ...))
   Copy]
  [(type ↓ (A B ...) (B ...))
   Drop])
