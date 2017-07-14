#lang racket

(require redex)

(provide (all-defined-out))


(define-language base
  (p (t ...))
  (t · ← → ↓ × p))


(define eval
  (reduction-relation
   base
   #:domain p

   (--> (t ... · (t_t ...) p ...)
        (t ... t_t ... p ...)
        Apply)

   (--> (t ... ← (t_t ...) p_t p ...)
        (t ... (p_t t_t ...) p ...)
        Left)

   (--> (t ... → (t_t ...) p_t p ...)
        (t ... (t_t ... p_t) p ...)
        Right)

   (--> (t ... ↓ p_d p ...)
        (t ... p ...)
        Drop)

   (--> (t ... × p_c p ...)
        (t ... p_c p_c p ...)
        Copy)))


(define-term apply ; (s → t) s → t
  ·)

(define-term left ; (s → t) A u → (s → A t) u
  ←)

(define-term right ; (A s → t) A u → (s → t) u
  →)

(define-term drop ; A s → s
  ↓)

(define-term copy ; A s → A A s
  ×)


(define-term swap ; A B s → B A s
  (· ← ← ()))


(define-term apply-with ; A (A s → t) s → t
  (· ,@(term swap)))


(define-term under ; (s → t) A s → A t
  (· ←))

(define-term under₂ ; (s → t) A B s → A B t
  (,@(term under) → under))


(define-term over ; (s → B t) A s → B A t
  (,@(term swap) ,@(term under)))


(define-term swap-over ; A B C s → C A B s
  (,@(term over) swap))

(define-term swap-under ; A B C s → B C A s
  (,@(term under₂) → ()))

(define-term copy-over ; A B s → B A B s
  (,@(term over) (×)))

(define-term copy-under ; A B s → A B A s
  (,@(term swap-under) ×))


(define-term compose ; (u → v) (t → u) s → (t → v) s
  (→ → (· ,@(term under) (·))))


(define-term Y ; ((t → u) t → u) s → (t → u) s
  (→ × → (· ,@(term under) (→ → (·) ×))))


;; 𝔹 = ∀ s t. A A s → A s

(define-term true ; s → 𝔹 s
  ((↓ ,@(term swap))))

(define-term false ; s → 𝔹 s
  ((↓)))

(define-term not ; 𝔹 s → 𝔹 s
  (→ (· ,@(term under) swap)))

(define-term if ; 𝔹 (s → t) (s → t) s → t
  (· ·))

(define-term and ; 𝔹 𝔹 s → 𝔹 s
  (→ → (,@(term if) ,@(term under₂) false)))

(define-term or ; 𝔹 𝔹 s → 𝔹 s
  (→ → (,@(term if) ,@(term under) true)))


;; ℕ = ∀ s. (s → s) s → s

(define-term zero ; s → ℕ s
  ((↓)))

(define-term suc ; ℕ s → ℕ s
  (→ (· ,@(term under) (·) ,@(term copy-over))))

(define-term + ; ℕ ℕ s → ℕ s
  (,@(term apply-with) suc))

(define-term * ; ℕ ℕ s → ℕ s
  (· ,@(term under) (→ + ,@(term swap) ,@(term zero))))


;; Pair A B = ∀ s t. (A B s → t) s → t

(define-term pair ; A B s → Pair A B s
  (→ → (· ,@(term swap-over))))

(define-term both ; Pair A B s → A B s
  (,@(term apply-with) ()))

(define-term first ; Pair A B s → A s
  (,@(term under) (↓) ,@(term both)))

(define-term second ; Pair A B s → B s
  (↓ ,@(term both)))

(define-term map-pair ; (A B s → C D s) Pair A B s → Pair C D s
  (,@(term pair) · ,@(term under) both))


;; Option A = ∀ s t. (A s → s) s → s

(define-term none ; s → Option A s
  ((↓)))

(define-term some ; A s → Option A s
  (→ (,@(term apply-with))))

(define-term none? ; Option A s → 𝔹 s
  (· ,@(term under) ((,@(term false) ↓) ,@(term true))))

(define-term some? ; Option A s → 𝔹 s
  (,@(term not) ,@(term none?)))

(define-term map-option ; (A s → B s) Option A s → Option B s
  (,@(term apply-with) → (· ,@(term under) (↓)) ,@(term under₂) (,@(term none))))


;; List A = ∀ s. (A s → s) s → s

(define-term nil ; s → List A s
  ((↓)))

(define-term cons ; A List A s → List A s
  (→ → (,@(term apply-with) ,@(term under) (,@(term under) (·) ,@(term copy-over)))))

(define-term head ; List A s → Option A s
  (,@(term apply-with) (,@(term some) ,@(term under) (↓)) ,@(term none)))

(define-term map-list ; (A s → B s) List A s → List B s
  (,@(term apply-with) ,@(term compose) cons ,@(term under₂) nil))
