#lang racket

#|
e ::= x number (let ([x e] ...) e)
|#

#|
I want to *decide* when are two `e`s equivalent.

e e -> boolean?

"Decide" -> always return #t of #f for all possible inputs.

Soundly approximate (w.r.t. contextual equivalence)
program equivalence on `e`s, using alpha-equivalence.

"Sound" -> my approximation will return #t when it's "actually" #t
        -> may return "#f" when it's actually "#t"
        -> will never return "#t" when it's actually "#f"

"Complete" -> if the property actually "#t", then my approximation
              certainly returns "#t"


|#

; what template? -> two-one-of template
; matching on two things simulatenously

; alpha equivalence is structural equivalence
(define (alpha-equivalent? e1 e2)
  (let alpha-equivalent? ([e1 e1]
                          [e2 e2]
                          [env '()]))

  (match (cons e1 e2)
    ; can also do [(cons x x)]
    [(cons x x)
      #:when (and (symbol? x) (eq? x y))
      (void)
    ]
    [(cons x y)
      #:when (and (symbol? x) (symbol? y))
      ;equal if bound and have the same value
      (or (eq? (dict-ref env x) y)
          (eq? (dict-ref env y) x))
    ]
    [(cons i j)
      #:when (and (integer? i) (integer? j))
      (equal? i j)
    ]
    [(cons `(let ([,xs ,els] ...) ,e2)
           `(let ([,xs ,fls] ...) ,f2))
      (void)
    ]
  )
)

(module+ test+
  (require rackunit)

  (check-false
   (alpha-equivalent?s
    'y
    'x))

  (check-true
   (alpha-equivalent?
    '(let ([x 5]) x)
    '(let ([y 5]) y))))
