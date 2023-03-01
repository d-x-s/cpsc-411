#lang racket

#|
Source:

e ::= (let ([x e]) e) | (if e e e) | (binop e e) | integer? | x

|#

;; TODO 1: Implement an interpreter for Source

;; e-> integer?
;; evaluates the program to a value
(define (interp-source e)

  (define EMPTY-ENV '())
  #; (define EMPTY-ENV (make-immutable-hash))

  (define (interp-binop e v1 v2)
    (match e
      ['+ (+ v1 v2)]
      ['- (- v1 v2)]
      ['* (* v1 v2)]
    )
  )

  (define (interp-e e env)
    (match e
    [`(let ([,x1, e1]) ,e2) (void)]
    [`(if ,pred ,e1 ,e2) (void)]
    [`(,binop ,e1 ,e2) (void)]
    [(? symbol?) (void)]
    [(? integer) (void)]
    ) 
  )
  (interp-e -e EMPTY-ENV)
)

#|
Monadic Lang:

V ::= integer? | x
N ::= V | (binop V V)
C ::= N | (let ([x C]) C) | (if V C C)

|#

;; TODO 2: Implement the monadic form transformation for Source.
(define (monadic-form e)
  (void))

(module+ test
  (require rackunit)
  (check-equal?
    (interp-source '(if 0 1 2))
    2)

  (check-equal?
    (interp-source '(if 1 1 2))
    1)

  (check-equal?
    (interp-source '())
    2)
  )

;; TODO 3: Implement an interpreter for Monadic, in 1 line of code.
(define (interp-monadic e)
  (void))

#|
Statement Lang:

V ::= integer? | x
N ::= V | (binop V V)
C ::= N | (set! x N) | (begin C ... C) | (if V C C)

|#

;; TODO 4: Implement statementify to translate Monadic Lang into Statement Lang.
(define (statementify C)
  (void))

;; TODO 5: Implement the interpreter for Statement Lang.
(define (interp-statement e)
  (void))


;; TODO 6: Modify the design of statement lang so that it has no top-level
;; expressions.
;;
;; That is, the following would no longer be valid:
;;   (begin (set! x (+ 4 5)) x)
;; and would have to be written
;;   (begin (set! x (+ 4 5)) (halt x))

;; TODO 7: Update statementify to produce the new language; you should modify
;; exactly 1 line of code.

;; TODO 8: Update interp-statement so that the main loop never returns.
