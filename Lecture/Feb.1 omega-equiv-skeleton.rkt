#lang racket

;; TODO EXERCISE 3: Prove that the following three programs are not equivalent, or
;; argue that they are.

; calls itself,  running forever
(define (loop1) (let loop () (loop)))

; calls itself, running forever
(define (loop2) (loop2))

; as long as two programs are "pure", having no side effects
; and they run forever
; then they can be considered equivalent
; Turing Complete, Halting Problem

; we can't prove for sure that loop1 and loop2 are equivalent

; why are loop 2 and loop 3 not equivalent?
(define (omega) ((lambda (x) (x x)) (lambda (x) (x x))))
; is equal to itself forever

(define (f g)
  (g))

(module+ test
  (require rackunit)
  (check-equal?
   (f (loop1))
   (f (omega)))

  (check-equal?
   (f (loop2))
   (f (omega))))
