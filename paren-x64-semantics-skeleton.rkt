#lang racket

(require
 cpsc411/compiler-lib
 cpsc411/langs/v1)

(provide
 interp-paren-x64)

#|
A valid Paren-x64 expression is a `p` following the grammar below

p ::= (begin i ...)
i ::= (set! reg reg)
      (set! reg int64)
      (set! reg_1 (* reg_1 int32))
      (set! reg_1 (* reg_1 reg_2))
      (set! reg_1 (+ reg_1 int32))
      (set! reg_1 (+ reg_1 reg_2))

reg ::= rax r8 r9

The value of the program is the value of rax after the final instruction in the
sequence `i ...`.

The program is only valid if rax is initialized by the end of the program.
|#

#;
(define (paren-x64-template p)
  (define (paren-x64-template-p p)
    (match p
      [`(begin ,is ...)
       (TODO is)]))
  (define (paren-x64-template-i i)
    (match i
      [`(set! ,reg1 ,reg2)
       #:when (register? reg2)
       (TODO ...)]
      [`(set! ,reg1 ,int64)
       #:when (int64? int64)
       (TODO ...)]
      [`(set! ,reg_1 (* ,reg_1 ,int32))
       #:when (int32? int32)
       (TODO ...)]
      [`(set! ,reg_1 (* ,reg_1 ,reg))
       #:when (register? reg)
       (TODO ...)]
      [`(set! ,reg_1 (+ ,reg_1 ,int32))
       (TODO ...)]
      [`(set! ,reg_1 (+ ,reg_1 ,reg_2))
       (TODO ...)]
      ; alternatively:
      #;[`(set! ,reg_1 (+ ,reg_1^ ,reg_2))
         #:when (equal? reg_1 reg_1^)
         (TODO ...)]))

  (paren-x64-template-p p))

;; any/c -> boolean?
(define (paren-x64? p)
  (with-handlers ([values (lambda _ #f)])
    (check-paren-x64 p)
    (init-paren-x64? p)))

(module+ test
  (require rackunit)

  (check-false (paren-x64? `(begin)))

  (check-false (paren-x64? `(begin (set! 5 5))))

  (check-true (paren-x64? `(begin (set! rax 5)))))


;; Paren-x64-p -> int64
;; This program interprets a Paren-x64 program, returning its value.

(define/contract (interp-paren-x64 p)
  (-> paren-x64-v1? int64?)

  (define (interp-p p)
    (TODO "Finish interp-p"))


  (define (interp-i i)
    (TODO "Finish interp-i"))

  (interp-p p))

(module+ test
  (require rackunit)

  (check-equal?
   (interp-paren-x64 '(begin (set! rax 5)))
   5)

  (check-equal?
   (interp-paren-x64 '(begin (set! rax 5) (set! rax (+ rax rax))))
   10)

  (check-equal?
   (interp-paren-x64 '(begin (set! rax 5) (set! rax (+ rax 32))))
   37)

  ;; ??
  #;(check-equal?
   (interp-paren-x64 '(begin))
   ??)

  ;; ??
  #;(check-equal?
     (interp-paren-x64 '(begin (set! rax r8)))
     ??)
  )

;; Any -> Paren-x64 or error
(define (check-paren-x64 p)
  (define (validate-paren-x64-p p)
    (TODO "finish validate-paren-x64-p"))

  (define (validate-paren-x64-i i)
    (TODO "finish validate-paren-x64-i"))

  #;(validate-paren-x64-p p)
  p)

(module+ test
  (require rackunit)

  (check-not-exn
   (thunk (check-paren-x64 `(begin))))

  (check-exn
   exn:fail?
   (thunk (check-paren-x64 `(begin (set! 5 5))))))

;; Paren-x64 -> Paren-x64 or error
(define (check-init-paren-x64 p)
  (define (validate-paren-x64-p p)
    (TODO "finish validate-paren-x64-p"))

  (define (validate-paren-x64-i i)
    (TODO "finish valida-paren-x64-i"))

  #;(validate-paren-x64-p p)
  p)

(module+ test
  (require rackunit)

  (check-exn values (thunk (check-init-paren-x64 `(begin))))

  ;; invalid test
  #;(check-exn values
             (thunk (check-init-paren-x64 `(begin (set! 5 5)))))

  (check-true (check-init-paren-x64 `(begin (set! rax 5)))))

;; Paren-x64 -> boolean?
(define (init-paren-x64? p)
  (with-handlers ([values (lambda _ #f)])
    (check-init-paren-x64 p)
    #t))

(module+ test
  (require rackunit)

  (check-false (init-paren-x64? `(begin)))

  ;; invalid test
  #;(check-false (init-paren-x64? `(begin (set! 5 5))))

  (check-true (init-paren-x64? `(begin (set! rax 5)))))
