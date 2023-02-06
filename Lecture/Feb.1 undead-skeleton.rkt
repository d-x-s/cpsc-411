#lang racket

'(module
   ((locals (x.1 y.2)))
   (begin
     (set! y.1 1)
     (set! x.1 1)
     (halt y.1)))

'(module
   ((locals (x.1 y.2)))
   (begin
     (set! x.1 1)
     (set! y.1 1)
     (halt y.1)))

'(module
   ((locals (x.1 y.2)))
   (begin
     (set! x.1 1)
     (begin
       (set! y.1 1))
     (halt y.1)))

'(module
   ((locals (x.1 y.2)))
   (begin
     (set! y.1 1)
     (set! x.1 2)
     (set! y.2 (* y.2 x.1))
     (begin
       (set! x.1 (+ x.1 -1))
       (set! y.2 (* y.2 x.1))
       (begin
         (set! x.1 (+ x.1 -1))
         (set! y.2 (* y.2 x.1))))
     (halt y.2)))

;; skeleton of above
'(
  ()
  ()
  ()
  (
    ()
    ()
    (
      ()
      ()))
  ())

(define (undead-set-tree? ust)
  (match ust
    [x #:when (undead-set? x)
       ; (halt y.2), (set! x.1 1)
       #t]
    [`(,usts ... ,ust2)
     ;(begin effects ... effect2)
     (and (andmap undead-set-tree? usts) (undead-set-tree? ust2))]))

(define (ff-for-undead-set-tree-and-tail ust tail)
  (match (cons ust tail)
    [(cons
      x
      `(halt ,triv))
     (... case for halt ...)]
    [(cons
      `(,ust ... ,ust_tail)
      `(begin ,effect ... ,tail))
     (... case for begin ...)
     (ff-for-undead-set-tree-and-tail ust_tail tail)]))

(define (ff-for-undead-set-tree-and-effect ust effect)
  (match (cons ust effect)
    [(cons
      `(,usts ... ,ust_effect)
      `(begin ,effects ... ,effect))
     (... case for begin ...)
     (map ff-for-undead-set-tree-and-effect usts effects)
     (ff-for-undead-set-tree-and-effect ust_effect effect)]
    [(cons ust effect)
     (... case for single instruction ...)]))

;; Local Variables:
;; eval: (put 'module 'racket-indent-function 0)
;; End:
