#lang racket

;; Tree-Values-Lang
;; p ::= (let* ([x p] ...) p) | v
;; v ::= x | integer?

;; TODO Exercise 1: Implement the interpreter functionally
(define (interp-tree-values-lang p)
  (define (interp-v v acc)
    (match v
      [(? integer?)
       v]
      [(? symbol?)
       (dict-ref acc v)]))

  (define (interp-p p acc)
    (match p
      [`(let* ([,xs ,ps] ...)
          ,body)
       (define new-acc
         (for/fold ([acc acc])
                   ([x xs]
                    [p ps])
           (dict-set acc x (interp-p p acc))))
       (interp-p body new-acc)]
      [_ (interp-v p acc)]))

  (interp-p p '()))

(module+ test
  (require rackunit)
  (check-equal?
   (interp-tree-values-lang `(let* ([x 5]) x))
   5)

  (check-equal?
   (interp-tree-values-lang `(let* ([x (let* ([y 120])
                                         y)]
                                    [y 5])
                               y))
   5)

  (check-equal?
   (interp-tree-values-lang `(let* ([y 5]
                                    [x (let* ([y 120])
                                         y)])
                               y))
   5)

  (check-equal?
   (interp-tree-values-lang `(let* ([x (let* ([y 120])
                                        y)]
                                   [y 5])
                               x))
   120)

  (check-exn
   exn:fail?
   (thunk
    (interp-tree-values-lang `(let* ([x (let* ([y 120])
                                          y)])
                                y)))))

;; TODO Exercise 2: Implement the interpreter imperatively
(define (interp-tree-values-lang! p)
  (define env (make-hash))
  (define (interp-v v)
    v)

  (define (interp-p p)
    p)

  (interp-p p))

(module+ test
  (check-equal?
   (interp-tree-values-lang! `(let* ([x 5]) x))
   5)

  (check-equal?
   (interp-tree-values-lang! `(let* ([x (let* ([y 120])
                                          y)]
                                     [y 5])
                                y))
   5)

  (check-equal?
   (interp-tree-values-lang! `(let* ([y 5]
                                    [x (let* ([y 120])
                                         y)])
                                y))
   5)

  (check-equal?
   (interp-tree-values-lang! `(let* ([x (let* ([y 120])
                                         y)]
                                    [y 5])
                                x))
   120)

  (check-exn
   exn:fail?
   (thunk
    (interp-tree-values-lang! `(let* ([x (let* ([y 120])
                                          y)])
                                 y)))))
