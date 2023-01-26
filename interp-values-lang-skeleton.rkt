#lang racket

;; Tree-Values-Lang
;; p ::= (let* ([x p] ...) p) v ;; any subsequent p can refer to any previous x's 
                                ;; in this sense the final p can refer to everything in the previous body
;; v ::= x integers

;; TODO Exercise 1: Implement the interpreter functionally
(define (interp-tree-values-lang p)
  (define (interp-v v acc)
    (match p
      [`(let* ([,xs ,ps] ...)
        ,body)]
      (for/fold ([acc 0]
                 []))
      )
    v)

  (define (interp-p p acc)

    p)

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
