#lang racket

;; Implementation of a run-time system for VecVar-Lang
;; ------------------------------------------------------------------------

;; variables with no particular value
(define rax (void))
(define r8 (void))
(define r9 (void))

;; infinite memory
(define memory (make-vector 1000))

;; Black Magic
(define-namespace-anchor ns)

(define (run s)
  (eval s (namespace-anchor->namespace ns)))

;;------------------------------------------------------------------------

;; Source Language: VecVar-Lang

;; p ::= (begin s ... (halt loc))
;; s ::= (set! loc loc) | (set! loc integer)
;; loc ::= vec-var?

;; Target Language: Racket

;; e ::= ... | (begin e ...) | (vector-ref e e) | (vector-set! e e e) | (set! x e)
;;    |  rax | r8 | r9 | memory | integer 

;; recognizes a vec var
(define (vec-var? s)
  (and (symbol? s) (regexp-match? #rx"v[0-9]+" (symbol->string s))))

;; extracts a vec var index
(define (vec-var->index s)
  (string->number (second (regexp-match #rx"v([0-9]+)" (symbol->string s)))))

;; goal: implement and design a compiler from vecvar to Racket 
;; our compiler assumes well-formed inputs 
(define (compile-vecvar-lang p)

  ;; TODO design and implement compile-vecvar-lang
  (define (compile-p p
    (match p
    [`(begin ,s ... (halt, loc)) ;; ends with a halt or loc 
      `(begin
        ,@(map compile-s s) ;; trying to consist a length of arbitrary length; use unquote splciing
        (set! rax (vector-ref memory ,(vec-var->index loc))))]))
  )
  (compile-p p))

  (define (compile-i s)
    (match i
      [`(set! ,loc1 ,loc2)
       #:when (and (vec-var? loc1) (vec-var? loc2))
       (vector-set! memory ,(vec-var->index loc1)loc1 loc2)
      ]

      [`(set! ,loc1 ,int64)
       #:when (int64? int64)
       (vector-set! memory int64)
      ]

      [`(halt loc)
      #:when (vec-var? loc)
       (set! (vector-ref loc) rax)
      ]
    )
  )

(module+ test
  (require rackunit)

  (begin
    (run (compile-vecvar-lang `(begin (set! v1 5) (halt v1))))
    (check-equal? rax 5))

  (begin
    (run (compile-vecvar-lang `(begin (set! v1 120) (set! v2 v1) (halt v2))))
    (check-equal? rax 120)))

(module+ debug
  ;; inspect run-time system
  (displayln rax)
  (displayln memory))
