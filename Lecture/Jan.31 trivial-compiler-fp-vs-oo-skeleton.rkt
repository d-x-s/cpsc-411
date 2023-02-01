#lang racket

(require
 racket/class)

#|
Consider the following language:

p ::= (begin i ...)
i ::= (set! reg int64)

We want to compile this to an x64 instruction sequence, as a string.

In a typical OO style, we might formalize the compiler for this using the
visitor pattern, as below.
We develop a class for each node in the abstract syntax tree.
Each class has it's own "compile" method, which implements the visitor pattern.
|#

(define term-class%
  (class object%
    (super-new)
    (define/public (compile)
      (error "Undefined method"))))

;; First, a begin class for representing `(begin i ...)
(define begin-class%
  (class term-class%
    (super-new)
    (init-field instruction-ls)

    ;; () -> String
    (define/override (compile)
      ...)))

;; Then a set! class for representing `(set! reg integer)
(define set!-class%
  (class term-class%
    (super-new)
    (init-field opand1)
    (init-field opand2)

    ;; () -> String
    (define/override (compile)
      ...)))

;; Then we need a reader/parser, to transform the data representation into the
;; correct class structure.

;; Lang -> term-class%
(define (parse-oo e)
  ...)

;; Finally, we can compile by "parsing" then calling the compile method.

;; Lang -> String
(define (compile-oo e)
  (send (parse-oo e) compile))

;; By contrast, in the functional programming style, we would use the structural
;; recursion design. W pattern match on the data and then process, eventually
;; recurring on the structure of the data.
;; Each clause of the pattern match corresponds to a visit method in the OO
;; style.
;; Because we operate over the data directly, we do not need a parser.

;; Lang -> String
(define (compile-fp e)
  ...)


;; There are trade offs.
;;
;; The OO style would save us from recompiling the entire code base after adding
;; a new AST node.
;; We could add a new class, with a new visit method, and only compile that new
;; class.
;; In the FP style, we would need to modify the pattern match clause, and
;; recompile the affected module.
;;
;; However, in the FP style, we work directly over the data representation,
;; which frees us from writing a parser from data to classes.
;; It also allows us to handle the data a little more generically, by writing
;; code that is abstract with respect to parts of the data, rather than
;; transforming all of the data into concrete classes.

(module+ test
  (require rackunit)
  (check-equal?
   (compile-oo `(begin (set! rax 5) (set! rax 10)))
   "mov rax 5\n mov rax 10")

  (check-equal?
   (compile-fp `(begin (set! rax 5) (set! rax 10)))
   "mov rax 5\n mov rax 10"))
