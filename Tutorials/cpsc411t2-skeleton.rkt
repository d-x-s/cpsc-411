#lang racket

;; CPSC 411 Tutorial 2: Type checking and Interpreters

;; Learning objectives

;; 1) Understand and implement a type checker for a very basic functional language
;;     (with basic imperative features!)
;; 2) Understanding mutation based accumulators.
;; 3) Understand and implement an interpreter for a very basic functional language
;;     (with basic imperative features!)
;; 4) Practice following the design recipe and Racket features (pattern matching and testing)

;; Let's write a type checker and interpreter!

;; Here's our language:
;; LL is for Little Lang :)

;; LL ::= numbers | #t | #f | (+ LL ...) | (- LL ...)
;;           | (if LL LL LL) | (and LL ...) | (or LL ...)
;;           | x | (let ([x LL] ...) LL)
;;           | (begin LL ...) | (set! x LL)

;; Here our language has the following features:
;; numbers, booleans
;; n-ary addition, subtraction
;; if expressions, and, or
;; variables
;; let expressions (for naming expressions)
;; begin expressions (for sequencing expressions)
;; set! expressions (for updating variables)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Type Checking
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; We'd like to write a type checker for this language.
;; A type checker makes sure that a given LL program
;; won't raise a run time error due to incompatible types.
;; For example, the type checker should check that an
;; LL program does not try to add booleans.

;; This leads us to a data definition:
;; Type ::= 'Number | 'Boolean

;; The signature of our typechecking function is thus:

;; typecheck-LL: LL -> Type

;; Following the design recipe further, we need examples
;; for our type checker.
;; Here are some examples that do type check:

; (check-equal? (typecheck-LL '(+ 2 3)) 'Number)
; (check-equal? (typecheck-LL '(let ([false #f]) (if false 3 4))) 'Number)

;; And here are some that do not:

; (check-exn exn:fail? (lambda () (typecheck-LL '(+ #t 4))))
;; This fails since the branches of the if expression are not the same type
; (check-exn exn:fail? (lambda () (typecheck-LL '(let ([false #f]) (if false #t 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q1: What are some examples of LL programs that /do/
;; type check? Write two more.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q2: What are some examples of LL programs that /do not/
;; type check? Write two more.


;; Not only do we have to type check to ensure functions like
;; + and `or` being applied to the correct types of arguments,
;; but we also have to have to think about correct scoping
;; of variables.

;; Design Question: Why not separate checking that a
;; program is well-scoped from type checking?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q3: What are some examples of LL programs that /are/
;; well scoped? Write two out.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q4: What are some examples of LL programs that /are not/
;; well scoped? Write two out.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q5: Write a template for processing LL.

;; Given all the steps of the design recipe have been followed,
;; We are ready to start writing typecheck-LL!

;; We need an accumulator to keep track of variables.
;; While we usually add an extra argument to the function to
;; serve as an accumulator, in this case we want to use an
;; accumulator that we use /mutation/ to update.
;; This is because we have to model mutation since
;; we can mutate variables in LL, we need an accumulator we
;; can mutate as well to update the types of variables as
;; the programs in LL can update the expressions of variables
;; (and thus the type)!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q6: Write typecheck-LL. Check it against the above
;; examples. Remember the accumulator; this can
;; keep track of the types of variables but also well-scopedness.
;; The accumulator should be a dictionary:
;; https://docs.racket-lang.org/reference/dicts.html

;; typecheck-LL: LL -> Type
;; Accumulator: TODO
#;(define (typecheck-LL ll)
    ;; The accumulator, also known as the typing context
    (define ctx (make-hash))

    (define (tc ll)
      ;; LL template goes here
      )

    (tc ll))

;; The above skeleton is given, since it is good design to
;; restrict the mutable variable to inside the function that needs it
;; as an accumulator. Otherwise, functions outside
;; typecheck-LL could update `ctx` and cause issues.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Interpreter
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A big assumption we will have when writing this interpreter
;; is that we are only interpreting type checked programs!

;; To write this interpreter, we will follow the design recipe: 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q7: Write the signature for interp-LL.

;; Think: Do we need an accumulator
;; (as we did for type checking)
;; and will we need to update it using mutation?
;; What should the function return, do we need a data definiton?

;; With our signature, we can start thinking of concrete examples.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q8: Write examples for interp-LL.
;; Recall that these examples must type check!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q9: Write interp-LL.
;; Remember that the program is assumed to be well typed.
