#lang racket

(require rackunit)

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

; (check-equal? (typecheck-LL '(let ([x #f]) (begin (set! x 3) (+ 2 x)))) 'Number)
; (check-equal? (typecheck-LL '(let ([false #f]) (begin (set! false #t) (if false 3 4)))) 'Number)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q2: What are some examples of LL programs that /do not/
;; type check? Write two more.

; (check-exn exn:fail? (lambda () (typecheck-LL '(let ([y 4]) (begin (set! y #t) (+ y 4))))))
; (check-exn exn:fail? (lambda () (typecheck-LL '(let ([false #f]) (if false (begin (set! false #t) false) (begin (set! false 4) false))))))


;; Not only do we have to type check to ensure functions like
;; + and `or` being applied to the correct types of arguments,
;; but we also have to have to think about correct scoping
;; of variables.

;; Design Question: Why not separate checking that a
;; program is well-scoped from type checking?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q3: What are some examples of LL programs that /are/
;; well scoped? Write two out.

; (check-equal? (typecheck-LL '(let ([x #f] [y 4]) (begin (set! x 3) (+ 2 x y)))) 'Number)
; (check-equal? (typecheck-LL '(let ([x 5] [y 4]) (begin (set! y 3) (+ 2 x y)))) 'Number)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q4: What are some examples of LL programs that /are not/
;; well scoped? Write two out.

; (check-exn exn:fail? (lambda () (typecheck-LL '(begin (set! y #t) (+ y 4)))))
; (check-exn exn:fail? (lambda () (typecheck-LL '(let ([y 4]) (begin (set! x #t) (+ y 4))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q5: Write a template for processing LL.

;; Given all the steps of the design recipe have been followed,
;; We are ready to start writing typecheck-LL!

#;(define (process-LL ll)
    (match ll
      [n #:when (number? n) (... n)]
      [b #:when (boolean? b) (... b)]
      [x #:when (symbol? x) (... x)]
      [`(+ ,lls ...) (... (map process-LL lls))]
      [`(- ,lls ...) (... (map process-LL lls))]
      [`(if ,llp ,llt ,llf) (... (process-LL llp)
                              (process-LL llt)
                              (process-LL llf))]
      [`(and ,lls ...) (... (map process-LL lls))]
      [`(or ,lls ...) (... (map process-LL lls))]
      [`(let ([,xs ,lls] ...) ,llb)
       (... (map process-LL lls) (process-LL llb))]
      [`(begin ,lls ...) (... (map process-LL lls))]
      [`(set! ,x ,ll) (... (process-LL ll))]))

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
;; Accumulator: Keeps track of variables and their types
(define (typecheck-LL ll)
    ;; The accumulator, also known as the typing context
  (define ctx (make-hash))

  (define (check-args lls type)
    (andmap (λ (ll) (equal? (tc ll) type)) lls))

  (define (tc ll)
    ;; LL template goes here
    (match ll
      [n #:when (number? n) 'Number]
      [b #:when (boolean? b) 'Boolean]
      [x #:when (symbol? x)
         (dict-ref ctx x (λ ()  (error "Not well scoped, variable not defined: " x)))]
      [`(+ ,lls ...)
       (if (check-args lls 'Number)
           'Number
           (error "Addition must take numbers"))]
      [`(- ,lls ...)
       (if (check-args lls 'Number)
           'Number
           (error "Subtraction must take numbers"))]
      [`(if ,llp ,llt ,llf)
       (define llp-type (tc llp))
       (define llt-type (tc llt))
       (define llf-type (tc llf))
       (cond
         [(and (equal? llp-type 'Boolean)
               (equal? llt-type llf-type))
          llt-type]
         [(not (equal? llp-type) 'Boolean)
          (error "Predicate to if must be of Boolean type, got: " llp)]
         [else
          (error "Branches to if much be of the same type, got: " llt llf)])]
      [`(and ,lls ...)
       (if (check-args lls 'Boolean)
           'Boolean
           (error "`and` must take Booleans"))]
      [`(or ,lls ...)
       (if (check-args lls 'Boolean)
           'Boolean
           (error "`or` must take Booleans"))]
      [`(let ([,xs ,lls] ...) ,llb)
       (define ll-types (map tc lls))
       (for ([x xs]
             [ty ll-types])
         (dict-set! ctx x ty))
       (tc llb)]
      [`(begin ,lls ...)
       (match-let ([`(,types ... ,type) (map tc lls)])
         type)]
      [`(set! ,x ,ll)
       (if (dict-has-key? ctx x)
           (let ([ty (tc ll)])
             (dict-set! ctx x ty)
             ty)
           (error "Not well scoped, variable not defined: " x))]))

  (tc ll))

;; The above skeleton is given, since it is good design to
;; restrict the mutable variable to inside the function that needs it
;; as an accumulator. Otherwise, functions outside
;; typecheck-LL could update `ctx` and cause issues.

(check-equal? (typecheck-LL '(+ 2 3)) 'Number)
(check-equal? (typecheck-LL '(let ([false #f]) (if false 3 4))) 'Number)
(check-exn exn:fail? (lambda () (typecheck-LL '(+ #t 4))))
;; This fails since the branches of the if expression are not the same type
(check-exn exn:fail? (lambda () (typecheck-LL '(let ([false #f]) (if false #t 4)))))

(check-equal? (typecheck-LL '(let ([x #f]) (begin (set! x 3) (+ 2 x)))) 'Number)
(check-equal? (typecheck-LL '(let ([false #f]) (begin (set! false #t) (if false 3 4)))) 'Number)
(check-exn exn:fail? (lambda () (typecheck-LL '(begin (set! y #t) (+ y 4)))))
(check-exn exn:fail? (lambda () (typecheck-LL '(let ([y 4]) (begin (set! x #t) (+ y 4))))))

(check-equal? (typecheck-LL '(let ([x 3]
                                   [y 7]
                                   [false #f])
                               (begin
                                 (set! false (if false #t #f))
                                 (set! x 4)
                                 (if (begin (+ x 1) false)
                                     (begin (+ x 7) (set! y 6) (+ x y))
                                     (begin (+ y 10) (set! y 9) (+ x y)))
                                 )))
              'Number)



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

;; Value ::= Number | Boolean

;; interp-LL: LL -> Value
;; evaluates LL programs
;; Accumulator: Keeps track of the values of variables

;; With our signature, we can start thinking of concrete examples.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q8: Write examples for interp-LL.
;; Recall that these examples must type check!

;(check-equal? (interp-LL '(+ 2 3 4)) 11)

#;
(check-equal? (interp-LL '(let ([x 3]
                                [y 7]
                                [false #f])
                            (begin
                              (set! false (if false #t #f))
                              (set! x 4)
                              (if (begin (+ x 1) false)
                                  (begin (+ x 7) (set! y 6) (+ x y))
                                  (begin (+ y 10) (set! y 9) (+ x y)))
                              )
                            13)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q9: Write interp-LL.
;; Remember that the program is assumed to be well typed.

(define (interp-LL ll)
    ;; The accumulator, also known as the environment
  (define env (make-hash))

    (define (interp ll)
      (match ll
      [n #:when (number? n) n]
      [b #:when (boolean? b) b]
      [x #:when (symbol? x) (dict-ref env x)]
      [`(+ ,lls ...) (apply + (map interp lls))]
      [`(- ,lls ...) (apply + (map interp lls))]
      [`(if ,llp ,llt ,llf) (if (interp llp)
                              (interp llt)
                              (interp llf))]
      [`(and ,lls ...) (andmap interp lls)]
      [`(or ,lls ...) (ormap interp lls)]
      [`(let ([,xs ,lls] ...) ,llb)
       (for ([x xs]
             [e (map interp lls)])
         (dict-set! env x e ))
       (interp llb)]
      [`(begin ,lls ...)
       (match-let ([`(,vals ... ,val) (map interp lls)])
         val)]
      [`(set! ,x ,ll)
       (define val (interp ll))
       (dict-set! env x val)
       val]))

    (interp ll))

(check-equal? (interp-LL '(+ 2 3 4)) 9)
(check-equal? (interp-LL '(let ([false #f]) (if false 3 4))) 4)
(check-equal? (interp-LL '(let ([x #f]) (begin (set! x 3) (+ 2 x)))) 5)
(check-equal? (interp-LL '(let ([false #f]) (begin (set! false #t) (if false 3 4)))) 3)


(check-equal? (interp-LL '(let ([x 3]
                                [y 7]
                                [false #f])
                            (begin
                              (set! false (if (and false false) #t #f))
                              (set! x 4)
                              (if (begin (+ x 1) (or false false))
                                  (begin (+ x 7) (set! y 6) (+ x y))
                                  (begin (+ y 10) (set! y 9) (+ x y)))
                              )))
              13)
