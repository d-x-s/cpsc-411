#lang racket

;; Learning objectives

;; 1. Practice with store passing functional accumulators
;; 2. Practice with mutuable accumulators
;; 3. Git and racket CLI stuff (if time)

;; Last tutorial we learned about type checking and
;; interpreting. We had the following little language:

;; LL ::= numbers | #t | #f | (+ LL ...) | (- LL ...)
;;           | (if LL LL LL) | (and LL ...) | (or LL ...)
;;           | x | (let ([x LL] ...) LL)
;;           | (begin LL LL ...) | (set! x LL)

;; And we have the following typechecker (also an interpreter)

;; LL -> Type
#;
(define (typecheck-LL ll)
  ;; The accumulator, also known as the typing context
  (define ctx (make-hash))

  (define (check-args lls type)
  ;; mapping over the list
  ;; get a bunch of booleans back
  ;; call AND on it

  ;; let-values VS define-values?
  ;; scoping differences 
    (andmap (λ (ll) (let-values ([(type _) (tc ll acc)])
      (equal? llty type))) lls)) 

  (define (tc ll)
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
      ;; I've changed begin to enforce that it must has at least one expression
      [`(begin ,lls ... ,ll)
       (for-each tc lls)
       (tc ll)]
      [`(set! ,x ,ll)
       (if (dict-has-key? ctx x)
           (let ([ty (tc ll)])
             (dict-set! ctx x ty)
             ty)
           (error "Not well scoped, variable not defined: " x))]))

  (tc ll))

;; However, the way I implemented these using a mutable
;; accumulator is actually wrong!

;; Consider, for example, the following example:

;; (begin (let ([x 4]) 3) x)

;; Which is not well scoped but our typechecker thinks it is!

;; In general, a good rule of thumb between functional and mutuable
;; accumulators is based on the structure on what the accumulator
;; is
;; If the accumulator depends on the tree structure, especially if it
;; must follow that tree structure, then a functional accumulator is best.
;; If it is something like a flat list, that simply grows regardless of the
;; tree, then a mutuable accumulator might be acceptable.

;; I've given a skeleton below. As you can see, the inner loop here
;; has two return values
;; This is because of set!; after a set! is performed, the new accumulator has
;; to be passed back to whatever outer expression may have type checked it,
;; so that the further expressions can be type checked with this accumulator

;; Given this, each case must return two values, and each recursive case
;; must receive two values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q1: Fill in the definition of typecheck-LL using a functional accumulator
;;

;; LL -> Type
#;
(define (typecheck-LL ll)
  
  ;; [Listof LL] Type [AssocList Symbol Type] -> Boolean
  ;; checks the list of lls against the given type
  (define (check-args lls type acc)
    (andmap (λ (ll) (equal? (tc ll acc) type)) lls))

  ;; Accumulator: TODO (fill in invariant here)
  ;; the type of the accumulator is an association list
  ;; > association list a list of cons pairs
  ;; > (list (cons 'a 3)) prints as '((a . 3))

  ;; so for the accumulator invariant:
  ;; keeps track of the variables and types at the
  ;; current expression scope

  ;; LL [AssocList Symbol Type] -> (values Type [AssocList Symbol Type])
  ;; this indicates we are returning the TYPE of the expression and the ACCUMULATOR
  (define (tc ll acc)
    (match ll
      [n #:when (number? n) (values 'Number acc)]
      [b #:when (boolean? b) (values 'Boolean acc)]
      [x #:when (symbol? x)
         ;; the accumulator has the variable and its type
         (values (dict-ref acc x (
                              λ ()  (error "Not well scoped, variable not defined: " x)
                  acc)))]   

      ;; how to type check a +/i expression?
      ;; just check that all the lls are numbers   
      ;; ... pattern indicates a list   
      [`(+ ,lls ...)
       (if (check-args lls 'Number)
            (values 'Number acc) ;; need to return the type and the current accumulator
                                 ;; we need to keep track of the accumulator in each recursive call
                                 ;; each recursive call has access to the state
            (error "Addition must take numbers"))]
      [`(- ,lls ...)
       (if (check-args lls 'Number)
            (values 'Number acc)
            (error "Subtraction must take numbers"))]
      [`(if ,llp ,llt ,llf) ;; each must be type checked
       (define-values (llp-type _) (tc llp acc)) ;; note that let-values is an alternative
       (define-values (llt-type _2) (tc llt acc))
       (define-values (llf-type _3) (tc llf acc))
       (cond
         [(and (equal? llp-type 'Boolean)
               (equal? llt-type llf-type))
          (values llt-type acc)]
         [(not (equal? llp-type 'Boolean))
          (error "Predicate to if must be of Boolean type, got: " llp)]
         [else
          (error "Branches to if much be of the same type, got: " llt llf)])]
      
      ;; check all the arguments against the BOOLEAN type
      [`(and ,lls ...)
       (if (check-args lls 'Boolean)
            (values 'Boolean acc)
            (error "AND must take booleans"))]
      [`(or ,lls ...)
       (if (check-args lls 'Boolean)
            (values 'Boolean acc)
            (error "OR must take booleans"))]

      ;; let requires careful thinking; what acc should we return?
      [`(let ([,xs ,lls] ...) ,llb)
      ;; '(let ([x 3] [y 5]) x)
      ;; xs = '(x y)
      ;; lls = '(3 5)
       (define ll-types 
            (map (lambda (ll)
                  (let-values ([(ty _) tc ll acc)]) lls)]) ty)) lls)) 
       (for/fold ([acc acc])
       ))
         (dict-set! ctx x ty))
       (tc llb)]

       ;; USING DR RACKET REPL TO UNDERSTAND CODE
      (for/fold ([new-acc '()]
                 [x '(x y)]
                 [])
      )

      ;; hint: a helper is helpful (hehe)
      [`(begin ,lls ... ,ll)
       ...]
      ;; set! requires careful thinking; what acc should we return?
      [`(set! ,x ,ll)
       ...]))

  ;; hint: you can still use dict-ref and dict-set with lists
  (let-values ([(ty _) (tc ll '())])
    ty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q2: Change the definition of the mutuable accumulator
;; version of typecheck-LL (from above) to be correct.
;; Hint: it only involves changing the let case

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q3: (Extra challenge) Change the definition of the
;; interpreter from the last tutorial to use a funcitonal
;; accumulator, then correct the mutuable accumulator
;; version.


#;
(module+ test
  (require rackunit)
  (check-equal? (typecheck-LL 3) 'Number)
  (check-equal? (typecheck-LL #t) 'Boolean)
  (check-equal? (typecheck-LL '(+ 3 4 5)) 'Number)
  (check-equal? (typecheck-LL '(and #t #f)) 'Boolean)

  (check-equal? (typecheck-LL '(let ([y (let ([x 3]) (begin (set! x #t) x))]) (if y 4 5))) 'Number)
  (check-equal? (typecheck-LL '(let ([y (let ([x 3]) (begin (set! x #t) x))] [x #f]) (let ([x 5]) (if y x 10)))) 'Number)
  (check-equal? (typecheck-LL '(let ([x 3]) (begin (begin (set! x #f)) (if x 3 4)))) 'Number)
  (check-equal? (typecheck-LL '(let ([x 3] [y 5]) (begin (begin (set! x #f)) (set! y #t) (if x y #t)))) 'Boolean)
  ;; all of the following are scoping errors
  ;; 'y
  (check-exn exn:fail? (thunk (typecheck-LL '(let ([y (let ([x 3]) (begin (set! y #t) x))]) (if y 4 5)))))
  ;; 'x
  (check-exn exn:fail? (thunk (typecheck-LL '(let ([y (let ([x 3]) x)]) (if x 4 5)))))
  ;; 'z
  (check-exn exn:fail? (thunk (typecheck-LL '(let ([y (let ([x 3]) (begin (let ([z 4]) 3) z))]) (if #f 4 5)))))
  ;; 'a
  (check-exn exn:fail? (thunk (typecheck-LL '(let ([y (let ([x 3]) (begin (let ([z 4]) 3) (let ([a 3] [b a]) (+ a b))))]) (if #f 4 5)))))
  ;; 'x
  (check-exn exn:fail? (thunk (typecheck-LL '(let ([y (let ([x 3]) (begin (let ([z 4]) 3) (let ([a 3] [b 5]) (+ a b))))]) (begin (set! x #f) (if x 4 5))))))
  ;; 'z
  (check-exn exn:fail? (thunk (typecheck-LL '(let ([y (let ([x 3]) (begin (let ([z 4]) 3) (let ([a 3] [b 5]) (+ a b z))))]) (if #f 4 5)))))

  (check-equal? (typecheck-LL '(begin
                                 (let ([y (let ([x 3]) (begin x))]
                                       [x (and (begin #f) #t)])
                                   (let ([z (let ([y 5]) (+ y y 10))]
                                         [a (- y 1)]) (begin (set! y #t) (if y 4 5))))
                                 (let ([y (let ([x #f]) (and x x))]
                                       [x (or (begin (let ([y #t]) (and y))) #t #f)])
                                   (let ([z (let ([y 5]) (- y y 10))]
                                         [a (or y #f)]) (if x z (begin (set! a 3) a))))))
                'Number)

  (check-equal? (typecheck-LL '(begin
                                 (let ([y (let ([x 3]) (begin x))]
                                       [x (and (begin #f) #t)])
                                   (let ([z (let ([y #f]) (and y y))]
                                         [a (- y 1)]) (begin (set! y #t) (if y 4 5))))
                                 (let ([y (let ([x #f]) (and x x))]
                                       [x (or (begin (let ([y #t]) (and y))) #t #f)])
                                   (let ([z (let ([y 5]) (- y y 10))]
                                         [a (and y #f)]) (if x (begin (set! z x) z) a)))))
                'Boolean)

  ;; should be "y is not defined"
  (check-exn exn:fail? (thunk (typecheck-LL '(begin
                                               (let ([y (let ([x 3]) (begin x))]
                                                     [x (and (begin (set! y #f) y) #t)]) ;; this y
                                                 (let ([z (let ([y 5]) (+ y y 10))]
                                                       [a (- y 1)]) (begin (set! y #t) (if y 4 5))))
                                               (let ([y (let ([x #f]) (and x x))]
                                                     [x (or (begin (let ([y #t]) (and y))) #t #f)])
                                                 (let ([z (let ([y 5]) (- y y 10))]
                                                       [a (- y 1)]) (if x z a)))))))

  ;; should be `and` must take Booleans  
  (check-exn exn:fail? (thunk (typecheck-LL '(begin
                                               (let ([y (let ([x 3]) (begin x))]
                                                     [x (and (begin #f) #t)])
                                                 (let ([z (let ([y 5]) (+ y y 10))]
                                                       [a (- y 1)]) (begin (set! y #t) (if y 4 5))))
                                               (let ([y (let ([x #f]) (and x x))]
                                                     [x (or (begin (let ([y 3]) (and y))) #t #f)]) ;; (and 3)
                                                 (let ([z (let ([y 5]) (- y y 10))]
                                                       [a (- y 1)]) (if x z a)))))))
  ;; should be Addition must take numbers
  (check-exn exn:fail? (thunk (typecheck-LL '(begin
                                               (let ([y (let ([x 3]) (begin x))]
                                                     [x (and (begin #f) #t)])
                                                 (let ([z (let ([y #f]) (and y y))]
                                                       [a (- y 1)]) (begin (set! y #t) (if y 4 5))))
                                               (let ([y (let ([x #f]) (and x x))]
                                                     [x (or (begin (let ([y #t]) (and y))) #t #f)])
                                                 (let ([z (let ([y 5]) (- y y 10))] ;; y is a bool
                                                       [a (+ y 4)]) (if x (begin (set! z x) z) a)))))))

  ;; should be Branches to if must be of the same type
  (check-exn exn:fail? (thunk (typecheck-LL '(begin
                                               (let ([y (let ([x 3]) (begin x))]
                                                     [x (and (begin #f) #t)])
                                                 (let ([z (let ([y 5]) (+ y y 10))]
                                                       [a (- y 1)]) (begin (set! y #t) (if y 4 5))))
                                               (let ([y (let ([x #f]) (and x x))]
                                                     [x (or (begin (let ([y #f]) (and y))) #t #f)])
                                                 (let ([z (let ([y 5]) (- y y 10))] ;; branches don't match up here
                                                       [a (and y #f)]) (if x z y))))))))
