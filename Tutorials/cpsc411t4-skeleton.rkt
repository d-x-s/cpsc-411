#lang racket

(require cpsc411/graph-lib)

;; https://www.students.cs.ubc.ca/~cs-411/2022w2/cpsc411/Functional_Graphs.html

;; Learning Objectives
;; 1) Practice using the CPSC graph lib
;; 2) Practice working with graphs

;; DataLang (DL)
;; DL ::= x | numbers | (ifz DL DL DL) | (let ([x DL] ...) DL)

;; ifz is short for ifzero, that is (ifz 0 DL1 DL2) evaluates to DL1
;; otherwise (ifz n DL1 DL2) evaluates to DL2 (when n =/= 0)

;; In this tutorial, we will be generating a data flow graph of a DL
;; program, so that we can remove dead bindings. By "dead"
;; bindings, we mean "bindings that are never used," that is,
;; variables that are introduced by a `let` never referenced.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Q1: Which bindings are dead in the following DL program?
;; (let ([x 3] [y 5]) (let ([z x]) (ifz z 6 2)))

;; So, what is a data flow graph?
;; For our purposes, we will have a very simple idea of a data flow graph
;; We will simply include a *directed* edge from each variable to whatever
;; node in the program expression it appears in (i.e., a let, an if, or
;; the right hand side of another binding)

;; Feel free to look at the (module+ test ...) at the end of this file for examples

;; So, we would like to write a function generate-dfg: DL -> DFG
;; One way to build up this functional graph is to carry it along as
;; an accumulator
;; We will use this method
;; We also need to keep track of what context a variable finds itself in
;; This is because, according to the template, we process variables on their own
;; We need to "remember" what the outer expression is
;; To do this, we carry along the node that each expression finds itself in

;; generate-dfg: DL -> DFG
;; for simplicity, assume each variable is unique

;; basically want to remove nodes that have no edges 
(define (generate-dfg dl)

  (define (g-dfg dl dfg node)
    (match dl
      [x #:when (symbol? x)
        (add-directed-edge dfg x node) ;; the node is the current node
      ]
      
      [n #:when (number? n)
        dfg
      ]
      
      ;; for each recursive call tracking WHERE in the IF you came from
      [`(ifz ,dlp, dlt, dlf)
        ;; thread the accumulator along if you want to remember things that happened in recursive calls
        (define dfg1 (g-dfg dlp dfg 'if))  ;; this is a DFG, you are finding yourself in the first position
        (define dfg2 (g-dfg dlt dfg1 'if0)) ;; this is another DFG, you are finding yourself in the second position
        (g-dfg dlf dfg2 'if1) ;; this is a third DFG, yoy are finding yourself in the third position
      ]
      
      [`(let ([,xs ,dls] ...) ,dlb)
        ;; add variables as vertices to graph
        (define new-dfg ;; add the vertices to the graph
          (for/fold ([new-dfg dfg])
                    ([x xs])
                    (add-vertex new-dfg x)))
        ;; accumulate graphs from dls in context
        (define dls-dfg
          (for/fold ([dls-dfg new-dfg]) ;; connecting any of the dl expressions to the fact that it is on the right hand side of a binding
                    ([x xs]
                     [d dls])
                    (g-dfg dls-dfg x)))

        (g-dfg dlb dls-dfg 'let)
      ]
    )
  )

  (g-dfg dl '() 'start))

;; Now that we've generated the DFG, let's use it to remove dead bindings!
;; In most cases it's quite simple: if in the DFG the variable has no edges
;; (i.e. no neighbors), then, we can remove it!

;; > (generate-dfg '(let ([x 3]) 4))
;; '((x ()))

;; So (remove-dead-bindings '(let ([x 3]) 4)) will simply give us 4

;; However, what's trickier is if a binding of one variable refers to another
;; variable. We need to check that the other variable is also dead, that is,
;; is not referenced in any expression.

;; > (generate-dfg '(let ([x 3]) (let ([y x]) y)))
;; '((y (let)) (x (y)))
;; We should not remove any bindings

;; > (generate-dfg '(let ([x 3]) (let ([y x]) 5)))
;; '((y ()) (x (y)))
;; 1) x is bound to y
;; 2) but y is not bound to anything (it is dead)
;; 3) hence we also remove 3
;; We should remove all the bindings to get 5

;; remove-dead-bindings: DL DFG -> DL
;; remove the dead bindings, accxording to the DFG
(define (remove-dead-bindings dl dfg)

  (define (rdb dl dfg)
    (match dl
      [x #:when (symbol? x) x]
      [n #:when (number? n) n]
      [`(ifz ,dlp ,dlt ,dlf)
       `(ifz ,(rdb dlp dfg)
             ,(rdb dlt dfg)
             ,(rdb dlf dfg))]
      [`(let ([,xs ,dls] ...) ,dlb)
       (define new-bindings
         (get-new-bindings xs dls dfg))
       (if (empty? new-bindings)
           (rdb dlb dfg)
           `(let ,new-bindings ,(rdb dlb dfg)))]))

  ;; [Listof Variable] [Listof DL] DFG -> [Listof `[,Variable ,DL]]
  (define (get-new-bindings xs dls dfg)
    (void)
    ;; TODO
    ;; Traverse DFG to find dead bindings
    ;; Have to determine if variables mapped to other variables are also dead

    ;; determines if something is an expression node
    (define (exp-node? n)
      (member n '(let if if0 if1)))

    (define (all-neighbors-dead? x dfg) 
      (let* ([ns (get-neighbors dfg x)] ;; what does let* do?
             [other-vars (filter (lambda (n) (not (exp-node? n))) ns)]
             [exp-nodes (filter exp-node? ns)]
        (empty? ns))) ;; when a variable is not referred to anyway, then the it's neighbors are empty
                      ;; like there are no edges
                      ;; so no edges between it and other vertices
      )

    ;; generate a list of variable to DL bindings
    (for/fold ([nbs '()]) ;;accumulate the new bindings, starting off as an empty list
              ([x xs]
               [dl dls])
              (if (all-neighbors-dead? x dfg)
                   nbs
                   (cons `[,x ,(rdb dl dfg)] nbs) ;; square bracket is the same thing as (), but we just want to emphasize these are bindings appearing in the let
              )
    )

    ;; determine deadness


    ) )

#;
(module+ test
  (require rackunit)

  (check-equal? (generate-dfg '(let ([x 3]) x)) '((x (let))))
  (check-equal? (generate-dfg '(let ([x 3]) (ifz x 1 2))) '((x (if))))
  (check-equal? (generate-dfg '(let ([x 3]) (ifz 1 x 2))) '((x (if0))))
  (check-equal? (generate-dfg '(let ([x 3]) (ifz 1 2 x))) '((x (if1))))
  
  (check-equal? (generate-dfg '(let ([x 4] [y 5]) (ifz 0 x y))) '((y (if1)) (x (if0))))
  (check-equal? (generate-dfg '(let ([x 4] [y 5]) (ifz x x y))) '((y (if1)) (x (if0 if))))
  (check-equal? (generate-dfg '(let ([x 4] [y 5]) (let ([z y]) (ifz x y z))))
                '((z (if1)) (y (if0 z)) (x (if))))

  (let ([p0 '(let ([x 4]) 4)])
    (check-equal?
     (remove-dead-bindings p0 (generate-dfg p0))
     4))

  (let ([p1 '(let ([x 4] [y 5]) (let ([z x]) (ifz z x y)))])
    (check-equal?
     (remove-dead-bindings p1 (generate-dfg p1))
     '(let ((y 5) (x 4)) (let ((z x)) (ifz z x y)))))

  (let ([p2 '(let ([x 4] [y 5]) (let ([z x]) (ifz 1 2 3)))])
    (check-equal?
     (remove-dead-bindings p2 (generate-dfg p2))
     '(ifz 1 2 3)))
  
  (let ([p3 '(let ([x 4] [y 5]) (let ([z x] [n y]) (ifz 1 2 3)))])
    (check-equal?
     (remove-dead-bindings p3 (generate-dfg p3))
     '(ifz 1 2 3)))

  (let ([p4 '(let ([x 4] [y 5]) (let ([z x] [n (ifz y 3 4)]) (let ([r n]) (ifz r x y))))])
    (check-equal?
     (remove-dead-bindings p4 (generate-dfg p4))
     '(let ((y 5) (x 4)) (let ((n (ifz y 3 4))) (let ((r n)) (ifz r x y))))))
  )
