#lang racket

;; TODO: Perform the undead-analysis for the following

(define eg1                     
  '(module              
     ((locals (x.1 y.2)))         
     (begin          

      ;undead-in: '()
       (set! y.2 1) ;undead-out: '(y.2)           

       ;undead-in: '(y.2 x.1)
       (set! x.1 1) ;undead-out: '(y.2)                          

       ;undead-in '(y.2)
       (halt y.2)) ;undead-out '()         
    ))                        

(define ust1 '(
  '(             
     ()         
     (y.2 x.1)
     (y.2)                
    )))

(define eg2
  '(module                     
     ((locals (x.1 y.2)))
     (begin

       ;undead-in: '()
       (set! x.1 1) ; undead-out: ;()

       ;undead-in '(y.2) ; (y.2) 
       (set! y.2 1) ;undead-out: '(y.2) 

       ;undead-in '(y.2)
       (halt y.2) ;undead-out '()
     )))

(define ust2 '(
  '(                  
     ()
     (y.2)
     ())
))

(define eg3
  '(module
     ((locals (x.1 y.2)))
     (begin
       (set! x.1 1)
       (begin
         (set! y.1 1))
       (halt y.1))))

(define ust3 
     '(
       ()
       (
        (y.2))
       ()
      ))

(define eg4
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
       (halt y.2))))

;; skeleton of above
;; undead set tree alwahys follows structure of program
(define ust4
  '(
    ()
    (y.2 x.1)
    (y.2 x.1)     ; 
    (
     (y.2 x.1)    ; (y.2 x.1) ; (y.2 x.1) - y.2 + y.2 + x.1
     (y.2 x.1)    ; (y.2 x.1) ; (y.2 x.1) - x.1 + x.1
     (
      (y.2 x.1)   ; (y.2 x.1) ; (y.2) - y.2 + y.2 + x.1
      (y.2)))     ; (y.2)     ; + y.2
    ()))          ; ()        ; 

;; skeleton of above
;; undead set tree alwahys follows structure of program
(define ust4
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
    ()))


;; ------------------------------------------------------------------------

;; TODO: *Design* the following function that performs undead analysis on a
;; single effect.
;; You do not need to finish the implementation.

(define (undead-analyse-effect effect)

  ;; Effect undead-set? -> (values undead-set-tree? undead-set?)
  ;;
  ;; the output undead-set-tree should only contain the undead-out sets
  ;; for (the current) effect 
  (define (analyse-effect effect undead-out)
    (match effect
      [`(begin ,effects ...)
        (define-values (unst undead-out))
        (for/foldr ([ust '()])                ;; for/fold and foldl traverse the list in order, whereas foldr goes backwards through the list
                   ([new-undead-out undead-out])
                   ([new-effect effects]))        
          
        (define-values (new-ust undead-in)
          (analyse-effect new-effect new-undead-out)
        )

        (values 
         (cons new-ust ???)
        ...)
      ]          
          
      ]
      (cons ??? ust)    
      [pattern (void)]
      [`(set! ,aloc , triv)
        ]))
)

(module+ test
  (require rackunit)
  (check-pred
   undead-set-tree?
   (undead-analyse-effect '(set! y.1 1)))

  (check-pred
   undead-set-tree?
   (undead-analyse-effect '(set! x.1 1)))

  (check-pred
   undead-set-tree?
   (undead-analyse-effect '(begin (set! x.1 1))))

  (check-pred
   undead-set-tree?
   (undead-analyse-effect '(begin (set! y.1 1) (set! x.1 1))))

  (check-equal?
   (undead-analyse-effect '(begin (set! y.1 1) (set! x.1 1) (set! x.1 y.1)))
   (list (first ust1) (second ust1) '())))

;; ------------------------------------------------------------------------

(require (only-in cpsc411/compiler-lib aloc?))

(define (undead-set? x)
  (and (list? x) (andmap aloc? x)))

(define (undead-set-tree? ust)
  (match ust
    [x #:when (undead-set? x)
       ; (halt y.2), (set! x.1 1)
       #t]
    [`(,usts ... ,ust2)
     ;(begin effects ... effect2)
     (and (andmap undead-set-tree? usts) (undead-set-tree? ust2))]))

#;
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
#;
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

;; ------------------------------------------------------------------------

;; TODO: Implement the following optimization pass, which uses the undead set
;; tree to replace definitions of dead variables with '(nop), a no-op instruction.

;; Lang:
;;
;; p      ::= (module info tail)
;; info   ::= ((locals (aloc ...)) (undead-out ust?))
;; tail   ::= (begin effect ... tail) | (halt triv)
;; effect ::= (begin effect ... effect) | (set! aloc_1 (binop aloc_1 triv)) | (nop)
;; triv   ::= integer | aloc?

;; anytime we see a set to a location that is ctually not used, resplace it with a nop
;; assume we whave an ust that is already processed
;; write the pass that takes the ust and sets nops as appropriate 
(require cpsc411/info-lib)

(define (optimize-dead-stores p ust)
  (match p 
    
  )
)

(module+ test
  (require cpsc411/info-lib rackunit)

  (define (merge-eg-ust eg ust)
    (match eg
      [`(module ,info ,p)
       `(module ,(info-set info 'undead-out ust) ,p)]))

  (check-match
   (optimize-dead-stores (merge-eg-ust eg1 ust1))
   `(module ,info (begin (set! y.1 1) (nop) (halt y.1))))

  (check-match
   (optimize-dead-stores (merge-eg-ust eg2 ust2))
   `(module ,info (begin (nop) (set! y.1 1) (halt y.1))))

  (check-match
   (optimize-dead-stores (merge-eg-ust eg3 ust3))
   `(module ,info (begin (nop) (begin (set! y.1 1)) (halt y.1))))

  (check-match
   (optimize-dead-stores (merge-eg-ust eg4 ust4))
   `(module
      ,info
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
        (halt y.2)))))

;; Local Variables:
;; eval: (put 'module 'racket-indent-function 0)
;; End:
