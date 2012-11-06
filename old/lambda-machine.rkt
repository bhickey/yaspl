#lang racket

;A program is:
; a source expression
; a location for the continuation
; an environment
; the heap

(struct program (expr cont env heap))

;The heap is a hash from heap locations to objects
(struct heap-location (v))

(define new-heap-location!
 (let ((counter 0))
  (lambda ()
   (begin0b
    (heap-location counter)
    (set! counter (add1 counter))))))


;The environment is a hash from identifiers to heap locations


;An expression is:
; Application
; Abstraction
; bind-rec
; identifier

(struct app (left right))
(struct abs (name body))
(struct bind-rec (bindings body))
(struct bind-rec-binding (name arg-name body))
(struct identifier (name))

;Values are
; Closures
; Primitives
; Structures

(struct closure (arg-name env body))
(struct prim-val (tag data))
(struct structure (tag data))

;Continuations are structures with specific tags
; 'stop-continuation: no data

(define stop-continuation (structure 'stop-continuation empty))


(define (inital-state expr)
  (define stop-location (new-heap-location!))
  (define heap (hash stop-location stop-continuation))
  (program expr stop-location (hash) heap))

(struct final-value (data))

(define (return value cont-loc heap)
 (define cont (hash-ref heap cont-loc))
 (define cont-tag (structure-tag cont))
 (case cont-tag
  ((stop-continuation) (final-value value))))



(define (step prog)
 (match prog
  ((program expr cont env heap)
   (match expr
    ((app left right)
     (program left 
     ...)
    ((abs name body)
     (return (closure name env body) cont heap))
    ((bind-rec bindings body)
     (error 'not-yet-implemented))
    ((bind-rec bindings body)
     (error 'not-yet-implemented))
    ((identifier name)
     (return (hash-ref env name) cont heap))))))
   
