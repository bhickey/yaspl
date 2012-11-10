#lang typed/racket/base

(provide unique)


(: unique (Symbol -> Symbol))
(define (unique sym)
  (let* ((base (lookup-base sym))
         (newsym (gensym base)))
    (register-base! newsym base)
    newsym))

;; We maintain a mapping from generated symbols to the original
;; symbol so that when new symbols are generated from generated
;; symbols the append numbers do not stack. 

(: base-hash (HashTable Symbol Symbol))
(define base-hash (make-weak-hasheq))

(: lookup-base  (Symbol -> Symbol))
(define (lookup-base sym)
  (hash-ref base-hash sym (lambda () sym)))

(: register-base!  (Symbol Symbol -> Void))
(define (register-base! sym base)
  (hash-set! base-hash sym base))
