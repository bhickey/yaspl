#lang typed/racket

(require
  "hash.rkt"
  "source-structures.rkt")


(require/typed (planet dyoo/tqueue)
  (#:opaque tqueue tqueue?)
  (new-tqueue (-> tqueue))
  (tqueue-try-get (tqueue -> (Option Symbol)))
  (tqueue-satisfy! (tqueue Symbol -> Void))
  (tqueue-add! (tqueue Symbol (Listof Symbol) -> Void)))

(provide linearize-modules)

(: linearize-modules ((Listof module) -> (Listof module)))
(define (linearize-modules modules)
  (define module-map
    (for/hash: : (HashTable Symbol module)
        ((module modules))
      (values (module-name module) module)))
  (define queue (new-tqueue))
  (for ((module modules))
    (tqueue-add! queue (module-name module) (map import-name (module-imports module))))
   
  (let: loop : (Listof module) ((acc : (Listof module) null))
    (define name (tqueue-try-get queue))
    (if name
       (begin
         (tqueue-satisfy! queue name)
         (loop (cons (hash-ref module-map name) acc)))
       (reverse acc))))
