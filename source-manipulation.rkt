#lang racket

(require "source-ast.rkt")
(require unstable/hash)

(define (get-inital-datatype-map imports)
  (if (empty? imports) (hash)
    (error 'get-initial-datatype-map "Imports not yet supported")))

(define (create-datatype-map existing-datatype-map data-defs)
 (define datatype-map (make-hash))
 (for ((def data-defs))
  (match def
   ((datatype-definition name args variant-defs)
    (define variants
     (for/list ((variant variant-defs))
      (match variant
       ((variant-definition var-name fields)
        (variant var-name #f fields)))))
    (define data (datatype name args variants))
    (hash-set! datatype-map name data)
    (for ((var variants))
      (set-variant-datatype! data)))))

 (define new-datatype-map (hash-union existing-datatype-map datatype-map))

 (for (((name def) datatype-map))
  (match def
   ((datatype name args variants)
    (for ((variant variants))
      (set-variant-fields!
        (map (curry fixup-structure-types new-datatype-map)
             (variant-fields variant)))))))

 new-datatype-map)

(define (fixup-structure-types datatype-map type)
 (define (get-datatype name)
   (hash-ref datatype-map name))

 (define (fixup type)
  (match type
   ((arrow-type arg result)
    (arrow-type (fixup arg) (fixup result)))
   ((forall-type name body)
    (forall-type name
                 (fixup-structure-types
                   (hash-remove datatype-map name)
                   body)))
   ((variable-type name) type)
   ((structure-type structure-name args)
    (structure-type
      (get-datatype structure-name))
      (map fixup args))))

 (fixup type))


