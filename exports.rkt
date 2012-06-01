#lang typed/racket/base

(provide 
  Export
  (struct-out value-export)
  (struct-out data-export))

(define-type (Export binding type)
             (U data-export (value-export binding type)))

(struct: data-export ((name : Any)) #:transparent)
;(struct: type-export ((value : Any)) #:transparent)
;(struct: pattern-export ((value : Any)) #:transparent)
(define-struct: (binding type) value-export
 ((ext-name : Symbol)
  (name : binding)
  (type : type))
 #:transparent)
