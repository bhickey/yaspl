#lang racket

(provide (all-defined-out))


(struct datatype (name args variants) #:transparent)
(struct variant (name datatype fields) #:mutable #:transparent)

(struct datatype-definition (name args variants) #:transparent)
(struct variant-definition
        (tag pattern-name constructor-name fields) #:transparent)
