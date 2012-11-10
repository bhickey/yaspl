#lang racket

(require
  (prefix-in src: "source-structures.rkt")
  (except-in syntax/parse expr))

(provide parse-yaspl)

(define (parse-yaspl form)
  (syntax-parse form
    ((~or f:module f:program)
     (attribute f.v))))


(define-syntax-class module
  (pattern ((~datum module) name:symbol i:imports e:exports (~or da:data de:defn) ...)
           #:attr v (src:module
                      (attribute name.v)
                      (attribute i.v)
                      (attribute e.v)
                      (attribute da.v)
                      (attribute de.v))))

(define-syntax-class program
  (pattern ((~datum program) i:imports e:expr)
           #:attr v (src:program
                      (attribute i.v)
                      (attribute e.v))))


(define-syntax-class symbol
  (pattern sym:id 
           #:attr v (syntax-e #'sym)))

(define-syntax-class imports
  (pattern ((~datum import) s:symbol ...)
           #:attr v (map src:import (attribute s.v))))

(define-syntax-class exports
  (pattern ((~datum export) s:symbol ...)
           #:attr v (map src:export (attribute s.v))))

(define-syntax-class defn
  (pattern ((~datum defn) name:symbol (args:symbol ...) e:expr)
           #:attr v (src:defn
                      (attribute name.v)
                      (src:lam* (attribute args.v)
                                (attribute e.v)))))

(define-syntax-class data
  (pattern ((~datum data) name:symbol (params:symbol ...) var:variant ...)
           #:attr v (src:data
                      (attribute name.v)
                      (attribute params.v)
                      (attribute var.v))))

(define-syntax-class variant
  (pattern (name:symbol fields:symbol ...)
           #:attr v (src:variant
                      (attribute name.v)
                      (attribute fields.v))))


(define-syntax-class expr
 (pattern i:integer #:attr v (src:int (syntax-e #'i)))
 (pattern s:str #:attr v (src:str (syntax-e #'s)))
 (pattern s:symbol #:attr v (src:id (attribute s.v)))
 (pattern ((~datum lambda) (args:symbol ...) body:expr)
          #:attr v (src:lam*
                     (attribute args.v)
                     (attribute body.v)))
 (pattern ((~datum case) e:expr c:clause ...)
          #:attr v (src:case
                     (attribute e.v)
                     (attribute c.v)))
 (pattern (op:expr args:expr ...)
          #:attr v (src:app*
                     (attribute op.v)
                     (attribute args.v))))

(define-syntax-class clause
 (pattern (pat:pattern* (~datum ->) e:expr)
          #:attr v (src:clause (attribute pat.v) (attribute e.v))))

(define-syntax-class pattern*
 (pattern (~datum _) 
          #:attr v (src:wildcard-pattern))
 (pattern s:symbol
          #:attr v (src:identifier-pattern (attribute s.v)))
 (pattern i:integer
          #:attr v (src:number-pattern (syntax-e #'i)))
 (pattern s:str
          #:attr v (src:string-pattern (syntax-e #'s)))
 (pattern (constructor:symbol args:pattern* ...)
          #:attr v (src:constructor-pattern (attribute constructor.v) (attribute args.v))))


