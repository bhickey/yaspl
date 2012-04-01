#lang racket

(require "unique.rkt")

(provide (all-defined-out))


(define (expr? x)
  ((or/c bind? bind-rec? val-app? ty-app?  val-fun?
        ty-fun? val-constant?  val-identifier? case?) x))

(define (type? x)
  ((or/c ty-constant? ty-identifier? val-fun-ty? ty-fun-ty?) x))

(define (kind? x) ((or/c kind-arr? type-kind?) x))

(define-struct/contract data-variant-declaration
 ((name unique?)
  (arg-kind-names (listof unique?))
  (variant-kinds (listof (list/c unique? kind?)))
  (variant-types (listof type?))))

(define-struct/contract data-type-declaration 
 ((name unique?)
  (arg-kinds (listof kind?))
  (variants (listof data-variant-declaration?))))

(define-struct/contract data-type ((arg-kinds (listof kind?))))
(define-struct/contract data-variant
 ((parent unique?)
  (arg-kind-names (listof unique?))
  (variant-kinds (listof (list/c unique? kind?)))
  (variant-types (listof type?))))

(define-struct/contract pattern
  ((variant-name? unique?)
   (type-names (listof unique?))
   (value-names (listof unique?))))

(struct type-kind ())
(define-struct/contract kind-arr ((arg kind?) (result kind?)))

(define-struct/contract ty-constant ((data any/c)))
(define-struct/contract ty-function ((name unique?)))
(define-struct/contract ty-identifier ((name unique?)))
(define-struct/contract val-fun-ty ((arg type?) (result type?)))
(define-struct/contract ty-fun-ty ((name unique?) (arg-kind kind?) (result type?)))


(define-struct/contract binding ((name unique?) (type type?) (expr expr?)))
(define-struct/contract fun-binding
  ((name unique?) (arg-name unique?)
   (arg-type type?) (result-type type?) (body expr?)))

(define-struct/contract bind
 ((binding binding?) (body expr?)))
(define-struct/contract bind-rec
 ((bindings (listof fun-binding?)) (body expr?)))
(define-struct/contract val-app
 ((fun expr?) (val expr?)))
(define-struct/contract ty-app
 ((fun expr?) (val type?)))
(define-struct/contract val-fun
 ((name unique?) (arg-type type?) (body? expr?)))
(define-struct/contract ty-fun
 ((name unique?) (arg-kind kind?) (body? expr?)))
(define-struct/contract val-constant
 ((type type?) (data any/c)))
(define-struct/contract val-identifier
 ((name unique?)))
(define-struct/contract case
 ((expr expr?) (patterns (listof pattern?))))



(define empty-env (hash))
(define add-env hash-set)
(define env-lookup hash-ref)



