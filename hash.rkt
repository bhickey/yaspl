#lang typed/racket

(provide for/hash: for*/hash: hash-union make-immutable-hash* hash-value-map)

(require (for-syntax syntax/parse
                     typed-racket/base-env/annotate-classes))

(define-syntax (for/hash: stx)
  (syntax-parse stx
    #:literals (:)
    ((_ : return-annotation:expr
        (bind:optionally-annotated-binding ...) body:expr ...)
     #'(for/fold: : return-annotation
                  ((return-hash : return-annotation (ann (make-immutable-hash null) return-annotation)))
                  (bind ...)
        (let-values (((key val) (let () body ...)))
          (hash-set return-hash key val))))))

(define-syntax (for*/hash: stx)
  (syntax-parse stx
    #:literals (:)
    ((_ : return-annotation:expr
        (bind:optionally-annotated-binding ...) body:expr ...)
     #'(for*/fold: : return-annotation
                  ((return-hash : return-annotation (ann (make-immutable-hash null) return-annotation)))
                  (bind ...)
        (let-values (((key val) (let () body ...)))
          (hash-set return-hash key val))))))

(: hash-union (All (a b) ((HashTable a b) * -> (HashTable a b))))
(define (hash-union . args)
  (for*/fold: ((base : (HashTable a b) ((inst make-immutable-hash a b) null)))
      ((new : (HashTable a b) args)
       (key : a (hash-keys new)))
    (hash-set base key (hash-ref new key))))

(: make-immutable-hash* (All (a b) ((Listof a) (Listof b) -> (HashTable a b))))
(define (make-immutable-hash* keys values)
  (make-immutable-hash (map (inst cons a b) keys values) ))

(: hash-value-map (All (a b c) ((b -> c) (HashTable a b) -> (HashTable a c))))
(define (hash-value-map fn hash)
  (make-immutable-hash
    (hash-map hash (lambda: ((k : a) (v : b)) (cons k (fn v))))))
