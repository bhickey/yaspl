#lang typed/racket

(provide for/hash: for*/hash: hash-union)

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

(: hash-union (All (a b) ((HashTable a b) (HashTable a b) *
                                    -> (HashTable a b))))
(define (hash-union base . args)
  (for*/fold: ((base : (HashTable a b) base))
      ((new : (HashTable a b) args)
       (key : a (hash-keys new)))
    (hash-set base key (hash-ref new key))))
