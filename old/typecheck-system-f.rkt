#lang racket

(require "system-f.rkt" "unique.rkt")

(define (assert-type= left right env)
  (unless (type=? left right env)
    (error 'type-error "~a does not equal ~a" left right)))

(define (resolve-type type env)
  (match type
    ((ty-identifier name)
     (resolve-type (env-lookup env name)))
    (_ type)))

(define (type=? left right env)
  (define (type=? left right lenv renv)
   (match* ((resolve-type left lenv) (resolve-type right renv))
     (((ty-constant left-data) (ty-constant right-data))
      (equal? left-data right-data))
     (((ty-function left-name) (ty-function right-name))
      (equal? left-name right-name))
     (((val-fun-ty left-arg left-res) (val-fun-ty right-arg right-res))
      (and (type=? left-arg right-arg lenv renv)
           (type=? left-res right lenv renv)))
     (((ty-fun-ty left-name left-kind left-result)
       (ty-fun-ty right-name right-kind right-result))
      (define abstract (gen-uniq 'abstract))
      (and (equal? left-kind right-kind)
           (type=? left-result right-result
                   (add-env lenv left-name abstract)
                   (add-env renv right-name abstract))))
     ((_ _) #f)))
  (type=? left right env env))

(define (type-check expr env)
  (match expr
    ((bind (binding name type expr) body)
     (assert-type= type (type-check expr env))
     (type-check body (add-env env name type)))
    ((bind-rec bindings body) (error 'not-implemented))
    ((val-app fun arg) (error 'not-implemented))
    ((ty-app fun arg) (error 'not-implemented))
    ((val-fun name type body) (error 'not-implemented))
    ((ty-fun name type body) (error 'not-implemented))
    ((val-constant type data) type)
    ((val-identifier name) (env-lookup env name))
    ((case expr patterns)
     (let ((expr-type (type-check expr env)))
       (match (resolve-type type env)
        ((abstact-data-ty adt-name params)
         (let ((res-types
                (for/list ((pattern patterns))
                 (match pattern
                  ((pattern variant-name type-names value-names)
                   (match (env-lookup variant-name)
                    ((data-variant parent param-names ((list _ _) ...)
                                   variant-types)
                     (unless (equal? parent adt-name)
                       (error 'type-check "Pattern doesn't match type"))
                     ;(assert-type= (env-lookup parent

       

     (let ((type (resolve-type type env)))
       (match type
        ((ty-constant (data-type
     (
     
     (error 'not-implemented))))

