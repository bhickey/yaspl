#lang racket

(require "system-f.rkt" "unique.rkt")


(define-struct/contract closure ((name unique?) (env any/c) (body expr?)))
(define-struct/contract value-constant ((data any/c)))
(define value? (or/c closure? value-constant?))

(define-struct/contract partial-data-constructor
  ((name unique?) (fields (listof value?)) (remaining exact-positive-integer?)))
(define-struct/contract data-constructor
  ((name unique?) (fields (listof value?))))

(define (evaluate expr)
 (define (eval expr env)
  (match expr
   ((bind (binding name type expr) body)
    (eval body (add-env env name (eval expr env))))
   ((bind-rec bindings body) (error 'not-implemented))
   ((val-app fun arg)
    (let ((clos (eval fun env))
          (arg (eval arg env)))
      (match clos
       ((closure name env body)
        (eval body (add-env env name arg)))
       ((value-constant (partial-data-constructor name fields k))
        (let ((k (sub1 k)) (fields (cons arg fields)))
         (value-constant 
          (if (zero? k)
             (data-constructor name (reverse fields))
             ((partial-data-constructor name fields k)))))))))
   ((ty-app fun arg) 
    (eval fun env))
   ((val-fun name type body)
    (closure name env body))
   ((ty-fun name type body)
    (eval body env))
   ((val-constant type data) (value-constant data))
   ((val-identifier name) (env-lookup env name))
   ((case expr patterns)
    
    (error 'not-implemented))))
 (eval expr empty-env))

