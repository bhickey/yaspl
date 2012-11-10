#lang racket

(require (planet dyoo/tqueue)
         "parser.rkt"
         "source-structures.rkt")


;; Runtime Structures
;; Values
(struct rt-int (val) #:transparent)
(struct rt-str (val) #:transparent)
(struct rt-unit () #:transparent)
(struct rt-adt (tag fields) #:transparent)
(struct rt-closure (arg body (env #:mutable)) #:transparent)







(define (interp env expr)
  (define (rinterp subexpr)
    (interp env subexpr))
  (match expr
    ((int v) (rt-int v))
    ((str v) (rt-str v))
    ((id v) (dict-ref env v))
    ((lam arg bdy) (rt-closure arg bdy env))
    ((prim-app fn args)
     (apply fn (map (compose (curry dict-ref env) id-val) args)))
    ((app fn arg)
     (match (rinterp fn)
       ((rt-closure arg-name body senv) (interp (dict-set senv arg-name (rinterp arg)) body))))
    ((unit) (rt-unit))
    ((case expr clauses)
     (define val (rinterp expr))
     (ormap (lambda (clause) (interp-clause clause val env)) clauses))))

(define (interp-clause clause val env)
  (define expr (clause-expr clause))
  (match* ((clause-pattern clause) val)
    (((wildcard-pattern) _) (interp env expr))
    (((number-pattern v) (rt-int val))
     (and (equal? v val) (interp env expr)))
    (((string-pattern v) (rt-str val))
     (and (equal? v val) (interp env expr)))
    (((constructor-pattern constructor (list (identifier-pattern ids) ...)) (rt-adt tag vals))
     (and (equal? constructor tag) (interp (foldl (lambda (id val env) (dict-set env id val))
                                                  env ids vals) expr)))
    (((identifier-pattern id) _) (interp (dict-set env id val) expr))))


(define (simple-interp expr)
  (match expr
    ((lam arg body) (rt-closure arg body #f))
    ((int v) (rt-int v))
    ((str v) (rt-str v))))

(define (module-env store mod)
  (match mod
    ((module name imports (list (export export-names) ...) data defns)
     (define values (map (compose simple-interp defn-expr) defns))
     (define env-list (cons (import-env store imports)
                            (append
                              (map data-env data)
                              (map defn-env defns values))))
     (define full-env (env-union env-list))
     (for ((value values))
       (when (rt-closure? value)
         (set-rt-closure-env! value full-env)))
     (for/fold ((env (hash))) ((export export-names))
       (dict-set env export (dict-ref full-env export))))))

(define (import-env store imports)
  (env-union (map (compose (curry dict-ref store) import-name)
                  imports)))

(define (defn-env defn value)
  (hash (defn-name defn) value))

(define (env-union envs)
  (for/fold ((env (hash))) ((new-env envs))
    (for/fold ((env env)) (((key value) new-env))
      (dict-set env key value))))

(define (data-env a-data)
  (match a-data
    ((data _ _ (list (variant names fieldss) ...))
     (for/hash ((name names) (fields fieldss))
       (define gensyms (map gensym fields))
       (values name
               (interp (hash) (foldr lam
                                     (prim-app (lambda args (rt-adt name args)) gensyms)
                                     gensyms)))))))

(define (interp-program module-store prog)
  (match prog
    ((program imports expr) (interp (import-env module-store imports) expr))))

(define (linearize-modules modules)
  (define module-map
    (for/hash ((module modules))
      (values (module-name module) module)))
  (define queue (new-tqueue))
  (for ((module modules))
    (tqueue-add! queue (module-name module) (map import-name (module-imports module))))
  (let loop ((acc null))
    (define name (tqueue-try-get queue))
    (if name
       (begin
         (tqueue-satisfy! queue name)
         (loop (cons (dict-ref module-map name) acc)))
       (reverse acc))))

(define (initialize-module-store modules)
  (define linear-modules (linearize-modules modules))
  (define module-store (make-hash))
  (for ((module linear-modules))
    (dict-set! module-store
               (module-name module)
               (module-env module-store module)))
  module-store)



(define color-module (parse-yaspl (with-input-from-file "color.rkt" read-syntax)))
(define bool-module (parse-yaspl (with-input-from-file "bool.yaspl" read-syntax)))
(define bool-program1 (parse-yaspl (with-input-from-file "bool-prog1.yaspl" read-syntax)))
(define bool-program2 (parse-yaspl (with-input-from-file "bool-prog2.yaspl" read-syntax)))
(define modules (linearize-modules (list color-module bool-module)))
(define module-store (initialize-module-store modules))
(interp-program module-store bool-program1)
(interp-program module-store bool-program2)
;; (with-input-from-file "color.rkt" read)
;; (parse-yaspl (with-input-from-file "color.rkt" read))
