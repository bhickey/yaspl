#lang racket

(struct import (name) #:transparent)
(struct export (name) #:transparent)
(struct data (name variants) #:transparent)
(struct variant (name fields) #:transparent)
(struct defn (name expression) #:transparent)
(struct module (name import export data functions) #:transparent)
(struct main (import expression) #:transparent)

(struct int (val) #:transparent)
(struct str (val) #:transparent)
(struct id (val) #:transparent)
(struct lam (arg body) #:transparent)
(struct application (fn argument) #:transparent)
(struct unit () #:transparent)
(struct case (expr clause) #:transparent)
(struct clause (pattern expr) #:transparent)

(struct number-pattern (val) #:transparent)
(struct string-pattern (val) #:transparent)
(struct identifier-pattern (sym) #:transparent)
(struct wildcard-pattern () #:transparent)
(struct constructor-pattern (constructor args) #:transparent)

(struct rt-int (val) #:transparent)
(struct rt-str (val) #:transparent)
(struct rt-unit () #:transparent)
(struct rt-adt (tag fields) #:transparent)
(struct rt-closure (arg body (env #:mutable)) #:transparent)

(define (parse-yaspl x)
  (match x
    ((list 'module moduleName 
           (list 'import import ...)
           (list 'export export ...)
           data-functions ...)
     (define parsed-data-defs (map parse-data-def data-functions))
     (module moduleName (map parse-import import) (map parse-export export) 
       (filter data? parsed-data-defs)
       (filter defn? parsed-data-defs)))
    ((list 'main (list import ...) (list expr ...)) '???)))

(define (parse-import sexpr)
  (if (symbol? sexpr)
      (import sexpr)
      (error 'parse-import "Expected symbol")))

(define (parse-export sexpr)
  (if (symbol? sexpr)
      (export sexpr)
      (error 'parse-export "Expected symbol")))

(define (parse-data-def sexpr)
  (match sexpr
    ((list 'data id variants ...) (data id (map parse-variant variants)))
    ((list 'defn id (list args ...) body)
     (defn id (foldl (lambda (acc arg) (lam arg acc)) (parse-expr body) args)))))

(define (parse-variant sexpr)
  (match sexpr
    ((list id fields ...) (variant id fields))))

(define (parse-expr sexpr)
  (match sexpr
    ((? integer? sexpr) (int sexpr))
    ((? string? sexpr) (str sexpr))
    ((? symbol? sexpr) (id sexpr))
    ((list 'lambda (list (? symbol? args) ...) body)
     (foldl (lambda (acc arg) (lam arg acc)) (parse-expr body) args))
    ((list 'case expr a-clause ...) (case expr (map parse-clause a-clause)))
    ((list fn) (application fn unit))
    ((list fn args ...) 
     (foldr (lambda (acc arg) (application acc arg)) (parse-expr fn) (map parse-expr args)))))

(define (parse-clause sexpr)
  (match sexpr
    ((list pattern '-> expr) (clause (parse-pattern pattern) (parse-expr expr)))))

(define (parse-pattern sexpr)
  (match sexpr
    ((? number? v) (number-pattern v))
    ((? string? v) (string-pattern v))
    ('_ (wildcard-pattern))
    ((? symbol? v) (identifier-pattern v)) 
    ((list constructor args ...) (constructor-pattern constructor (map parse-pattern args)))))

(define (interp env expr)
  (define (rinterp subexpr)
    (interp env subexpr))
  (match expr
    ((int v) (rt-int v))
    ((str v) (rt-str v))
    ((id v) (dict-ref env v))
    ((lam arg bdy) (rt-closure arg bdy env))
    ((application fn arg)
     (match (rinterp fn)
       ((rt-closure arg-name bdy senv) (interp bdy (dict-set senv arg-name (rinterp arg))))))
    ((unit) (rt-unit))
    ((case expr clauses)
     (define val (rinterp expr))
     (ormap (lambda (clause) (interp-clause clause val env)) clauses))))

(define (interp-clause clause val env)
  (define expr (clause-expr clause))
  (match* ((clause-pattern clause) val)
    (((wildcard-pattern) _) (interp expr env))
    (((number-pattern v) (rt-int val))
     (and (equal? v val) (interp expr env)))
    (((string-pattern v) (rt-str val))
     (and (equal? v val) (interp expr env)))
    (((constructor-pattern constructor (list (identifier-pattern ids) ...)) (rt-adt tag vals))
     (and (equal? constructor tag) (interp expr (foldl dict-set env ids vals))))
    (((identifier-pattern id) _) (interp expr (dict-set env id val)))))

;; (with-input-from-file "color.rkt" read)
;; (parse-yaspl (with-input-from-file "color.rkt" read))
;; (interp (hash) (parse-yaspl (with-input-from-file "color.rkt" read)))

(define (interp-module store mod)
  (match mod
    ((module name (list (import import-name) ...) (list (export export-name) ...) data defn)
     (define values (map simple-interp functions))
     (define env-list (append (map (dict-ref store) import-name)
                              (map data-env data)
                              (map defn-env defn values)))
     (define full-env
       (for/fold ((env (hash))) ((new-env env-list))
         (for/fold ((env env)) (((key value) new-env))
           (dict-set env key value))))
     (for ((value values))
       (when (rt-closure? value)
         (set-rt-closure-env! value full-env)))
     (for/fold ((env (hash))) ((export export-name))
       (dict-set env export (dict-ref full-env export-name))))))

(define (defn-env defn value)
  (hash (defn-name defn) value))

(define (data-env a-data)
  (match a-data
    ((data _ (list (variant names fieldss) ...))
     (for/hash ((name names) (fields fieldss))
       (define gensyms (map gensym fields))
       (values name
               (interp (hash) ((foldr (lambda (acc sym) (lam sym acc))
                                      (prim-app (prim (lambda args (rt-adt name args))) gensyms)
                                      gensyms))))))))

(define (interp-program program)
  (match program 
    ((main imports expression) (interp (interp-module imports) expr))))