#lang racket

(require "source-ast.rkt")
(require "names.rkt")
(provide parse-module parse-expr)

(define (parse-declaration sexp)
 (match sexp
  ((list 'data data-name (list args ...) (list variants fieldss ...) ...)
   (datatype-definition (source-name data-name) (map source-name args)
     (for/list ((var variants) (fields fieldss))
      (variant-definition var (source-name var) (source-name var) (map source-name fields)))))
  ((list 'export-data data-name)
   (data-export (source-name data-name)))
  ((list 'export var)
   (value-export var (source-name var) #f))
  ((list 'import module-name)
   (module-import module-name))
  ((list 'define var-name body)
   (variable-definition (source-name var-name) (parse-expr body)))))

(define (parse-expr sexp)
 (match sexp
  ((list 'lambda (list args ...) body)
   (foldr lambda-expr (parse-expr body) (map source-name args)))
  ((list 'case expr (list patterns bodies) ...)
   (case-expr (parse-expr expr)
              (map case-clause
                   (map parse-pattern patterns)
                   (map parse-expr bodies))))
  ((list arg args ...)
   (foldr (lambda (new acc) (app-expr acc new))
          (parse-expr arg)
          (map parse-expr args)))
  ((? symbol? id)
   (identifier-expr (source-name id)))))

(define (parse-pattern sexp)
 (match sexp
  ((list cons args ...)
   (constructor-pattern (source-name cons) cons (map parse-pattern args)))
  ('_ (nobind-pattern))
  ((? symbol? id) (identifier-pattern (source-name id)))))


(define (import? decl)
  (module-import? decl))
(define (export? decl)
  (or (data-export? decl) (value-export? decl)))
(define (definition? decl)
  (or (datatype-definition? decl) (variable-definition? decl)))

(define (parse-module sexp)
 (match sexp
 ((list 'module (? symbol? id) decl ...)
  (define declarations (map parse-declaration decl))
  (module id 
          (filter import? declarations)
          (filter export? declarations)
          (filter definition? declarations)))))
