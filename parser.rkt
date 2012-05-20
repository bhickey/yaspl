#lang racket

(require "source-ast.rkt")
(provide parse-module parse-expr)

(define (parse-declaration sexp)
 (match sexp
  ((list 'data data-name (list args ...) (list variant fields ...) ...)
   (datatype-definition data-name args
     (map variant-definition variant fields)))
  ((list 'define var-name body)
   (variable-definition var-name (parse-expr body)))))

(define (parse-expr sexp)
 (match sexp
  ((list 'lambda (list args ...) body)
   (foldr lambda-expr (parse-expr body) args))
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
   (identifier-expr id))))

(define (parse-pattern sexp)
 (match sexp
  ((list cons args ...)
   (constructor-pattern cons (map parse-pattern args)))
  ('_ (nobind-pattern))
  ((? symbol? id) (identifier-pattern id))))


(define (parse-module sexp)
 (match sexp
 ((list 'module (? symbol? id) decl ...)
   (module empty empty (map parse-declaration decl)))))
