#lang racket


(require "lifted-anf-ast.rkt")
(require (prefix-in inter: "intermediate-ast.rkt"))
(provide convert)

(define (convert inter-lifted-module)
  (define (convert-expr expr)
   (match expr
    ((inter:create-closure def args)
     (define environment (fresh-name 'environment))
     (pack-expr #f #f
       (bind-expr environment (tuple-expr (map identifier-expr args))
         (tuple-expr (list (identifier-expr def) (identifier-expr environment))))))
    ((inter:case-expr value clauses)
     (case-expr (convert-expr value) (map convert-case-clause clauses)))
    ((inter:identifier-expr name) (identifier-expr name))
    ((inter:constructor-expr variant args)
     (constructor-expr variant (map convert-expr args)))
    ((inter:app-expr fun arg)
     (convert-app-expr #f #f fun arg))
    ((inter:bind (inter:binding name (inter:app-expr fun arg)) body)
     (convert-app-expr name body fun arg))
    ((inter:bind (inter:binding name bound) body)
     (bind-expr name (convert-expr bound) (convert-expr body)))))

  (define (convert-app-expr binding-name binding-body fun arg)
   (define closure-pair (fresh-name 'closure-pair))
   (define function (fresh-name 'function))
   (define environment (fresh-name 'environment))
   (unpack-expr #f closure-pair (convert-expr fun)
    (bind-expr function (tuple-proj-expr 0 (identifier-expr closure-pair))
     (bind-expr environment (tuple-proj-expr 1 (identifier-expr closure-pair))
      (let ((app (app-expr (identifier-expr function) #f
                    (list (identifier-expr environment) (convert-expr arg)))))
        (if binding-name
          (bind-expr binding-name app (convert-expr binding-body))
          app))))))


  (define (convert-type type)
    (cond
      ((false? type) #f)
      (else (error 'nyi))))
  (define (convert-case-clause clause)
   (match clause
    ((inter:case-clause pattern body)
     (case-clause (convert-pattern pattern) (convert-expr body)))))
  (define (convert-pattern pattern)
   (match pattern
    ((inter:nobind-pattern) (nobind-pattern))
    ((inter:identifier-pattern name) (identifier-pattern name))
    ((inter:constructor-pattern variant fields)
     (constructor-pattern variant (map convert-pattern fields))))) 
  (define (convert-data-def def)
    (match def
     ((inter:datatype-definition name args variants)
      (datatype-definition name args (map convert-variant-def variants)))))
  (define (convert-variant-def def)
    (match def
     ((inter:variant-definition name fields)
      (variant-definition name (map identifier-type fields)))))

  (define (extract-environment environment args expr)
    (for/fold ((expr expr)) ((index (in-naturals)) (arg args))
     (bind-expr arg (tuple-proj-expr index (identifier-expr environment))
                expr)))



  (match inter-lifted-module
   ((inter:lifted-module imports exports data-defs top-defs functions)
    (program
      (map convert-data-def data-defs)
      (for/hash (((name expr) top-defs))
       (values name (convert-expr expr)))
      (for/hash (((name closure) functions))
       (match closure
        ((inter:closure-def arg-name type args body)
         (define environment (fresh-name 'environment))
         (values name
           (function 
             (list
               (argument environment #f)
               (argument arg-name (convert-type type)))
                   
             #f
             (extract-environment environment args
                                  (convert-expr body)))))))))))

