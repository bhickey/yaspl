#lang racket

(require racket/runtime-path)
(require racket/pretty)
(require "parser.rkt")
(require (prefix-in inter: "intermediate-manipulations.rkt"))
(require (prefix-in lifted: "lifted-manipulations.rkt"))
(require (prefix-in lifted: "lifted-anf-ast.rkt"))
(require (prefix-in eval: "evaluator.rkt"))

(define-runtime-path bool-module-path "yaspl/bool.yaspl")
(define-runtime-path nat-module-path "yaspl/nat.yaspl")

(define bool-module (parse-module (with-input-from-file bool-module-path read)))
(define nat-module (parse-module (with-input-from-file nat-module-path read)))

(define inter-bool-module (inter:convert-module bool-module))
(define inter-nat-module (inter:convert-module nat-module))


(define lifted-bool-module (lifted:convert inter-bool-module))
(define lifted-nat-module (lifted:convert inter-nat-module))

(define call-main
  (let ()
   (define closure-pair (lifted:fresh-name 'closure-pair))
   (define function (lifted:fresh-name 'function))
   (define environment (lifted:fresh-name 'environment))

   (lifted:unpack-expr #f closure-pair (lifted:identifier-expr 'main)
    (lifted:bind-expr function (lifted:tuple-proj-expr
                                 0 (lifted:identifier-expr closure-pair))
     (lifted:bind-expr environment (lifted:tuple-proj-expr
                                     1 (lifted:identifier-expr closure-pair))
      (lifted:app-expr (lifted:identifier-expr function) #f
                (list (lifted:identifier-expr environment)
                      (lifted:identifier-expr 'main))))))))



(define initial-bool-state 
  (eval:initial-state lifted-bool-module call-main))
                                       
(define initial-nat-state 
  (eval:initial-state lifted-nat-module call-main))

(pretty-display initial-bool-state)
(eval:extract-value (eval:run initial-bool-state))
(eval:extract-value (eval:run initial-nat-state))
