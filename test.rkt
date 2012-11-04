#lang racket

(require racket/runtime-path)
(require racket/pretty)
(require "parser.rkt")
(require "names.rkt")
(require (prefix-in inter: "intermediate-manipulations.rkt"))
(require (prefix-in lifted: "lifted-manipulations.rkt"))
(require (prefix-in lifted: "lifted-anf-ast.rkt"))
(require (prefix-in eval: "evaluator.rkt"))

(define-runtime-path bool-module-path "yaspl/bool.yaspl")
(define-runtime-path bool2-module-path "yaspl/bool2.yaspl")
(define-runtime-path nat-module-path "yaspl/nat.yaspl")

(define signatures (hash))

(define bool-module (parse-module (with-input-from-file bool-module-path read)))
(define bool2-module (parse-module (with-input-from-file bool2-module-path read)))
(define nat-module (parse-module (with-input-from-file nat-module-path read)))

(define inter-bool-module (inter:convert-module bool-module signatures))
(define inter-nat-module (inter:convert-module nat-module signatures))


(define lifted-bool-module (lifted:convert inter-bool-module))
(define lifted-nat-module (lifted:convert inter-nat-module))


(define bool-program (lifted:program (hash 'bool lifted-bool-module)
                                     'bool))
(define nat-program (lifted:program (hash 'nat lifted-nat-module)
                                    'nat))

(define initial-bool-state (eval:initial-state bool-program))
                                       
(define initial-nat-state (eval:initial-state nat-program))

(eval:extract-value (eval:run initial-bool-state))
(eval:extract-value (eval:run initial-nat-state))
