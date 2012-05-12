#lang racket


(struct code-block (args closed-args seq-instrs return-instr))

(struct seq-instruction (name impl args))

(struct return (val))
(struct call-closure (clos args))
(struct call-function (loc args closed-args))
; Instructions
; primop
; control-primop
; conditional
; return


(struct location (code-location offset))
(struct program (loc cont env heap))

(define (step prog
 (match prog
  ((program (location code offset) cont env heap)
   (match (vector-ref (code-block-instructions (hash-ref heap code)) offset)
    ((primop ret impl args)
     (define-values (val new-heap) (impl (map (curry hash-ref env) args) heap))
     (program (location code (add1 offset)) cont (hash-set env ret val) new-heap))
    ((control-primop impl args)
     (define-values (new-cont new-env) (impl (map (


