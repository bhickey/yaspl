#lang racket 

(require 
  "calc-struct.rkt"
  racket-llvm/safe
  (only-in racket-llvm/unsafe LLVMLinkInJIT)
  (only-in racket-llvm/simple llvm:optimize-module llvm-module-description))

(define context (LLVMContextCreate))
(define standard-builder (LLVMCreateBuilderInContext context))
(define module (LLVMModuleCreateWithNameInContext "calc-module" context))
(define int-type (LLVMInt32TypeInContext context))
(define fun-type1 (LLVMFunctionType int-type (list int-type int-type) false))
(define fun-type2 (LLVMFunctionType int-type (list) false))
(define div-fun (LLVMAddFunction module "div" fun-type1))
(define top-fun (LLVMAddFunction module "top" fun-type2))

#|
 | int div(int x, int y) {
 |   if (!y)
 |     return 0;
 |   return x / y;
 | }
 |#

(let ()
  (define x (LLVMGetParam div-fun 0))
  (define y (LLVMGetParam div-fun 1))
  (define entry  (LLVMAppendBasicBlockInContext context div-fun "entry"))
  (define zero-block
    (LLVMAppendBasicBlockInContext context div-fun "zero"))
  (define div-block
    (LLVMAppendBasicBlockInContext context div-fun "div-zero"))
  (define builder (LLVMCreateBuilderInContext context))
  (define zero (LLVMConstInt int-type 0 false))

  (LLVMSetValueName x "x")
  (LLVMSetValueName y "y")
  (LLVMPositionBuilderAtEnd builder entry)
  (let ((testZero (LLVMBuildICmp builder 'LLVMIntEQ y zero "testZero")))
    (LLVMBuildCondBr builder testZero zero-block div-block))
  
  (LLVMPositionBuilderAtEnd builder zero-block)
  (LLVMBuildRet builder zero)
  
  (LLVMPositionBuilderAtEnd builder div-block)
  (LLVMBuildRet builder (LLVMBuildSDiv builder x y "ans"))
  (LLVMDisposeBuilder builder))

(let-values (((err) (LLVMVerifyModule module 'LLVMReturnStatusAction)))
 (when err
   (display err) (exit 1)))

(define (emit-int v) (LLVMConstInt int-type v #t))

(define (emit-binop opt x y)
  (match opt
         ('+ (LLVMBuildAdd standard-builder x y "sum"))
         ('- (LLVMBuildSub standard-builder x y "difference"))
         ('* (LLVMBuildMul standard-builder x y "product"))
         ('/ (LLVMBuildCall standard-builder div-fun (list x y) "quotient"))))

(define (emit expr)
  (match expr
         ((? integer? n) (emit-int n))
         ((Binop op lhs rhs) 
          (let ((lhs-res (emit lhs))
                (rhs-res (emit rhs)))
            (emit-binop op lhs-res rhs-res)))))

(define (compile expr)
  (define entry  (LLVMAppendBasicBlockInContext context top-fun "entry"))
  (LLVMPositionBuilderAtEnd standard-builder entry)
  (LLVMBuildRet standard-builder (emit expr))
  (LLVMDisposeBuilder standard-builder))

(compile (Binop '* (Binop '- 7 5) (Binop '/ (Binop '+ 1 1) 1)))

(let-values (((err) (LLVMVerifyModule module 'LLVMReturnStatusAction)))
 (when err
   (display err) (exit 1)))

(void (llvm:optimize-module module))
(display (llvm-module-description module))
(LLVMLinkInJIT)
(define ee (LLVMCreateExecutionEngineForModule module))

(define output (LLVMRunFunction ee top-fun (list)))
(LLVMGenericValueToInt output #t)

