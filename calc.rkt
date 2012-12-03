#lang racket 

(require 
  "calc-struct.rkt"
  racket-llvm/unsafe)

(define context (LLVMContextCreate))
(define standard-builder (LLVMCreateBuilderInContext context))
(define module (LLVMModuleCreateWithNameInContext "calc-module" context))
(define int-type (LLVMInt32TypeInContext context))
(define fun-type1 (LLVMFunctionType int-type (list int-type int-type) false))
(define fun-type2 (LLVMFunctionType int-type (list) false))
(define add-fun (LLVMAddFunction module "add" fun-type1))
(define sub-fun (LLVMAddFunction module "sub" fun-type1))
(define mul-fun (LLVMAddFunction module "mul" fun-type1))
(define div-fun (LLVMAddFunction module "div" fun-type1))
(define top-fun (LLVMAddFunction module "top" fun-type2))

(let ()
  (define x (LLVMGetParam add-fun 0))
  (define y (LLVMGetParam add-fun 1))
  (define builder (LLVMCreateBuilderInContext context))
  (define entry  (LLVMAppendBasicBlockInContext context add-fun "entry"))
  (LLVMSetValueName x "x")
  (LLVMSetValueName y "y")
  (LLVMPositionBuilderAtEnd builder entry)
  (LLVMBuildRet builder (LLVMBuildAdd builder x y "ans"))
  (LLVMDisposeBuilder builder))

(let ()
  (define x (LLVMGetParam sub-fun 0))
  (define y (LLVMGetParam sub-fun 1))
  (define builder (LLVMCreateBuilderInContext context))
  (define entry  (LLVMAppendBasicBlockInContext context sub-fun "entry"))
  (LLVMSetValueName x "x")
  (LLVMSetValueName y "y")
  (LLVMPositionBuilderAtEnd builder entry)
  (LLVMBuildRet builder (LLVMBuildSub builder x y "ans"))
  (LLVMDisposeBuilder builder))

(let ()
  (define x (LLVMGetParam mul-fun 0))
  (define y (LLVMGetParam mul-fun 1))
  (define builder (LLVMCreateBuilderInContext context))
  (define entry  (LLVMAppendBasicBlockInContext context mul-fun "entry"))
  (LLVMSetValueName x "x")
  (LLVMSetValueName y "y")
  (LLVMPositionBuilderAtEnd builder entry)
  (LLVMBuildRet builder (LLVMBuildMul builder x y "ans"))
  (LLVMDisposeBuilder builder))

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

(define (get-opt opt)
  (match opt
         ('+ add-fun)
         ('- sub-fun)
         ('* mul-fun)
         ('/ div-fun)))

(define (emit-int v) (LLVMConstInt int-type v #t))

(define (emit-binop opt arg0 arg1)
  (LLVMBuildCall standard-builder opt (list arg0 arg1) "opt"))

(define (emit expr)
  (match expr
         ((? integer? n) (emit-int n))
         ((Binop op lhs rhs) 
          (let ((lhs-res (emit lhs))
                (rhs-res (emit rhs))
                (prim-opt (get-opt op)))
            (emit-binop prim-opt lhs-res rhs-res)))))

(define (compile expr)
  (define entry  (LLVMAppendBasicBlockInContext context top-fun "entry"))
  (LLVMPositionBuilderAtEnd standard-builder entry)
  (LLVMBuildRet standard-builder (emit expr))
  (LLVMDumpModule module)
  (LLVMDisposeBuilder standard-builder))

(compile (Binop '* (Binop '- 7 5) (Binop '+ 1 1)))

(let-values (((err) (LLVMVerifyModule module 'LLVMReturnStatusAction)))
 (when err
   (display err) (exit 1)))

(LLVMLinkInJIT)
(define ee (LLVMCreateExecutionEngineForModule module))

(define output (LLVMRunFunction ee top-fun (list)))
(LLVMGenericValueToInt output #t)

