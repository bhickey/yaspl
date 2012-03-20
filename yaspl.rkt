#lang plai

(define-type SEXP
  [num (n number?)]
  [int32 (n number?)]
  [float (n number?)]
  [cast (n symbol?) (expr SEXP?)]
  [str (s string?)]
  [if-then (test SEXP?) (then SEXP?) (else SEXP?)]
  [add (lhs SEXP?) (rhs SEXP?)]
  [sub (lhs SEXP?) (rhs SEXP?)]
  [div (lhs SEXP?) (rhs SEXP?)]
  [mul (lhs SEXP?) (rhs SEXP?)]
  [mod (lhs SEXP?) (rhs SEXP?)]
  [addf (lhs SEXP?) (rhs SEXP?)]
  [subf (lhs SEXP?) (rhs SEXP?)]
  [divf (lhs SEXP?) (rhs SEXP?)]
  [mulf (lhs SEXP?) (rhs SEXP?)]
  [add32 (lhs SEXP?) (rhs SEXP?)]
  [sub32 (lhs SEXP?) (rhs SEXP?)]
  [div32 (lhs SEXP?) (rhs SEXP?)]
  [mul32 (lhs SEXP?) (rhs SEXP?)]
  [mod32 (lhs SEXP?) (rhs SEXP?)])

(define (parse num-context sexp)
  (cond
    [(number? sexp) (get-number num-context sexp)]
    [(string? sexp) (str sexp)]
    [(list? sexp)
     (case (first sexp)
       [(num) (parse 'num (second sexp))]
       [(int32) (parse 'int32 (second sexp))]
       [(float) (parse 'float (second sexp))]
       [(->num) (cast 'num (parse num-context (third sexp)))]
       [(->int32) (cast 'int32 (parse num-context (second sexp)))]
       [(->float) (cast 'float (parse num-context (second sexp)))]
       [(if) (if-then (parse num-context (second sexp))
                     (parse num-context (third sexp))
                     (parse num-context (fourth sexp)))]
       [(+) ((get-add num-context) 
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))]
       [(-) ((get-sub num-context)
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))]
       [(*) ((get-mul num-context)
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))]
       [(/) ((get-div num-context)
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))]
       [(%) ((get-mod num-context) 
                 (parse num-context (second sexp))
                 (parse num-context (third sexp)))])]))

(define (get-add num-context)
  (case num-context
    [(num) add]
    [(float) addf]
    [(int32) add32]))

(define (get-sub num-context)
  (case num-context
    [(num) sub]
    [(float) subf]
    [(int32) sub32]))

(define (get-mul num-context)
  (case num-context
    [(num) mul]
    [(float) mulf]
    [(int32) mul32]))

(define (get-div num-context)
  (case num-context
    [(num) div]
    [(float) divf]
    [(int32) div32]))

(define (get-mod num-context)
  (case num-context
    [(num) mod]
    [(int32) mod32]
    [else (error "Modulus operator not available in this context")]))

(define (to-int32 n)
  (- (modulo (+ (floor n) 2147483648)
                    4294967296)
            2147483648))

(define (get-number num-context n)
  (if (or  (eq? (floor n) n)
           (eq? num-context 'float))
      (case num-context
        [(num) (num n)]
        [(float) (float n)]
        [(int32) 
         (if (or (> n 2147483647)
                 (< n -2147483648))
             (error "Numeric constant is out of bounds")
             (int32 (to-int32 n)))])
      (error "Expected integer type")))

(define (interp sexp)
  (type-case SEXP sexp
    [num (n) n]
    [float (n) n]
    [int32 (n) n]
    [cast (t n) (do-cast t (interp n))]
    [str (s) s]
    [if-then (i t e) (if (interp i) (interp t) (interp e))]
    [add (lhs rhs) (num-op + lhs rhs)]
    [sub (lhs rhs) (num-op - lhs rhs)]
    [mul (lhs rhs) (num-op * lhs rhs)]
    [div (lhs rhs) (num-op / lhs rhs)]
    [mod (lhs rhs) (num-op modulo lhs rhs)]
    [addf (lhs rhs) (float-op + lhs rhs)]
    [subf (lhs rhs) (float-op - lhs rhs)]
    [mulf (lhs rhs) (float-op * lhs rhs)]
    [divf (lhs rhs) (float-op / lhs rhs)]
    [add32 (lhs rhs) (int32-op + lhs rhs)]
    [sub32 (lhs rhs) (int32-op - lhs rhs)]
    [mul32 (lhs rhs) (int32-op * lhs rhs)]
    [div32 (lhs rhs) (int32-op / lhs rhs)]
    [mod32 (lhs rhs) (int32-op modulo lhs rhs)]
    ))

(define (do-cast type val)
  (case type
    [(num) (inexact->exact (floor val))]
    [(int32) (inexact->exact (to-int32 val))]
    [(float) val]))

(define (num-op fn a b)
  (floor
   (fn (floor (interp a))
       (floor (interp b)))))

(define (float-op fn a b)
  (fn (interp a)
      (interp b)))

(define (int32-op fn a b)
  (to-int32
   (fn (to-int32 (interp a))
       (to-int32 (interp b)))))

(define (repl)
  (interp (parse 'num (read))))