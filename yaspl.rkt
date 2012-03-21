#lang plai

(define-type BINDINGS
  [binding (s symbol?) (v SEXP?) (b BINDINGS?)]
  [empty-binding])

(define-type SEXP
  [bind (bindings BINDINGS?) (body SEXP?)]
  [sym (n symbol?)]
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
    [(symbol? sexp) (sym sexp)]
    [(number? sexp) (get-number num-context sexp)]
    [(string? sexp) (str sexp)]
    [(list? sexp)
     (case (first sexp)
       [(let) (bind (parse-bind num-context (second sexp)) 
                (parse num-context (third sexp)))]
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

(define (parse-bind num-context s)
  (if (empty? s)
      (empty-binding)
      (binding (caar s) (parse num-context (cadar s))
               (parse-bind num-context (cdr s)))))

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

(define (interp env sexp)
  (type-case SEXP sexp
    [sym (s) (hash-ref env s)]
    [num (n) n]
    [float (n) n]
    [int32 (n) n]
    [bind (bindings body) (interp (do-binding env bindings) body)]
    [cast (t n) (do-cast t (interp n))]
    [str (s) s]
    [if-then (i t e) (if (interp env i) (interp env t) (interp env e))]
    [add (lhs rhs) (num-op env + lhs rhs)]
    [sub (lhs rhs) (num-op env - lhs rhs)]
    [mul (lhs rhs) (num-op env * lhs rhs)]
    [div (lhs rhs) (num-op env / lhs rhs)]
    [mod (lhs rhs) (num-op env modulo lhs rhs)]
    [addf (lhs rhs) (float-op env + lhs rhs)]
    [subf (lhs rhs) (float-op env - lhs rhs)]
    [mulf (lhs rhs) (float-op env * lhs rhs)]
    [divf (lhs rhs) (float-op env / lhs rhs)]
    [add32 (lhs rhs) (int32-op env + lhs rhs)]
    [sub32 (lhs rhs) (int32-op env - lhs rhs)]
    [mul32 (lhs rhs) (int32-op env * lhs rhs)]
    [div32 (lhs rhs) (int32-op env / lhs rhs)]
    [mod32 (lhs rhs) (int32-op env modulo lhs rhs)]
    ))

(define (do-binding env bindings)
  (type-case BINDINGS bindings
    [empty-binding () env]
    [binding (s v rest)
            (do-binding (hash-set env s (interp env v)) rest)]))

(define (do-cast type val)
  (case type
    [(num) (inexact->exact (floor val))]
    [(int32) (inexact->exact (to-int32 val))]
    [(float) val]))

(define (num-op env fn a b)
  (floor
   (fn (floor (interp env a))
       (floor (interp env b)))))

(define (float-op env fn a b)
  (fn (interp env a)
      (interp env b)))

(define (int32-op env fn a b)
  (to-int32
   (fn (to-int32 (interp env a))
       (to-int32 (interp env b)))))

(define (repl)
  (interp (make-immutable-hash) (parse 'num (read))))