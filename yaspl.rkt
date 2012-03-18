#lang plai

(define-type SEXP
  [num (n number?)]
  [str (s string?)]
  [if-then (test SEXP?) (then SEXP?) (else SEXP?)]
  [add (lhs SEXP?) (rhs SEXP?)]
  [sub (lhs SEXP?) (rhs SEXP?)]
  [div (lhs SEXP?) (rhs SEXP?)]
  [mul (lhs SEXP?) (rhs SEXP?)]
  [mod (lhs SEXP?) (rhs SEXP?)])

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(string? sexp) (str sexp)]
    [(list? sexp)
     (case (first sexp)
       [(if) (if-then (parse (second sexp))
                     (parse (third sexp))
                     (parse (fourth sexp)))]
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))]
       [(*) (mul (parse (second sexp))
                 (parse (third sexp)))]
       [(/) (div (parse (second sexp))
                 (parse (third sexp)))]
       [(%) (mod (parse (second sexp))
                 (parse (third sexp)))])]))

(define (interp sexp)
  (type-case SEXP sexp
    [num (n) n]
    [str (s) s]
    [if-then (i t e) (if (interp i) (interp t) (interp e))]
    [add (lhs rhs) (+ (interp lhs) (interp rhs))]
    [sub (lhs rhs) (- (interp lhs) (interp rhs))]
    [mul (lhs rhs) (* (interp lhs) (interp rhs))]
    [div (lhs rhs) (/ (interp lhs) (interp rhs))]
    [mod (lhs rhs) (modulo (interp lhs) (interp rhs))]))