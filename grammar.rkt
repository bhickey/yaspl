#lang racket
(define yaspl-grammar
  (grammar
    (alias expression (or literal application id case))
    (alias literal (or int string))
    (alias pattern (or literal-pattern id-pattern constructor-pattern))

    (rule module (list 'module import export (or data defn) ...))
    (rule program (list 'program import expression))
    (rule import (list 'import id ...))
    (rule export (list 'export id ...))
    (rule data (list 'data id (list id ...) variant ...))
    (rule variant (list id id ...))
    (rule defn (list 'defn id (list id ...) expression))
    (rule application (list expression expression ...))
    (rule case (list 'case expression (list pattern '-> expression) ...))
    (rule literal-pattern literal)
    (rule id-pattern id)
    (rule constructor-pattern (list id pattern ...))))



(define-syntax-class rule
  (pattern ((~literal rule) constructor:id pattern:pat)))

(define-syntax-class alias
  (pattern ((~literal rule) name:id ((~literal or) variant:id))))

(define-syntax-class pat
  (pattern ((~literal quote) sym:id))
  (pattern ((~literal list) pattern:pat ... pattern2:pat (~literal ...)))
  (pattern ((~literal list) pattern:pat ...))
  (pattern ((~literal list) pattern:pat ...))
