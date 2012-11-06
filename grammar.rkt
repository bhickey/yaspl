#lang yaspl

(module moduleName import export data functions)

(moduleName String)

(import
   [moduleName])

(export [identifiers])

(main import expr)

(expr
 literal
 application
 identifier
 case)

(data
  identifier [data-variant])

(data-variant
   identifier [identifier])

(literal
 Number
 String)

(identifier
 String)

(case
  (expr [(pattern expr)]))

(pattern
 literal
 variable
 constructor-pattern)

(constructor-pattern
 identifier [pattern])