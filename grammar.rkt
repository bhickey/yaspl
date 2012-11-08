(define yaspl-grammar
  (grammar
    (rule module (list 'module import export (or data defn) ...))
    (rule program (list 'program import expression))
    (rule import (list 'import id ...))
    (rule export (list 'export id ...))
    (rule data (list 'data id (list id ...) variant ...))
    (rule variant (list id id ...))
    (rule defn (list 'defn id (list id ...) expression))
    (alias expression (or literal application id case))
    (rule literal (or int string))
    (rule application (list expression expression ...))
    (rule case (list 'case expression (list pattern '-> expression) ...))
    (rule pattern (or literal id (list id pattern ...)))))



