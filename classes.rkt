#lang racket

(require syntax/parse)

(provide binding
         distinct-bindings)

(define-syntax-class binding
  #:description "binding pair"
  (pattern (var:id rhs:expr)))

(define-syntax-class distinct-bindings
  #:description "sequence of distinct binding pairs"
  (pattern (b:binding ...)
           #:fail-when (check-duplicate-identifier
                         (syntax->list #'(b.var ...)))
           "duplicate variable name"
           #:with (var ...) #'(b.var ...)
           #:with (rhs ...) #'(b.rhs ...)))
