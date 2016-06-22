#lang racket

(provide (except-out (all-from-out lang/htdp-advanced)
                     #%module-begin
                     define
                     lambda
                     let
                     let*
                     unless
                     when)
         (rename-out
           [dssl:module-begin   #%module-begin]
           [dssl:define         define]
           [dssl:define-struct  define-struct]
           [dssl:lambda         lambda]
           [dssl:let            let]
           [dssl:let*           let*]
           [dssl:unless         unless]
           [dssl:when           when]))

(require (for-syntax syntax/parse))
(require (for-syntax "classes.rkt"))

(require (except-in lang/htdp-advanced
                    define-struct
                    require))
(require test-engine/racket-tests)

(define-syntax-rule (dssl:module-begin expr ...)
  (#%module-begin expr ... (test)))

(define-syntax (dssl:define stx)
  (syntax-parse stx
    [(_ (name:id param:id ...) expr:expr ...+)
     #'(define (name param ...)
         (begin expr ...))]
    [(_ name:id rhs:expr)
     #'(define name rhs)]))

(define-syntax (dssl:define-struct stx)
  (syntax-parse stx
    [(_ name:id [field:id ...])
     #'(define-struct name [field ...])]))

(define-syntax (dssl:lambda stx)
  (syntax-parse stx
    [(_ (param:id ...) expr:expr ...+)
     #'(lambda (param ...)
         (begin (expr ...)))]))

(define-syntax (dssl:let stx)
  (syntax-parse stx
    [(_ bs:distinct-bindings expr:expr ...+)
     #'(let bs (begin expr ...))]
    [(_ name:id bs:distinct-bindings expr:expr ...+)
     #'(let name bs (begin expr ...))]))

(define-syntax (dssl:let* stx)
  (syntax-parse stx
    [(_ bs:distinct-bindings expr:expr ...+)
     #'(let* bs (begin expr ...))]
    [(_ name:id bs:distinct-bindings expr:expr ...+)
     #'(let* name bs (begin expr ...))]))

(define-syntax (dssl:unless stx)
  (syntax-parse stx
    [(_ condition:expr expr:expr ...+)
     #'(unless condition (begin expr ...))]))

(define-syntax (dssl:when stx)
  (syntax-parse stx
    [(_ condition:expr expr:expr ...+)
     #'(when condition (begin expr ...))]))

