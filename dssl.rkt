#lang racket

(provide (except-out (all-from-out lang/htdp-advanced)
                     #%module-begin
                     lambda
                     λ
                     let
                     let*
                     local
                     unless
                     when)
         (rename-out
           ; Based on racket version:
           [dssl:define         define]
           [dssl:define-struct  define-struct]
           ; Based on ASL version:
           [dssl:module-begin   #%module-begin]
           [dssl:lambda         lambda]
           [dssl:lambda         λ]
           [dssl:let            let]
           [dssl:let*           let*]
           [dssl:local          local]
           [dssl:unless         unless]
           [dssl:when           when]))

(require (for-syntax syntax/parse))
(require (for-syntax "classes.rkt"))

(require (except-in lang/htdp-advanced
                    define              ; uses racket version
                    define-datatype     ; not available
                    define-struct       ; uses racket version
                    require             ; not available
                    set!                ; not available
                    ))
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
         (begin expr ...))]))

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

(define-syntax (dssl:local stx)
  (syntax-parse stx
    [(_ (decl:expr ...) expr:expr ...+)
     #'(local (decl ...) (begin expr ...))]))

(define-syntax (dssl:unless stx)
  (syntax-parse stx
    [(_ condition:expr expr:expr ...+)
     #'(unless condition (begin expr ...))]))

(define-syntax (dssl:when stx)
  (syntax-parse stx
    [(_ condition:expr expr:expr ...+)
     #'(when condition (begin expr ...))]))

