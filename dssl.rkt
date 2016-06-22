#lang racket

(provide (except-out (all-from-out lang/htdp-advanced)
                     #%module-begin
                     cond
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
           [dssl:cond           cond]
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

(define-syntax dssl:cond
  (syntax-parser
    #:literals (else)
    [(_ [test:expr expr:expr ...+] ...+)
     #'(cond [test (begin expr ...)] ...)]
    [(_ [test:expr expr:expr ...+]
        ...
        [else last:expr ...])
     #'(cond [test (begin expr ...)]
             ...
             [else (begin last ...)])]))

(define-syntax dssl:define
  (syntax-parser
    [(_ (name:id param:id ...) expr:expr ...+)
     #'(define (name param ...)
         (begin expr ...))]
    [(_ name:id rhs:expr)
     #'(define name rhs)]))

(define-syntax dssl:define-struct
  (syntax-parser
    [(_ name:id [field:id ...])
     #'(define-struct name [field ...])]))

(define-syntax dssl:lambda
  (syntax-parser
    [(_ (param:id ...) expr:expr ...+)
     #'(lambda (param ...)
         (begin expr ...))]))

(define-syntax dssl:let
  (syntax-parser
    [(_ bs:distinct-bindings expr:expr ...+)
     #'(let bs (begin expr ...))]
    [(_ name:id bs:distinct-bindings expr:expr ...+)
     #'(let name bs (begin expr ...))]))

(define-syntax dssl:let*
  (syntax-parser
    [(_ bs:distinct-bindings expr:expr ...+)
     #'(let* bs (begin expr ...))]
    [(_ name:id bs:distinct-bindings expr:expr ...+)
     #'(let* name bs (begin expr ...))]))

(define-syntax dssl:local
  (syntax-parser
    [(_ (decl:expr ...) expr:expr ...+)
     #'(local (decl ...) (begin expr ...))]))

(define-syntax dssl:unless
  (syntax-parser
    [(_ test:expr expr:expr ...+)
     #'(unless test (begin expr ...))]))

(define-syntax dssl:when
  (syntax-parser
    [(_ test:expr expr:expr ...+)
     #'(when test (begin expr ...))]))

