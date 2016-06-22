#lang racket

(provide ;; from test-engine/racket-tests:
         (rename-out
           [test                run-all-tests])
         ;; from Advanced Student Language:
         (except-out (all-from-out lang/htdp-advanced)
                     cond
                     lambda
                     λ
                     let
                     let*
                     local
                     unless
                     when)
         ;; Our own definitions:
         (rename-out
           ;; Based on the Racket version:
           [dssl-define         define]
           [dssl-define-struct  define-struct]
           ;; Based on the ASL version:
           [dssl-cond           cond]
           [dssl-lambda         lambda]
           [dssl-lambda         λ]
           [dssl-let            let]
           [dssl-let*           let*]
           [dssl-local          local]
           [dssl-unless         unless]
           [dssl-when           when]))

(require (except-in lang/htdp-advanced
                    define              ; uses racket version
                    define-datatype     ; not available
                    define-struct       ; uses racket version
                    require             ; not available
                    set!                ; not available
                    ))

(require (for-syntax syntax/parse))
(require syntax/parse/define)

(require test-engine/racket-tests)

(require (for-syntax "classes.rkt"))

(define-syntax-parser dssl-cond
  #:literals (else)
  [(_ [test:expr expr:expr ...+] ...+)
   #'(cond [test (begin expr ...)] ...)]
  [(_ [test:expr expr:expr ...+]
      ...
      [else last:expr ...])
   #'(cond [test (begin expr ...)]
           ...
           [else (begin last ...)])])

(define-syntax-parser dssl-define
  [(_ (name:id param:id ...) expr:expr ...+)
   #'(define (name param ...)
       (begin expr ...))]
  [(_ name:id rhs:expr)
   #'(define name rhs)])

(define-simple-macro (dssl-define-struct name:id [field:id ...])
  (define-struct name [field ...]))

(define-simple-macro (dssl-lambda (param:id ...) expr:expr ...+)
  (lambda (param ...) (begin expr ...)))

(define-syntax-parser dssl-let
  [(_ bs:distinct-bindings expr:expr ...+)
   #'(let bs (begin expr ...))]
  [(_ name:id bs:distinct-bindings expr:expr ...+)
   #'(let name bs (begin expr ...))])

(define-syntax-parser dssl-let*
  [(_ bs:distinct-bindings expr:expr ...+)
   #'(let* bs (begin expr ...))]
  [(_ name:id bs:distinct-bindings expr:expr ...+)
   #'(let* name bs (begin expr ...))])

(define-simple-macro (dssl-local (decl:expr ...) expr:expr ...+)
  #'(local (decl ...) (begin expr ...)))

(define-simple-macro (dssl-unless test:expr expr:expr ...+)
  (unless test (begin expr ...)))

(define-simple-macro (dssl-when test:expr expr:expr ...+)
  (when test (begin expr ...)))
