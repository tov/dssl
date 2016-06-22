#lang racket

(provide ;; from test-engine/racket-tests:
         (rename-out
           [test                run-all-tests])
         ;; from Advanced Student Language:
         (except-out (all-from-out lang/htdp-advanced)
                     case
                     cond
                     lambda
                     λ
                     let
                     let*
                     letrec
                     local
                     match
                     recur
                     shared
                     time
                     unless
                     when)
         ;; Our own definitions:
         (rename-out
           ;; Based on the Racket version:
           [dssl-define         define]
           [dssl-define-struct  define-struct]
           ;; Based on the ASL version:
           [dssl-case           case]
           [dssl-cond           cond]
           [dssl-lambda         lambda]
           [dssl-lambda         λ]
           [dssl-let            let]
           [dssl-let*           let*]
           [dssl-letrec         letrec]
           [dssl-match          match]
           [dssl-recur          recur]
           [dssl-shared         shared]
           [dssl-local          local]
           [dssl-time           time]
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

(define-syntax-parser dssl-case
  #:literals (else)
  [(_ [(choice:expr ...) expr:expr ...+] ...+)
   #'(case [(choice ...) (begin expr ...)] ...)]
  [(_ [(choice:expr ...) expr:expr ...+]
      ...
      [else last:expr ...])
   #'(cond [(choice ...) (begin expr ...)]
           ...
           [else (begin last ...)])])

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

(define-simple-macro (dssl-let* bs:distinct-bindings expr:expr ...+)
  (let* bs (begin expr ...)))

(define-simple-macro (dssl-letrec bs:distinct-bindings expr:expr ...+)
  (letrec bs (begin expr ...)))

(define-simple-macro (dssl-local (decl:expr ...) expr:expr ...+)
  (local (decl ...) (begin expr ...)))

(define-syntax-parser dssl-match
  #:literals (else)
  [(_ [pattern:expr expr:expr ...+] ...+)
   #'(match [pattern (begin expr ...)] ...)]
  [(_ [pattern:expr expr:expr ...+]
      ...
      [else last:expr ...])
   #'(match [pattern (begin expr ...)]
            ...
            [else (begin last ...)])])

(define-simple-macro (dssl-recur name:id bs:distinct-bindings expr:expr ...+)
  (recur name bs (begin expr ...)))

(define-simple-macro (dssl-shared bs:distinct-bindings expr:expr ...+)
  (shared bs (begin expr ...)))

(define-simple-macro (dssl-time expr:expr ...+)
  (time (begin expr ...)))

(define-simple-macro (dssl-unless test:expr expr:expr ...+)
  (unless test (begin expr ...)))

(define-simple-macro (dssl-when test:expr expr:expr ...+)
  (when test (begin expr ...)))
