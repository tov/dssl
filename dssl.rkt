#lang racket

(provide ;; from test-engine/racket-tests:
         (rename-out
           [test                run-all-tests])

         ;; from racket/base:
         in-range in-naturals in-vector in-indexed
         printf
         set!
         for for/list for/vector for/and for/or for/sum
         for/product for/lists for/first for/last for/fold
         for* for*/list for*/vector for*/and for*/or for*/sum
         for*/product for*/lists for*/first for*/last for*/fold

         ;; from Advanced Student Language:
         (except-out (all-from-out lang/htdp-advanced)
                     ;; Names we are redefining:
                     case
                     cond
                     lambda
                     λ
                     let
                     let*
                     letrec
                     local
                     make-posn
                     match
                     posn
                     recur
                     shared
                     time
                     unless
                     when
                     ;; Not available:
                     define-datatype
                     ;; hash tables not available:
                     hash-copy hash-count hash-eq? hash-equal?
                     hash-eqv? hash-for-each hash-has-key?
                     hash-map hash-ref hash-ref! hash-remove
                     hash-remove! hash-set hash-set! hash-update
                     hash-update! hash? make-hash make-hasheq
                     make-hasheqv make-immutable-hash
                     make-immutable-hasheq make-immutable-hasheqv)

         ;; Our own definitions:
         (rename-out
           [dssl-break          break]
           [dssl-continue       continue]
           [dssl-until          until]
           [dssl-while          while]
           [dssl-do-times       do-times]
           ;; Based on the Racket version:
           [dssl-define         define]
           [dssl-define-struct  define-struct]
           ;; Based on the ASL version:
           [make-posn           posn]
           [dssl-case           case]
           [dssl-cond           cond]
           [dssl-lambda         lambda]
           [dssl-lambda         λ]
           [dssl-let            let]
           [dssl-let*           let*]
           [dssl-letrec         letrec]
           [dssl-match          match]
           [dssl-local          local]
           [dssl-recur          recur]
           [dssl-shared         shared]
           [dssl-time           time]
           [dssl-unless         unless]
           [dssl-when           when]))

(require (except-in lang/htdp-advanced
                    ;; We want the Racket versions of these:
                    define
                    define-struct
                    require
                    set!))

(require racket/stxparam)
(require (for-syntax syntax/parse))
(require syntax/parse/define)

(require test-engine/racket-tests)

(require (for-syntax "classes.rkt"))

(define-syntax (dssl-case stx)
  (syntax-parse stx
    #:literals (else)
    [(_ scrutinee:expr [(choice:expr ...) expr:expr ...+] ...+)
     #'(case scrutinee [(choice ...) (begin expr ...)] ...)]
    [(_ scrutinee:expr
        [(choice:expr ...) expr:expr ...+]
        ...
        [else last:expr ...])
     #'(case scrutinee
         [(choice ...) (begin expr ...)]
         ...
         [else (begin last ...)])]))

(define-syntax (dssl-cond stx)
  (syntax-parse stx
    #:literals (else)
    [(_ [test:expr expr:expr ...+] ...+)
     #'(cond [test (begin expr ...)] ...)]
    [(_ [test:expr expr:expr ...+]
        ...
        [else last:expr ...])
     #'(cond [test (begin expr ...)]
             ...
             [else (begin last ...)])]))

(define-syntax (dssl-define stx)
  (syntax-parse stx
    [(_ (name:id param:id ...) expr:expr ...+)
     #'(define (name param ...)
         (begin expr ...))]
    [(_ name:id rhs:expr)
     #'(define name rhs)]))

(define-simple-macro (dssl-define-struct name:id [field:id ...])
  (define-struct name [field ...]
                 #:mutable
                 #:transparent))

(define-simple-macro (dssl-lambda (param:id ...) expr:expr ...+)
  (lambda (param ...) (begin expr ...)))

(define-syntax (dssl-let stx)
  (syntax-parse stx
    [(_ bs:distinct-bindings expr:expr ...+)
     #'(let bs (begin expr ...))]
    [(_ name:id bs:distinct-bindings expr:expr ...+)
     #'(let name bs (begin expr ...))]))

(define-simple-macro (dssl-recur name:id bs:distinct-bindings expr:expr ...+)
  (recur name bs (begin expr ...)))

(define-simple-macro (dssl-shared bs:distinct-bindings expr:expr ...+)
  (shared bs (begin expr ...)))

(define-simple-macro (dssl-let* bs:distinct-bindings expr:expr ...+)
  (let* bs (begin expr ...)))

(define-simple-macro (dssl-letrec bs:distinct-bindings expr:expr ...+)
  (letrec bs (begin expr ...)))

(define-simple-macro (dssl-local (decl:expr ...) expr:expr ...+)
  (local (decl ...) (begin expr ...)))

(define-syntax (dssl-match stx)
  (syntax-parse stx
    #:literals (else)
    [(_ scrutinee:expr [pattern:expr expr:expr ...+] ...+)
     #'(match scrutinee [pattern (begin expr ...)] ...)]
    [(_ scrutinee:expr
        [pattern:expr expr:expr ...+]
        ...
        [else last:expr ...])
     #'(match scrutinee
              [pattern (begin expr ...)]
              ...
              [else (begin last ...)])]))

(define-simple-macro (dssl-time expr:expr ...+)
  (time (begin expr ...)))

(define-simple-macro (dssl-unless test:expr expr:expr ...+)
  (unless test (begin expr ...)))

(define-simple-macro (dssl-when test:expr expr:expr ...+)
  (when test (begin expr ...)))

(define-syntax-parameter dssl-break
  (lambda (stx)
    (raise-syntax-error #f "use of break keyword not in a loop" stx)))

(define-syntax-parameter dssl-continue
  (lambda (stx)
    (raise-syntax-error #f "use of continue keyword not in a loop" stx)))

(define-syntax (dssl-while stx)
  (syntax-parse stx
    [(_ test:expr expr:expr ...)
     #`(let/ec break-f
         (dssl-let loop ()
           (define (continue-f) (loop) (break-f))
           (syntax-parameterize
             ([dssl-break (syntax-rules () [(_) (break-f)])]
              [dssl-continue (syntax-rules () [(_) (continue-f)])])
             (dssl-when test
               expr ...
               (loop)))))]))

(define-simple-macro (dssl-until test:expr expr:expr ...)
  (dssl-while (not test) expr ...))

(define-syntax (dssl-do-times stx)
  (syntax-parse stx
    [(_ (var:id times:expr result:expr ...) body:expr ...)
     (define continue (datum->syntax stx 'continue))
     (define break    (datum->syntax stx 'break))
     #`(let/ec #,break
         (let ([limit times])
           (dssl-let loop [(var 0)]
             (define (#,continue) (#,break (loop (add1 var))))
             (if (< var limit)
               (begin
                 body ...
                 (loop (add1 var)))
               (begin
                 (void)
                 result ...)))))]))
