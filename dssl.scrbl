#lang scribble/manual
@require[@for-label[dssl]]

@title{DSSL: Data Structures Student Language}
@author{Jesse A. Tov <jesse@"@"eecs.northwestern.edu>}

@defmodulelang[dssl]

@racketgrammar*[
#:literals (define define-struct define-datatype lambda λ cond else if and or require lib planet
            local let let* letrec time begin begin0 set! delay shared recur when case match unless
             ; match
             _ cons list list* struct vector box
            check-expect check-random check-satisfied check-within check-member-of check-range check-error)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case]
[definition (define (name variable ...) expr)
            (define name expr)
            (define-struct name (name ...))]
[expr (begin expr expr ...)
      (begin0 expr expr ...)
      (lambda (variable ...) expr ...)
      (λ (variable ...) expr ...)
      (local [definition ...] expr ...)
      (letrec ([name expr] ...) expr ...)
      (let ([name expr] ...) expr ...)
      (let name ([name expr] ...) expr ...)
      (let* ([name expr] ...) expr ...)
      (set! name expr)
      (code:line (expr expr ...))
      (cond [expr expr ...] ... [expr expr ...])
      (cond [expr expr ...] ... [else expr ...])
      (case expr [(choice choice ...) expr ...] ...
                 [(choice choice ...) expr ...])
      (case expr [(choice choice ...) expr ...] ...
                 [else expr ...])
      (match expr [pattern expr ...] ...)
      (if expr expr expr)
      (when expr expr ...)
      (unless expr expr ...)
      (and expr expr expr ...)
      (or expr expr expr ...)
      (while expr expr ...)
      (until expr expr ...)
      (do-times (var expr) expr ...)
      (do-times (var expr expr) expr ...)
      (time expr ...)
      (delay expr)
      (code:line name)
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]})
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]})
      (code:line @#,elem{@racketvalfont{'}@racket[()]})
      number
      boolean
      string
      character]
[choice (code:line name)
        number]
[pattern _
         name
         number
         true
         false
         string
         character
         @#,elem{@racketvalfont{'}@racket[_quoted]}
         @#,elem{@racketvalfont{`}@racket[_quasiquoted-pattern]}
         (cons pattern pattern)
         (list pattern ...)
         (list* pattern ...)
         (struct id (pattern ...))
         (vector pattern ...)
         (box pattern)]
[quasiquoted-pattern name
                     number
                     string
                     character
                     (quasiquoted-pattern ...)
                     @#,elem{@racketvalfont{'}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketvalfont{`}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketfont[","]@racket[_pattern]}
                     @#,elem{@racketfont[",@"]@racket[_pattern]}]
]

@section[#:tag "dssl-syntax"]{Syntax for DSSL}

In DSSL, @racket[define-struct]'s structures are mutable,
@racket[define] and @racket[lambda] can define functions of zero
arguments, and function calls can invoke functions of zero arguments.

@defform[(define (name variable ...) expression ...)]{

Defines a function named @racket[name]. The @racket[expression]s are the
body of the function. When the function is called, the values of the
arguments are inserted into the body in place of the @racket[variable]s. The
function returns the value of the last expression in the sequence.

The function name’s cannot be the same as that of another function or
variable.}

@defform[(define name expression)]{

Defines a variable called @racket[name] with the the value of
@racket[expression]. The variable’s name cannot be the same as that of
another function or variable, and @racket[name] itself must not appear in
@racket[expression].}

@defform[(lambda (variable ...) expression ...)]{

Creates a function that takes as many arguments as given @racket[variable]s,
and whose body is a sequence of @racket[expression]s. The result of the function is the result of the last @racket[expression].}

@defform[(λ (variable ...) expression)]{

The Greek letter @racket[λ] is a synonym for @racket[lambda].}

@defform/none[(expression expression ...)]{

Calls the function that results from evaluating the first
@racket[expression]. The value of the call is the value of function's body when
every instance of @racket[name]'s variables are replaced by the values of the
corresponding @racket[expression]s.

The function being called must come from either a definition appearing
before th○e function call, or from a @racket[lambda] expression. The
number of argument @racket[expression]s must be the same as the number
of arguments expected by the function.}

@defform[(delay expression)]{

Produces a “promise” to evaluate @racket[expression]. The
@racket[expression] is not evaluated until the promise is forced with
@racket[force]; when the promise is forced, the result is recorded, so
that any further @racket[force] of the promise immediately produces the
remembered value.}

