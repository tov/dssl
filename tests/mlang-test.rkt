#lang s-exp "../dssl.rkt"

(define f
  (lambda (x)
    (define y 1)
    (display (+ x y))
    (newline)))

(f 9)

(let ([x 3])
  (define (g y)
    (display (+ x y))
    (newline))
  (g x))

(recur loop ()
  (define x 11)
  (display x)
  (newline))

(define i 0)

(while (< i 40)
  (define five 5)
  (set! i (add1 i))
  (when (= i five)
    (continue))
  (when (= i 9)
    (break))
  (display i)
  (newline))

(do-times (i 10)
  (when (= i 5)
    (continue))
  (display i)
  (newline))
