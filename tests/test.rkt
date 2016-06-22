#lang dssl

(define-struct pair [x y])

(define p (pair 3 5))

(define (f x)
  'discard 'these 'symbols
  (let ((y x)
        (n 8))
    (+ y (pair-y p))))

(check-expect (f 4) 9)

(define g (λ (x) 'discard x))

(check-expect (g 8) 8)

(check-expect
  (cond
    [false 4]
    [else 5])
  5)

(run-all-tests)
