#lang s-exp "dssl.rkt"

(define-struct pair [x y])

(define p (pair 3 5))

(define (f x)
  'discard 'these 'symbols
  (let ((y x)
        (n 8))
    (+ y (pair-y p))))

(f 3)

(check-expect (f 4) 9)

(define g (λ (x) 'discard x))

(check-expect (g 8) 8)
