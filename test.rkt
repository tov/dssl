#lang s-exp "dssl.rkt"

(define-struct pair [x y])

(define p (pair 3 5))

(define (f x)
  'discard 'these 'symbols
  (let ((y x))
    (+ y (pair-y p))))

(f 3)

(check-expect (f 4) 9)
