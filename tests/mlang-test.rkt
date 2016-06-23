#lang s-exp "../dssl.rkt"

(define i 0)

(while (< i 4)
  (set! i (add1 i))
  (when (= i 5)
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
