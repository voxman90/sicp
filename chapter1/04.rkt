#lang sicp

(#%require rackunit)

#|
  Если b больше нуля, то возвращаем a + b,
  елси b меньше или равно нулю, то возвращаем a - b.
  По сути: a + Math.abs(b) (js)
|#

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define solution a-plus-abs-b)

(check-equal? (solution 10 20) 30)
(check-equal? (solution 10 (- 20)) 30)
(check-equal? (solution 10 0) 10)
