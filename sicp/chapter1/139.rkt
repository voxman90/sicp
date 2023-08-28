#lang sicp

#|
  Упражнение 1.39

  Представление тангенса в виде цепной дроби было опубликовано в 1770 году немецким математиком
  Й.Х. Ламбертом:

                       x
    tan x = --------------------
                        x²
             1 - ---------------
                          x²
                  3 - ----------

                       5 - ...

  где x дан в радианах. Определите процедуру (tan-cf x k), которая вычисляет приближение к тангенсу
  на основе формулы Ламберта. K указывает количество термов, которые требуется вычислить, как в
  упражнении 1.37.
|#

(#%require rackunit)

(define pi 3.141592653589793)

(define (dec n) (- n 1))

(define (cont-frac n d k)
  (define (cont-frac-iter acc i)
    (if (< i 1)
        acc
        (cont-frac-iter (/ (n i) (+ (d i) acc)) (dec i))))

  (cont-frac-iter 0 k))

(define (tan-cf angle term-amount)
  (let ((-x^2 (- (* angle angle))))
    (/ angle
       (+ 1
          (cont-frac (lambda (_) -x^2)
                     (lambda (i) (+ (* 2 i) 1))
                     (dec term-amount))))))

(check-equal? (tan-cf (/ pi 4) 100) 1.0)
(check-equal? (tan-cf 0 10) 0)
(check-equal? (round (* 100 (tan-cf (/ pi 3) 100))) 173.0)
