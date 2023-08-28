#lang sicp

#|
  Упражнение 1.38

  В 1737 году швейцарский математик Леонард Эйлер опубликовал статью De functionibus Continuis,
  которая содержала расширение цепной дроби для e − 2, где e — основание натуральных логарифмов.
  В этой дроби все Nᵢ равны 1 , а Dᵢ последовательно равны 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...
  Напишите программу, использующую Вашу процедуру cont-frac из упражнения 1.37 для вычисления e
  на основании формулы Эйлера.
|#

(#%require rackunit)

(define (dec n) (- n 1))

(define (cont-frac-iter term-n term-d k)
  (define (cont-frac acc i)
    (if (< i 1)
        acc
        (cont-frac (/ (term-n i)
                      (+ (term-d i) acc))
                   (dec i))))

  (cont-frac 0 k))

(define (e k)
  (define (term-d i)
    (let ((j (/ (+ i 1) 3)))
      (if (integer? j)
          (* 2 j)
          1)))

  (+ 2.0 (cont-frac-iter (lambda (_) 1)
                         term-d
                         k)))

(check-equal? (round (* 1000000000000 (e 100))) 2718281828459.0)
(check-equal? (round (* 100000 (e 10)))  271828.0)
