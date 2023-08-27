#lang sicp

#|
  Упражнение 1.31

  а. Процедура sum — всего лишь простейшая из обширного множества подобных абстракций, которые
  можно выразить через процедуры высших порядков. Напишите аналогичную процедуру под названием
  product, которая вычисляет произведение значений функции в точках на указанном интервале.
  Покажите, как с помощью этой процедуры определить factorial. Кроме того, при помощи product
  вычислите приближенное значение π по формуле

    π     2·4·4·6·6·8···
    -  =  --------------
    4     3·3·5·5·7·7···

  б. Если Ваша процедура product порождает рекурсивный процесс, перепишите ее так, чтобы она
  порождала итеративный. Если она порождает итеративный процесс, перепишите ее так, чтобы она
  порождала рекурсивный.
|#

(#%require rackunit)

(define (product-rec func x next end)
  (if (> x end)
      1
      (* (func x) (product-rec func (next x) next end))))

(define (product func x next end)
  (define (product-iter x result)
    (if (> x end)
        result
        (product-iter (next x) (* result (func x)))))
  (product-iter x 1))

(define (factorial n)
  (product identity 1 inc n))

(define (inc n) (+ n 1))

(define (square x) (* x x))

(define (identity x) x)

(check-equal? (product square 1 inc 3) 36)
(check-equal? (product identity 3 inc 5) 60)
(check-equal? (factorial 5) 120)
