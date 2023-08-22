#lang sicp

#|
  Упражнение 1.16

  Напишите процедуру solution, которая развивается в виде итеративного процесса и реализует
  возведение в степень за логарифмическое число шагов, как fast-expt (Указание: используя наблюдение,
  что (bⁿ୵²)² = (b²)ⁿ୵², храните, помимо значения степени n и основания b, дополнительную переменную
  состояния a, и определите переход между состояниями так, чтобы произведение abⁿ от шага к шагу не
  менялось. Вначале значение a берется равным 1, а ответ получается как значение a в момент окончания
  процесса. В общем случае метод определения инварианта (invariant quantity), который не изменяется
  при переходе между шагами, является мощным способом размышления о построении итеративных
  алгоритмов.)
|#

(#%require rackunit)

(define (even? n)
  (= (remainder n 2) 0))

(define (dec n)
  (- n 1))

(define (^ base degree acc)
  (cond ((= degree 0) acc)
        ((= degree 1) (^ base
                         (dec degree)
                         (* acc base)))
        (else (if (even? degree)
                  (^ (* base base)
                     (/ degree 2)
                     acc)
                  (^ base
                     (dec degree)
                     (* acc base))))))

(define (solution base degree)
  (^ base degree 1))

(check-equal? (solution 10 0) 1)
(check-equal? (solution 3 20) (expt 3 20))
(check-equal? (solution 2 10) (expt 2 10))
(check-equal? (solution 0 5) 0)
