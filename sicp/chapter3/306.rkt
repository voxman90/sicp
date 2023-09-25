#lang sicp

#|
  Упражнение 3.6

  Полезно иметь возможность сбросить генератор случайных чисел, чтобы получить последовательность, которая
  начинается с некоторого числа. Постройте новую процедуру rand, которая вызывается с аргументом. Этот
  аргумент должен быть либо символом generate, либо символом reset. Процедура работает так: (rand 'generate)
  порождает новое случайное число; ((rand 'reset) <новое-значение>) сбрасывает внутреннюю переменную
  состояния в указанное <новое-значение>. Таким образом, сбрасывая значения, можно получать повторяющиеся
  последовательности. Эта возможность очень полезна при тестировании и отладке программ, использующих
  случайные числа.
|#

(#%require rackunit)

(define (make-linear-congruent-gen x0)
  (let ((m (inexact->exact
             (expt 2 (+ 16 (ceiling (log 2 (ceiling (abs x0)))))))))
    (let ((a (random m))
          (c (random m)))
      (lambda (x) (modulo (+ (* a x) c) m)))))

(define (make-rand initial-x)
  (let ((next (make-linear-congruent-gen initial-x)))
    (let ((current-x initial-x))
      (lambda (op)
        (cond ((eq? op 'reset)
               (lambda (initial-value)
                 (set! current-x initial-value)))
              ((eq? op 'generate)
               (set! current-x (next current-x))
               current-x)
              (else current-x))))))

(define rand (make-rand 1000))

(define random-value (rand 'generate))
(define test-value-1 (rand 'generate))
(define test-value-2 (rand 'generate))
(define test-value-3 (rand 'generate))

((rand 'reset) random-value)

(check-equal? test-value-1 (rand 'generate))
(check-equal? test-value-2 (rand 'generate))
(check-equal? test-value-3 (rand 'generate))
