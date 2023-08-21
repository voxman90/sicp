#lang sicp

#|
  Упражнение 1.11

  Функция f определяется правилом: f(n) = n, если n < 3, и f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3),
  если n ≥ 3. Напишите процедуру f, вычисляющую f с помощью рекурсивного процесса. Напишите процедуру
  f-iter, вычисляющую f с помощью итеративного процесса.
|#

(#%require rackunit)

#|
    f(n) = n, где n < 3
    f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3), где n ≥ 3
|#

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* (f (- n 2)) 2)
         (* (f (- n 3)) 3))))

(define (f-iter n)
  (define (f count n1 n2 n3)
    (cond ((< count 3) count)
          ((= count 3) n1)
          (else (f (- count 1)
                   (+ n1 (* n2 2) (* n3 3))
                   n1
                   n2))))
  (f n 4 2 1))

(check-equal? (f-iter 1) 1)
(check-equal? (f-iter 2) 2)
(check-equal? (f-iter 3) 4)
(check-equal? (f-iter 4) 11)
(check-equal? (f-iter 5) 25)
(check-equal? (f-iter 6) 59)
(check-equal? (f 1) 1)
(check-equal? (f 2) 2)
(check-equal? (f 3) 4)
(check-equal? (f 4) 11)
(check-equal? (f 5) 25)
(check-equal? (f 6) 59)
