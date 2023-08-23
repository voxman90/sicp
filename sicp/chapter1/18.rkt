#lang sicp

#|
  Упражнение 1.18

  Используя результаты упражнений 1.16 и 1.17, разработайте процедуру mul-iter, которая порождает
  итеративный процесс для умножения двух чисел с помощью сложения, удвоения и деления пополам,
  и затрачивает логарифмическое число шагов.
|#

(#%require rackunit)

(define (double a)
  (+ a a))

(define (max a b)
  (if (< a b)
      b
      a))

(define (min a b)
  (if (> a b)
      b
      a))

(define (dec a)
  (- a 1))

(define (halve a)
  (if (= (remainder a 2) 0)
      (/ a 2)
      a))

(define (even? a)
  (not (= (halve a) a)))

(define (mul a b acc)
  (cond ((= b 0) acc)
        ((= b 1) (mul a
                      (dec b)
                      (+ acc a)))
        (else (if (even? b)
                  (mul (double a)
                       (halve b)
                       acc)
                  (mul a
                       (dec b)
                       (+ acc a))))))

(define (mul-iter a b)
  (mul (max a b) (min a b) 0))

(check-equal? (mul-iter 1 1) 1)
(check-equal? (mul-iter 40 30) (* 40 30))
(check-equal? (mul-iter 5 0) 0)
(check-equal? (mul-iter 5 15) (* 5 15))
