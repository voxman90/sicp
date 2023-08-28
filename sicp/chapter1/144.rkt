#lang sicp

#|
  Упражнение 1.44

  Идея сглаживания (smoothing a function) играет важную роль в обработке сигналов. Если f — функция,
  а dx — некоторое малое число, то сглаженная версия f есть функция, значение которой в точке x есть
  среднее между f(x − dx) , f(x) и f(x + dx). Напишите процедуру smooth, которая в качестве ввода
  принимает процедуру, вычисляющую f, и возвращает процедуру, вычисляющую сглаженную версию f. Иногда
  бывает удобно проводить повторное сглаживание (то есть сглаживать сглаженную функцию и т.д.),
  получая n-кратно сглаженную функцию (n-fold smoothed function). Покажите, как породить n-кратно
  сглаженную функцию с помощью smooth и repeated из упражнения 1.43.
|#

(#%require rackunit)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (repeated-rec n)
    (if (= n 1)
        f
        (compose f (repeated-rec (- n 1)))))
  
  (repeated-rec n))

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
       (f x)
       (f (+ x dx)))
       3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(check-equal? (round ((smooth square) 3)) 9.0)
(check-equal? (round ((smooth cube) 10)) 1000.0)

(check-equal? (round ((n-fold-smooth square 3) 3)) 9.0)
(check-equal? (round ((n-fold-smooth cube 3) 10)) 1000.0)
