#lang sicp

#|
Программа зациклится, потому что из-за того, что Scheme применяет аппликативный порядок вычисления,
она попытается осуществить вычисление аргументов, а так как один из аргументов ссылается на самого 
себя, то получится цепочка подстановок, которая никогда не завершится:
(new-if (good? guess x) guess (sqrt (improve guess x) x))) =>
(new-if ... (new-if (good? (improve guess x) x) (sqrt (improve (improve guess x) x) x))))
В случае встроенного оператора ветвления if - предикат вычисляется предварительно, а после вычисляется
либо одна, либо другая ветка.
|#


(define (never x)
  (if #t
      x
      (/ x 0)))

(never 1)


#|
  Это не выдаст ошибку 'division by zero'.
|#

#|
(define (custom-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))


(define (ever x)
  (custom-if #t
             x
             (/ x 0)))

(ever 1)
|#

#|
  Это выдаёт ошибку 'division by zero'.
|#

#| 
  Проверим на факториале.
|#

(define (good-fac x)
  (if (<= x 1)
      1
      (* x (good-fac (- x 1)))))

(good-fac 5)

#|
  Работает! Как только доходим до 1, то программа заканчивает вычисление
  и не пытается вычислять else-clause дальше.
|#

#|
(define (bad-fac x)
  (custom-if (<= x 1)
      1
      (* x (bad-fac (- x 1)))))

(bad-fac 5)
|#

#|
  Мы получили бесконечный цикл.
|#
