#lang sicp

#|
  Упражнение 1.33

  Можно получить еще более общую версию accumulate (упражнение 1.32), если ввести понятие фильтра
  (filter) на комбинируемые термы. То есть комбинировать только те термы, порожденные из значений
  диапазона, которые удовлетворяют указанному условию. Получающаяся абстракция filtered-accumulate
  получает те же аргументы, что и accumulate, плюс дополнительный одноаргументный предикат, который
  определяет фильтр. Запишите filtered-accumulate ввиде процедуры. Покажите, как с помощью
  filtered-accumulate выразить следующее:

  а. сумму квадратов простых чисел в интервале от a до b (в предположении, что процедура prime? уже
  написана);

  б. произведение всех положительных целых чисел меньше n, которые просты по отношению к n (то есть
  всех таких положительных целых чисел i < n, что НОД (i, n) = 1).
|#

(#%require rackunit)

(define (expmod base exp m)
  (define (remainder-of-square expmod-res)
    (define remainder-res (remainder (square expmod-res) m))
    (if (= remainder-res 1)
        0
        remainder-res))

  (define (expmod-rec exp)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder-of-square (expmod-rec (/ exp 2))))
          (else
           (remainder (* base (expmod-rec (- exp 1))) m))))

  (expmod-rec exp))

(define PRIME_TEST_TIMES 15.0)

(define (prime? n)
  (define (miller-rabin-test n)
    (define (try-it a)
      (expmod a n n))
    (try-it (+ 2 (random (- n 2)))))

  (define (prime-rec? n times)
    (cond ((= times 0) true)
          ((= (miller-rabin-test n) 0) (prime-rec? n (- times 1)))
          (else false)))

  (if (even? n)
      (= n 2)
      (prime-rec? n PRIME_TEST_TIMES)))

(define (filtered-accumulate combiner null-value term a next b pred?)
  (define (filtered-accumulate-iter acc a)
    (if (> a b)
        acc
        (filtered-accumulate-iter (if (pred? a)
                                      (combiner acc (term a))
                                      acc)
                                  (next a))))

  (filtered-accumulate-iter null-value a))

(define (prime-sum a b)
  (filtered-accumulate +
                       (if (= a 2) 2 0)
                       identity
                       (if (even? a) (+ a 1) a)
                       double-inc
                       b
                       prime?))

(define (reciprocral-prime-prod n)
  (define (reciprocral-prime? a)
    (= (gcd a n) 1))

  (if (prime? n)
      (filtered-accumulate * 1 identity 2 inc (- n 1) ->true)
      (if (even? n) 
        (filtered-accumulate * 1 identity 3 double-inc (- n 1) reciprocral-prime?)
        (filtered-accumulate * 1 identity 2 inc (- n 1) reciprocral-prime?))))

(define (->true n) #t)

(define (double-inc x) (+ x 2))

(define (inc n) (+ n 1))

(define (square x) (* x x))

(define (identity x) x)

(check-equal? (prime-sum 5 2) 0)
(check-equal? (prime-sum 2 2) 2)
(check-equal? (prime-sum 2 11) 28)
(check-equal? (prime-sum 4 16) 36)

(check-equal? (reciprocral-prime-prod 2) 1)
(check-equal? (reciprocral-prime-prod 5) (* 2 3 4))
(check-equal? (reciprocral-prime-prod 15) (* 2 4 7 8 11 13 14))
(check-equal? (reciprocral-prime-prod 22) (* 3 5 7 9 13 15 17 19 21))

(check-equal? (filtered-accumulate * 1 square 1 inc 3 odd?) 9)
(check-equal? (filtered-accumulate * 1 identity 3 inc 5 odd?) 15)
(check-equal? (filtered-accumulate + 0 identity 1 inc 10 odd?) 25)
