#lang sicp

#|
  Упражнение 1.17

  Алгоритмы возведения в степень из этого раздела основаны на повторяющемся умножении. Подобным
  же образом можно производить умножение с помощью повторяющегося сложения. Следующая процедура
  умножения (в которой предполагается, что наш язык способен только складывать, но не умножать)
  аналогична процедуре expt:

    (define (* a b)
      (if (= b 0)
          0
          (+ a (* a (- b 1)))))

  Этот алгоритм затрачивает количество шагов, линейно пропорциональное b. Предположим теперь,
  что, наряду со сложением, у нас есть операции double, которая удваивает целое число, и halve,
  которая делит (четное) число на 2. Используя их, напишите процедуру умножения fast-mul,
  аналогичную fast-expt, которая затрачивает логарифмическое число шагов.
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

(define (fast-mul-iter a b acc)
  (cond ((= b 0) acc)
        ((= b 1) (fast-mul-iter a (dec b) (+ acc a)))
        (else (if (not (= (halve b) b))
                  (fast-mul-iter (double a)
                                 (halve b)
                                 acc)
                  (fast-mul-iter a
                                 (dec b)
                                 (+ acc a))))))

(define (fast-mul a b)
  (fast-mul-iter (max a b) (min a b) 0))

(check-equal? (fast-mul 1 1) 1)
(check-equal? (fast-mul 40 30) (* 40 30))
(check-equal? (fast-mul 0 5) 0)
(check-equal? (fast-mul 5 0) 0)
(check-equal? (fast-mul 5 15) (* 5 15))
