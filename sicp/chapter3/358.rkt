#lang racket

#|
  Упражнение 3.58

  Дайте интерпретацию потоку, порождаемому следующей процедурой:

    (define (expand num den radix)
      (cons-stream
        (quotient (* num radix) den)
        (expand (remainder (* num radix) den) den radix)))

  (Элементарная процедура quotient возвращает целую часть частного двух целых чисел.) Каковы последовательные
  элементы потока, порожденного выражением (expand 1 7 10)? Что дает вычисление (expand 3 8 10)?
|#

#|
  Процедура переводит рациональную дробь, представленную числителем (num) и знаменателем (den) в
  последовательность разрядов числа после запятой в системе счисления по основанию radix.

    (expand 1 7 10) => 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, ...

    (expand 3 8 10) => 3, 7, 5, 0, 0, 0, ...
|#

(#%require rackunit
           racket/stream)

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(define (expand num den radix)
  (stream-cons
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(define exp1 (expand 1 7 10))

(define output1 (open-output-string))

(display-stream exp1 output1 12)

(check-equal? (get-output-string output1) "\n1\n4\n2\n8\n5\n7\n1\n4\n2\n8\n5\n7")

(close-output-port output1)

(define exp2 (expand 3 8 10))

(define output2 (open-output-string))

(display-stream exp2 output2 10)

(check-equal? (get-output-string output2) "\n3\n7\n5\n0\n0\n0\n0\n0\n0\n0")

(close-output-port output2)

(define exp3 (expand 1 128 2))

(define output3 (open-output-string))

(display-stream exp3 output3 10)

(check-equal? (get-output-string output3) "\n0\n0\n0\n0\n0\n0\n1\n0\n0\n0")

(close-output-port output3)
