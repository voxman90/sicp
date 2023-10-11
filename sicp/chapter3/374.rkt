#lang racket

#|
  Упражнение 3.74

  Лиза П. Хакер разрабатывает систему для обработки сигналов, приходящих от физических сенсоров. Один
  из важных инструментов, который она хочет построить, — это сигнал, описывающий переходы входного
  сигнала через ноль (zero crossings). Выходной сигнал должен равняться +1, когда сигнал на входе меняется
  с отрицательного на положительный, -1, когда сигнал меняется с положительного на отрицательный, и 0
  в остальных случаях. (Допустим, что знак нулевого входа положителен). Например, типичный входной сигнал
  и связанный с ним сигнал перехода через ноль могут выглядеть так:

    ... 1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4 ...
    ... 0  0   0   0    0    -1   0   0   0    0    1   0  0 ...

  В Лизиной системе сигнал от сенсора представляется как поток sense-data, а zero-crossings представляет
  соответствующий поток пересечений нуля. Для начала Лиза пишет процедуру sign-change-detector, которая
  берет два значения в качестве аргументов и, сравнив их знаки, выдает 0, 1 или -1. Затем она строит
  поток переходов через ноль следующим образом:

    (define (make-zero-crossings input-stream last-value)
      (stream-cons
        (sign-change-detector (stream-car input-stream) last-value)
        (make-zero-crossings (stream-cdr input-stream)
                             (stream-car input-stream))))

    (define zero-crossings (make-zero-crossings sense-data 0))

  Мимо проходит Лизина начальница Ева Лу Атор и замечает, что программа приблизительно равносильна
  следующей, написанной с использованием обобщенной версии stream-map из упражнения 3.50:

    (define zero-crossings
      (stream-map sign-change-detector sense-data <expression>))

  Завершите программу, вставив необходимое <expression>
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (stream-null? stream) (stream-empty? stream))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-ref stream n)
  (if (<= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (stream-cons
        (apply proc (map stream-car streams))
        (apply stream-map
               (cons proc (map stream-cdr streams))))))

(define (sign-change-detector val1 val2)
  (let ((sign1 (sgn val1))
        (sign2 (sgn val2)))
    (cond ((or (= sign1 sign2) (= sign1 0) (= sign2 0)) 0)
          ((> sign2 0) 1)
          (else -1))))

(define sense-data (stream-cons 1
  (stream-cons 2 (stream-cons 1.5 (stream-cons 1 (stream-cons 0.5
  (stream-cons -0.1 (stream-cons -2 (stream-cons -3 (stream-cons -2
  (stream-cons -0.5 (stream-cons 0.2 (stream-cons 3 (stream-cons 4 '()))))))))))))))

(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cons 0 sense-data)))

(check-equal? (stream-ref zero-crossings 0) 0)
(check-equal? (stream-ref zero-crossings 1) 0)
(check-equal? (stream-ref zero-crossings 2) 0)
(check-equal? (stream-ref zero-crossings 3) 0)
(check-equal? (stream-ref zero-crossings 4) 0)
(check-equal? (stream-ref zero-crossings 5) 1)
(check-equal? (stream-ref zero-crossings 6) 0)
(check-equal? (stream-ref zero-crossings 7) 0)
(check-equal? (stream-ref zero-crossings 8) 0)
(check-equal? (stream-ref zero-crossings 9) 0)
(check-equal? (stream-ref zero-crossings 10) -1)
(check-equal? (stream-ref zero-crossings 11) 0)
(check-equal? (stream-ref zero-crossings 12) 0)
