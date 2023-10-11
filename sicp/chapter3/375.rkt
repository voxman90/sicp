#lang racket

#|
  Упражнение 3.75

  К сожалению, Лизин детектор перехода через ноль из упражнения 3.74 оказывается недостаточным, потому
  что зашумленный сигнал от сенсоров приводит к ложным срабатываниям. Инженер-электронщик Дайко Поправич
  предлагает Лизе сгладить сигнал, чтобы отфильтровать шум, прежде, чем отлавливать пересечение нуля.
  Лиза принимает его совет и решает извлечь переходы через ноль из сигнала, полученного взятием среднего
  арифметического каждого значения входных данных с предыдущим значением. Она объясняет задачу своему
  помощнику Хьюго Думу, и тот пытается реализовать идею, поправив Лизин текст следующим образом:

    (define (make-zero-crossings input-stream last-value)
      (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
        (cons-stream (sign-change-detector avpt last-value)
                     (make-zero-crossings (stream-cdr input-stream)
                                          avpt))))

  Этот код неверно реализует замысел Лизы. Найдите ошибку, внесенную Хьюго, и исправьте ее, не меняя
  структуру программы. (Подсказка: придется увеличить число аргументов make-zero-crossings.)
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

#|
  Хьюго считал среднее не от двух последовательных значений потока, а от значения потока и среднего
  значения на предыдущем шаге.
|#

(define (make-zero-crossings input last-value last-average)
  (let ((average (/ (+ (stream-car input) last-value) 2)))
    (stream-cons (sign-change-detector last-average average)
                 (make-zero-crossings (stream-cdr input)
                                      (stream-car input)
                                      average))))

(define zero-crossings
  (make-zero-crossings sense-data (stream-car sense-data) 0))

(check-equal? (stream-ref zero-crossings 0) 0)
(check-equal? (stream-ref zero-crossings 1) 0)
(check-equal? (stream-ref zero-crossings 2) 0)
(check-equal? (stream-ref zero-crossings 3) 0)
(check-equal? (stream-ref zero-crossings 4) 0)
(check-equal? (stream-ref zero-crossings 5) 0)
(check-equal? (stream-ref zero-crossings 6) -1)
(check-equal? (stream-ref zero-crossings 7) 0)
(check-equal? (stream-ref zero-crossings 8) 0)
(check-equal? (stream-ref zero-crossings 9) 0)
(check-equal? (stream-ref zero-crossings 10) 0)
(check-equal? (stream-ref zero-crossings 11) 1)
(check-equal? (stream-ref zero-crossings 12) 0)
