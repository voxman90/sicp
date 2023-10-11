#lang racket

#|
  Упражнение 3.76

  Ева Лу Атор недовольна подходом Хьюго из упражнения 3.75. Написанная им программа не модульна, поскольку
  смешивает операции сглаживания и отлова пересечений ноля. Например, тест на пересечение не должен
  изменяться, если Лизе удастся найти другой способ улучшить качество входного сигнала. Помогите Хьюго
  и напишите процедуру smooth, которая берет на входе поток, а на выходе выдает поток, элементы которого
  получены усреднением каждых двух последовательных элементов входного потока. Затем используйте smooth
  как компоненту и реализуйте детектор перехода через ноль в более модульном стиле.
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

(define (smooth input)
  (stream-map (lambda (a b) (/ (+ a b) 2))
              input
              (stream-cdr input)))

(define (make-zero-crossings input smooth)
  (define smoothed-input (smooth input))

  (define (rec input last-input)
    (stream-cons (sign-change-detector (stream-car last-input) (stream-car input))
                 (rec (stream-cdr input) input)))

  (rec (stream-cons 0 smoothed-input) smoothed-input))

(define zero-crossings (make-zero-crossings sense-data smooth))

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

(define s (smooth sense-data))

(check-equal? (stream-ref s 0) 3/2)
(check-equal? (stream-ref s 1) 1.75)
(check-equal? (stream-ref s 2) 1.25)
(check-equal? (stream-ref s 3) 0.75)
