#lang racket

#|
  Упражнение 3.64

  Напишите процедуру stream-limit, которая в качестве аргумента принимает поток и число (погрешность).
  Она должна просматривать поток, пока не найдется два элемента подряд, различающихся меньше, чем на
  погрешность, и возвращать второй из этих элементов. При помощи этой процедуры можно будет вычислять
  квадратные корни с заданной точностью так:

    (define (sqrt x tolerance)
      (stream-limit (sqrt-stream x) tolerance))
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (stream-null? stream) (stream-empty? stream))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
      (stream-map (lambda (guess)
                    (sqrt-improve guess x))
      guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (define (iter curr-stream-state next-stream-state)
    (if (> (abs (- (stream-car curr-stream-state)
                   (stream-car next-stream-state)))
           tolerance)
        (iter next-stream-state (stream-cdr next-stream-state))
        (stream-car next-stream-state)))

  (iter stream (stream-cdr stream)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(check-true (<= (abs (- (sqrt 16 0.0001) 4)) 0.0001))
(check-true (<= (abs (- (sqrt 16 0.0000001) 4)) 0.0000001))
(check-true (<= (abs (- (sqrt 16 0.0000000001) 4)) 0.0000000001))
