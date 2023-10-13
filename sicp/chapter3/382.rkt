#lang racket

#|
  Упражнение 3.82

  Переделайте на основе потоков упражнение 3.5 на интегрирование методом Монте-Карло. Потоковая версия
  процедуры estimate-integral не требует аргумента, который говорит, сколько проводить испытаний. Вместо
  этого она порождает поток оценок, основанных на все большем количестве испытаний.
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

(define (sicp-random n)
  (if (and (exact? n) (integer? n))
      (random n)
      (* n (random))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (sicp-random range))))

(define (monte-carlo experiment)
  (define (prob-stream trials-passed trials)
    (if (experiment)
        (stream-cons (/ (+ trials-passed 1) trials)
                     (prob-stream (+ trials-passed 1) (+ trials 1)))
        (stream-cons (/ trials-passed trials)
                     (prob-stream trials-passed (+ trials 1)))))

  (prob-stream 0 1))

(define (estimate-integral belongs-to-figure? x1 x2 y1 y2)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (belongs-to-figure? x y)))

  (define (S x1 x2 y1 y2)
    (* (abs (- y2 y1))
       (abs (- x2 x1))))

  (define (estimate-integral-stream prob-stream)
    (stream-cons (* (stream-car prob-stream) (S x1 x2 y1 y2))
                 (estimate-integral-stream (stream-cdr prob-stream))))

  (estimate-integral-stream (monte-carlo experiment)))

(define (make-circle-pred center-x center-y r)
  (define (square x) (* x x))

  (define (belongs-to-circle? x y)
    (< (+ (square (- x center-x))
          (square (- y center-y)))
       (square r)))

  belongs-to-circle?)

(define pi-stream
  (stream-map (lambda (area) (/ area (* 0.5 0.5)))
              (estimate-integral (make-circle-pred 0.5 0.5 0.5) 0.0 1.0 0.0 1.0)))

(check-equal? (floor (* 10 (exact->inexact (stream-ref pi-stream 10000)))) 31.0)
