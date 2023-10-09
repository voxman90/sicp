#lang racket

#|
  Упражнение 3.61

  Пусть S будет степенным рядом (упражнение 3.59) с постоянным членом 1. Предположим, что мы хотим найти
  степенной ряд 1/S, то есть такой ряд X, что S·X = 1. Запишем S = 1 + Sᵣ, где Sᵣ — часть S после постоянного
  члена. Тогда мы можем решить уравнение для X так:

           S·X = 1
    (1 + Sᵣ)·X = 1
      X + Sᵣ·X = 1
             X = 1 − Sᵣ·X

  Другими словами, X есть степенной ряд с постоянным членом 1, чьи члены с более высокими степенями
  определяются как минус произведение SR и X. Воспользовавшись этим, напишите процедуру invert-unit-series,
  которая вычисляет 1/S для степенного ряда S с постоянным членом 1. Вам потребуется mul-series из
  упражнения 3.60
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

(define (add-streams stream1 stream2)
  (stream-map + stream1 stream2))

(define (mul-streams stream1 stream2)
  (stream-map * stream1 stream2))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams ones integers)))

(define (integrate-series coeff-stream)
  (define (rec stream int)
    (stream-cons (/ (stream-car stream) (stream-car int))
                 (rec (stream-cdr stream) (stream-cdr int))))

  (rec coeff-stream integers))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series stream)
  (stream-cons 1
               (mul-series (stream-cdr (scale-stream stream -1))
                           (invert-unit-series stream))))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define output1 (open-output-string))

(display-stream (mul-series (invert-unit-series ones) ones) output1 8)

(check-equal? (get-output-string output1) "\n1\n0\n0\n0\n0\n0\n0\n0")

(close-output-port output1)

(define output2 (open-output-string))

(display-stream (mul-series (invert-unit-series exp-series) exp-series) output2 8)

(check-equal? (get-output-string output2) "\n1\n0\n0\n0\n0\n0\n0\n0")

(close-output-port output2)
