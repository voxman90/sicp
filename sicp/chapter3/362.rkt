#lang racket

#|
  Упражнение 3.62

  При помощи результатов упражнений 3.60 и 3.61 определите процедуру div-series, которая делит один
  степенной ряд на другой. Div-series должна работать для любых двух рядов, при условии, что ряд в
  знаменателе начинается с ненулевого постоянного члена. (Если в знаменателе постоянный член равен нулю,
  div-series должна сообщать об ошибке.) Покажите, как при помощи div-series и результата упражнения
  3.59 получить степенной ряд для тангенса.
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

(define (sub-streams s1 s2)
  (stream-map - s1 s2))

(define (div-series s1 s2)
  (define (div s1 s2)
    (scale-stream (stream-cons (stream-car s1)
                               (sub-streams (stream-cdr s1)
                                            (mul-series (stream-cdr s2)
                                                        (div s1 s2))))
                  (/ 1 (stream-car s2))))

  (if (= (stream-car s2) 0)
      (error "Division by zero -- DIV-SERIES")
      (div s1 s2)))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cosine-series
  (stream-cons 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define output1 (open-output-string))

(display-stream (div-series sine-series cosine-series) output1 8)

(check-equal? (get-output-string output1) "\n0\n1\n0\n1/3\n0\n2/15\n0\n17/315")

(close-output-port output1)

(define output2 (open-output-string))

(display-stream (div-series integers (scale-stream integers 4)) output2 8)

(check-equal? (get-output-string output2) "\n1/4\n0\n0\n0\n0\n0\n0\n0")

(close-output-port output2)
