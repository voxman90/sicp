#lang racket

#|
  Упражнение 3.60

  Если степенной ряд представляется в виде потока своих коэффициентов, как в упражнении 3.59, то сумма
  последовательностей реализуется посредством add-streams. Завершите определение следующей процедуры для
  перемножения последовательностей:

    (define (mul-series s1 s2)
      (cons-stream <??> (add-streams <??> <??>)))

  Можете проверить свою процедуру, убедившись, что sin²x + cos²x = 1 с помощью последовательностей из
  упражнения 3.59
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

(define cosine-series
  (stream-cons 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams (mul-series (scale-stream (stream-cdr s2) (stream-car s1))
                                        (stream-cdr s1))
                            (mul-series (scale-stream (stream-cdr s1) (stream-car s2))
                                        (stream-cdr s2)))))

(define sum1 (add-streams (mul-series cosine-series cosine-series)
                          (mul-series sine-series sine-series)))

(define output1 (open-output-string))

(display-stream sum1 output1 8)

(check-equal? (get-output-string output1) "\n1\n0\n0\n0\n0\n0\n0\n0")

(close-output-port output1)

(define exp1 (mul-series ones ones))

(define output2 (open-output-string))

(display-stream exp1 output2 8)

(check-equal? (get-output-string output2) "\n1\n2\n4\n8\n16\n32\n64\n128")

(close-output-port output2)
