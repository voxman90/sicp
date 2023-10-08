#lang racket

#|
  Упражнение 3.54

  Определите процедуру mul-streams, аналогичную add-streams, которая порождает поэлементное произведение
  двух входных потоков. С помощью нее и потока integers закончите следующее определение потока, n-й
  элемент которого (начиная с 0) равен факториалу n + 1:

    (define factorials (cons-stream 1 (mul-streams <??> <??>)))
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

(define factorials (stream-cons 1 (mul-streams (stream-cdr integers) factorials)))

(define output1 (open-output-string))

(display-stream factorials output1 6)

(check-equal? (get-output-string output1) "\n1\n2\n6\n24\n120\n720")

(close-output-port output1)
