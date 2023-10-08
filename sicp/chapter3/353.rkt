#lang racket

#|
  Упражнение 3.53

  Не запуская программу, опишите элементы потока, порождаемого

    (define s (cons-stream 1 (add-streams s s)))
|#

#|
  Это будет поток степеней двойки:

    1 -> 1 + 1 -> (1 + 1) + (1 + 1) -> ((1 + 1) + (1 + 1)) + ((1 + 1) + (1 + 1)) -> ...
    2⁰ -> 2¹ -> 2² -> 2³ -> ...
|#

(#%require rackunit
           racket/stream)

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (add-streams stream1 stream2)
  (stream-cons (+ (stream-car stream1) (stream-car stream2))
               (add-streams (stream-cdr stream1) (stream-cdr stream2))))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(define s (stream-cons 1 (add-streams s s)))

(define output1 (open-output-string))

(display-stream s output1 8)

(check-equal? (get-output-string output1) "\n1\n2\n4\n8\n16\n32\n64\n128")

(close-output-port output1)
