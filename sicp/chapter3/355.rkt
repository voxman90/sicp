#lang racket

#|
  Упражнение 3.55

  Определите процедуру partial-sums, которая в качестве аргумента берет поток S, а возвращает поток,
  элементы которого равны S₀, S₀ + S₁, S₀ + S₁ + S₂, ... . Например, (partial-sums integers) должно
  давать поток 1, 3, 6, 10, 15 ... .
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

(define (partial-sums stream)
  (stream-cons (stream-car stream)
               (stream-map (lambda (item) (+ (stream-car stream) item))
                           (partial-sums (stream-cdr stream)))))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams ones integers)))

(define integers-sum (partial-sums integers))

(define output1 (open-output-string))

(display-stream integers-sum output1 6)

(check-equal? (get-output-string output1) "\n1\n3\n6\n10\n15\n21")

(close-output-port output1)

(define output2 (open-output-string))

(define ones-sum (partial-sums ones))

(display-stream ones-sum output2 6)

(check-equal? (get-output-string output2) "\n1\n2\n3\n4\n5\n6")

(close-output-port output2)
