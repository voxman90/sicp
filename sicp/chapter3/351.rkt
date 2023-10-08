#lang racket

#|
  Упражнение 3.51

  Чтобы внимательнее изучить задержанные вычисления, мы воспользуемся следующей процедурой, которая
  печатает свой аргумент, а затем возвращает его:

    (define (show x)
      (display-line x)
      x)

  Что печатает интерпретатор в ответ на каждое выражение из следующей последовательности?

    (define x (stream-map show (stream-enumerate-interval 0 10)))

    (stream-ref x 5)

    (stream-ref x 7)
|#

#|
  (stream-ref x 5)
  55

  (stream-ref x 7)
  77
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (cons-stream first rest) (stream-cons first rest))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (display-line x out)
  (newline out)
  (display x out))

(define output1 (open-output-string))

(define (show x)
  (display-line x output1)
  x)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define x (stream-map show (stream-enumerate-interval 0 10)))

(check-equal? (get-output-string output1) "")

(display (stream-ref x 5) output1)
(check-equal? (get-output-string output1) "\n55")

(display (stream-ref x 7) output1)
(check-equal? (get-output-string output1) "\n55\n77")
(close-output-port output1)
