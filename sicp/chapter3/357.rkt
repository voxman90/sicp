#lang racket

#|
  Упражнение 3.57

  Сколько сложений происходит при вычислении n-го числа Фибоначчи, в случае, когда мы используем определение
  fibs через процедуру add-streams? Покажите, что число сложений выросло бы экспоненциально, если бы мы
  реализовали (delay <exp>) просто как (lambda () <exp>), без оптимизации через процедуру memo-proc из
  раздела 3.5.1
|#

#|
  Если мы используем мемоизацию, то для рассчёта Fib(n) требуется такое количество шагов:

    0, 0, 1, 2, 3, 4, 5, 6, 7, 8, ...
    s₁(0) = 0,
    s₁(n) = n - 2, n > 1

  Если мы не используем мемоизацию, то получаем такую последовательность значений:

    0, 0, 1, 3, 7, 14, 26, 46, 79, 133, ...
    s₂(0) = 0,
    s₂(1) = 0,
    s₂(2) = 1 = 1 + s₂(2) + s₂(1),
    s₂(3) = 3 = 2 + s₂(3) + s₂(2),
    s₂(4) = 7 = 3 + s₂(4) + s₂(3),
    ...,
    s₂(n) = (n - 1) + s₂(n - 1) + s₂(n - 2), n > 1,

  Очевидно, что s(n) больше Fib(n), n > 2. Так как n-ое число Фибоначчи есть ближайшее целое число к φⁿ/√5,
  где φ = (1 + √5)/2 (см. решение упражнения 1.13), то число шагов при рассчёте числа Фибоначчи через
  потоки без мемоизации растёт минимум экспоненциально.
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (stream-null? stream) (stream-empty? stream))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define counter 0)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (begin
        (set! counter (+ counter 1))
        (stream-cons
         (apply proc (map stream-car argstreams))
         (apply stream-map
                (cons proc (map stream-cdr argstreams)))))))

(define (add-streams stream1 stream2)
  (stream-map + stream1 stream2))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define fibs
  (stream-cons 0
               (stream-cons 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(check-equal? (stream-ref fibs 9) 34)

(check-equal? counter 8)

(define output1 (open-output-string))

(display-stream fibs output1 10)

(check-equal? (get-output-string output1) "\n0\n1\n1\n2\n3\n5\n8\n13\n21\n34")

(check-equal? counter 8)

(close-output-port output1)

(define counter-alt 0)

(define (stream-map-alt proc . argstreams)
  (if (null? (car argstreams))
     '()
      (begin
        (set! counter-alt (+ counter-alt 1))
        (cons
         (apply proc (map car argstreams))
         (lambda () (apply stream-map-alt
                           (cons proc
                                 (map (lambda (stream) ((cdr stream)))
                                      argstreams))))))))

(define (add-streams-alt stream1 stream2)
  (stream-map-alt + stream1 stream2))

(define fibs-alt
  (cons 0
        (lambda ()
          (cons 1
                (lambda () (add-streams-alt ((cdr fibs-alt))
                                            fibs-alt))))))

(define (stream-ref-alt s n)
  (if (= n 0)
      (car s)
      (stream-ref-alt ((cdr s)) (- n 1))))

(check-equal? (stream-ref-alt fibs-alt 9) 34)

(check-equal? counter-alt 133)
