#lang racket

#|
  Упражнение 3.65

  С помощью ряда

    ln 2 = 1 - 1/2 + 1/3 - 1/4 + ···,

  породите три последовательности приближений к натуральному логарифму 2, так же, как мы выше сделали
  это для π. Как быстро сходятся эти последовательности?
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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(define (partial-sums stream)
  (stream-cons (stream-car stream)
               (add-streams (stream-cdr stream)
                            (partial-sums stream))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (ln2-sequence n)
  (stream-cons (/ 1 n)
               (stream-map - (ln2-sequence (+ n 1)))))

(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
    (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (stream-cons s
    (make-tableau transform
                  (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define test-sequence (ln2-sequence 1))

(define euler-test-sequence (euler-transform test-sequence))

(define accelerated-test-sequence
  (accelerated-sequence euler-transform test-sequence))

(define output1 (open-output-string))

(display-stream test-sequence output1 8)

(check-equal? (get-output-string output1) "\n1\n-1/2\n1/3\n-1/4\n1/5\n-1/6\n1/7\n-1/8")

(close-output-port output1)

(define output2 (open-output-string))

(display-stream (partial-sums test-sequence) output2 8)

(check-equal? (get-output-string output2) "\n1\n1/2\n5/6\n7/12\n47/60\n37/60\n319/420\n533/840")

(close-output-port output2)

(define output3 (open-output-string))

(display-stream euler-test-sequence output3 8)

(check-equal? (get-output-string output3)
              "\n1/28\n-1/102\n1/248\n-1/490\n1/852\n-1/1358\n1/2032\n-1/2898")

(close-output-port output3)

(define output4 (open-output-string))

(display-stream accelerated-test-sequence output4 3)

(check-equal? (get-output-string output4) "\n1\n1/28\n173/214404")

(close-output-port output4)
