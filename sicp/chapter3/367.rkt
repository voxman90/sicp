#lang racket

#|
  Упражнение 3.67

  Измените процедуру так, чтобы (pairs integers integers) порождало поток из всех пар натуральных чисел
  (i, j), без дополнительного условия i ≤ j. Подсказка: потребуется примешать еще один поток.
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

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (stream-map (lambda (x) (list x (stream-car t)))
                  (stream-cdr s)))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams integers ones)))

(define int-pairs (pairs integers integers))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(define output1 (open-output-string))

(display-stream int-pairs output1 7)

(check-equal? (get-output-string output1) "\n(1 1)\n(1 2)\n(2 2)\n(2 1)\n(2 3)\n(1 3)\n(3 3)")

(close-output-port output1)
