#lang racket

#|
  Упражнение 3.69

  Напишите процедуру triples, которая берет три бесконечных потока S, T и U, и порождает поток троек
  (Sᵢ, Tⱼ, Uₖ) , таких, что i ≤ j ≤ k. С помощью triples породите поток всех Пифагоровых троек натуральных
  чисел, т. е. таких троек (i, j, k), что i ≤ j и i² + j² = k².
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

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (stream-cons
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (pair) (cons (stream-car s) pair))
                  (stream-cdr (pairs t u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams integers ones)))

(define int-triples (triples integers integers integers))

(define output1 (open-output-string))

(display-stream int-triples output1 7)

(check-equal? (get-output-string output1)
  "\n(1 1 1)\n(1 1 2)\n(2 2 2)\n(1 2 2)\n(2 2 3)\n(1 1 3)\n(3 3 3)")

(close-output-port output1)

(define (square x) (* x x))

(define pythagorean-triples
  (stream-filter
    (lambda (triple) (= (+ (square (car triple)) (square (cadr triple)))
                        (square (caddr triple))))
    int-triples))

(define output2 (open-output-string))

(display-stream pythagorean-triples output2 5)

(check-equal? (get-output-string output2)
  "\n(3 4 5)\n(6 8 10)\n(5 12 13)\n(9 12 15)\n(8 15 17)")

(close-output-port output2)
