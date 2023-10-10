#lang racket

#|
  Упражнение 3.66

  Рассмотрим поток (pairs integers integers). Можете ли Вы что-то сказать о порядке, в котором пары
  попадают в поток? Например, сколько приблизительно пар предшествуют паре (1, 100)? Паре (99, 100)?
  (100, 100)? (Если Вы способны предоставить точные математические утверждения, — прекрасно. Однако
  если Вы увязаете в деталях, достаточно качественных оценок.)
|#

#|
  Пара (m, m) появляется после 2ᵐ - 2 пар.

  Пара (m, n), где m фиксировано и n > m, появляются после пары (m m) через 2ᵐ⁻¹ - 1 + 2ᵐ(n - m - 1) пар.

  Т.е. (m, n), где n > m появляется после (2ᵐ - 2) + 1 + 2ᵐ⁻¹ - 1 + 2ᵐ(n - m - 1) = 2ᵐ⁻¹(1 + 2n - 2m) - 2
  пар от начала.

  Обозначим количество пар до конкретной пары, как B от этой пары. Тогда:

    B(1, 1) = 2¹ - 2 = 0,
    B(1, 2) = 2⁰(1 + 4 - 2) - 2 = 1,
    B(2, 2) = 2² - 2 = 2,
    B(1, 3) = 2⁰(1 + 6 - 2) - 2 = 3,
    B(2, 3) = 2¹(1 + 6 - 4) - 2 = 4,
    B(1, 4) = 2⁰(1 + 8 - 2) - 2 = 5,
    B(3, 3) = 2³ - 2 = 6,
    ...

    B(1, 100) = 2⁰(1 + 200 - 2) - 2 = 197,
    B(99, 100) = 2⁹⁸(1 + 200 - 198) - 2 = 3*2⁹⁸ - 2,
    B(100, 100) = 2¹⁰⁰ - 2
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
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams integers ones)))

(define int-pairs (pairs integers integers))

(check-equal? (stream-ref int-pairs 197) '(1 100))

(check-equal? (stream-ref int-pairs (- (expt 2 10) 2)) '(10 10))

(check-equal? (stream-ref int-pairs (- (* 3 (expt 2 8)) 2)) '(9 10))
