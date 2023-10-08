#lang racket

#|
  Упражнение 3.50

  Закончите следующее определение, которое обобщает процедуру stream-map , чтобы она позволяла использовать процедуры от нескольких аргументов, подобно стандартной map языка Scheme. Этот вариант map принимает процедуру от n аргументов и n списков и применяет процедуру ко всем первым элементам списков, всем вторым элементам списков и так далее. Возвращается список результатов. Например:

    (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
    (741 852 963)

    (map (lambda (x y) (+ x (* 2 y)))
         (list 1 2 3)
         (list 4 5 6))
    (9 12 15)

    (define (stream-map proc . argstreams)
      (if (<??> (car argstreams))
          the-empty-stream
          (<??>
           (apply proc (map <??> argstreams))
           (apply stream-map
                  (cons proc (map <??> argstreams))))))
|#

(#%require rackunit
           racket/stream)

(define (stream-null? s) (stream-empty? s))

(define the-empty-stream empty-stream)

(define (cons-stream first rest) (stream-cons first rest))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define first-stream (stream-enumerate-interval 1 5))
(define second-stream (stream-enumerate-interval 6 10))

(define stream-sum (stream-map + first-stream second-stream))
(check-equal? (stream-car stream-sum) 7)
(check-equal? (stream-car (stream-cdr stream-sum)) 9)

(define stream-product (stream-map * first-stream second-stream))
(check-equal? (stream-car stream-product) 6)
(check-equal? (stream-car (stream-cdr stream-product)) 14)
