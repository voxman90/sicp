#lang racket

#|
  Упражнение 3.72

  Используя метод, подобный описанному в упражнении 3.71, породите поток всех чисел, которые можно
  записать как сумму двух квадратов тремя различными способами (и покажите, каковы эти способы).
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

(define (merge-weighted s1 s2 weigth-proc)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let* ((s1car (stream-car s1))
                 (s2car (stream-car s2))
                 (w1 (apply weigth-proc s1car))
                 (w2 (apply weigth-proc s2car)))
            (cond ((<= w1 w2)
                   (stream-cons s1car (merge-weighted (stream-cdr s1) s2 weigth-proc)))
                  (else
                   (stream-cons s2car (merge-weighted s1 (stream-cdr s2) weigth-proc)))
                  )))))

(define (weighted-pairs s t weigth-proc)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weigth-proc)
      weigth-proc)))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams integers ones)))

(define (square x) (* x x))

(define (square-sum i j) (+ (square i) (square j)))

(define square-weighted-stream1
  (weighted-pairs integers
                  integers
                  square-sum))

(define square-numbers
  ((lambda (s)
    (stream-map
       (lambda (triple) (list (apply square-sum (car triple)) triple))
       (stream-filter
         (lambda (triple)
           (= (apply square-sum (car triple))
              (apply square-sum (cadr triple))
              (apply square-sum (caddr triple))))
         (stream-map
           (lambda (pair1 pair2 pair3) (list pair1 pair2 pair3))
           s (stream-cdr s) (stream-cdr (stream-cdr s))))))
   square-weighted-stream1))

(define output1 (open-output-string))

(display-stream square-numbers output1 3)

(check-equal? (get-output-string output1)
  "\n(325 ((1 18) (6 17) (10 15)))\n(425 ((5 20) (8 19) (13 16)))\n(650 ((5 25) (11 23) (17 19)))")

(close-output-port output1)
