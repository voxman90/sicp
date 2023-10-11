#lang racket

#|
  Упражнение 3.71

  Числа, которые можно выразить в виде суммы двух кубов более, чем одним способом, иногда называют числами
  Рамануджана (Ramanujan numbers), в честь математика Шринивасы Рамануджана. Упорядоченные потоки пар
  предлагают изящное решение для задачи порождения таких чисел. Чтобы найти число, которое можно двумя
  разными способами записать в виде суммы двух кубов, требуется только породить поток пар натуральных
  чисел (i, j), взвешенных согласно сумме i³ + j³ (см. упражнение 3.70), и искать в этом потоке две пары
  подряд с одинаковым весом. Напишите процедуру для порождения чисел Рамануджана. Первое такое число 1729.
  Каковы следующие пять?
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

(define (cube x) (* x x x))

(define (cube-sum i j) (+ (cube i) (cube j)))

(define cube-weighted-stream1
  (weighted-pairs integers
                  integers
                  cube-sum))

(define ramanujan-numbers
  ((lambda (s)
     (stream-map
       (lambda (pair) (car pair))
       (stream-filter
         (lambda (pair) (= (car pair) (cdr pair)))
         (stream-map (lambda (pair1 pair2) (cons (apply cube-sum pair1) (apply cube-sum pair2)))
                     s (stream-cdr s)))))
   cube-weighted-stream1))

(define output1 (open-output-string))

(display-stream ramanujan-numbers output1 10)

(check-equal? (get-output-string output1)
  "\n1729\n4104\n13832\n20683\n32832\n39312\n40033\n46683\n64232\n65728")

(close-output-port output1)
