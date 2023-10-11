#lang racket

#|
  Упражнение 3.70

  Интересно было бы уметь порождать потоки в каком-либо полезном порядке, а не в порядке, задаваемом к
  случаю придуманным процессом чередования. Можно воспользоваться методом, подобным процедуре merge из
  упражнения 3.56, если мы определим способ сказать, что одна пара целых чисел «меньше» другой. Один из
  способов состоит в том, чтобы определить «функцию взвешивания» W(i, j) и постановить, что (i₁, j₁)
  меньше, чем (i₂, j₂), если W(i₁, j₁) ≤ W(i₂, j₂). Напишите процедуру merge-weighted, которая во всем
  подобна merge, но только в качестве дополнительного аргумента принимает процедуру weight, которая
  вычисляет вес пары, и используется для определения порядка, в котором элементы должны появляться в
  получающемся смешанном потоке. При помощи merge-weighted напишите процедуру weighted-pairs, обобщающую
  pairs. Она должна принимать два потока и процедуру, вычисляющую функцию взвешивания, и порождать поток
  пар, упорядоченных по весу. Породите, используя эту процедуру:

    а. Поток всех пар натуральных чисел (i, j) где i ≤ j, упорядоченных по сумме i + j.

    б. поток всех пар натуральных чисел (i, j), где i ≤ j, ни i, ни j не делится ни на 2, ни на 3, ни
    на 5, и пары упорядочены по значению суммы 2i + 3j + 5ij.
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

(define int-stream1 (weighted-pairs integers integers +))

(define output1 (open-output-string))

(display-stream int-stream1 output1 10)

(check-equal? (get-output-string output1)
  "\n(1 1)\n(1 2)\n(1 3)\n(2 2)\n(1 4)\n(2 3)\n(1 5)\n(2 4)\n(3 3)\n(1 6)")

(close-output-port output1)

(define int-stream2
  (weighted-pairs integers integers
                  (lambda (i j) (+ (* 2 i) (* 3 j) (* 5 i j)))))

(define output2 (open-output-string))

(display-stream int-stream2 output2 10)

(check-equal? (get-output-string output2)
  "\n(1 1)\n(1 2)\n(1 3)\n(2 2)\n(1 4)\n(1 5)\n(2 3)\n(1 6)\n(2 4)\n(1 7)")

(close-output-port output2)

(define (pred1? x) (not (or (= (remainder x 2) 0)
                            (= (remainder x 3) 0)
                            (= (remainder x 5) 0))))

(define int-stream3
  (stream-filter (lambda (pair) (and (pred1? (car pair)) (pred1? (cadr pair))))
                 int-stream2))

(define output3 (open-output-string))

(display-stream int-stream3 output3 10)

(check-equal? (get-output-string output3)
  "\n(1 1)\n(1 7)\n(1 11)\n(1 13)\n(1 17)\n(1 19)\n(1 23)\n(1 29)\n(1 31)\n(7 7)")

(close-output-port output3)

