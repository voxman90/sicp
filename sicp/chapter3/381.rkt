#lang racket

#|
  Упражнение 3.81

  В упражнении 3.6 обсуждалась возможность обобщить генератор случайных чисел и позволить пользователю
  сбрасывать последовательность случайных чисел, так, чтобы можно было порождать воспроизводимые
  «случайные» последовательности. Постройте потоковый вариант такой же процедуры-генератора, которая
  работает со входным потоком запросов вида generate — породить новое число, либо reset — сбросить
  последовательность в нужную точку, и которая порождает требуемый поток случайных чисел. Не используйте
  в своем решении присваивание.
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

(define (make-linear-congruent-gen x0)
  (let ((m (inexact->exact
             (expt 2 (+ 16 (ceiling (log 2 (ceiling (abs x0)))))))))
    (let ((a (random m))
          (c (random m)))
      (define (linear-congruent-gen x)
        (stream-cons x
                     (linear-congruent-gen (modulo (+ (* a x) c) m))))

      linear-congruent-gen)))

(define (make-random-numbers-stream initial-value generator)
  (define (dispatch command-stream random-numbers-stream)
    (let ((op (stream-car command-stream)))
      (if (null? op)
          the-empty-stream
          (cond ((eq? op 'generate)
                 (stream-cons (stream-car random-numbers-stream)
                              (dispatch (stream-cdr command-stream)
                                        (stream-cdr random-numbers-stream))))
                ((eq? op 'reset)
                 (dispatch (stream-cons 'generate (stream-cdr command-stream))
                           (generator initial-value)))
                (else
                 (dispatch (stream-cdr command-stream)
                           (stream-cons (stream-car random-numbers-stream)
                                        (generator op))))))))

      (lambda (commands) (dispatch commands (generator initial-value))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define random-numbers (make-random-numbers-stream 10 (make-linear-congruent-gen 10)))

(define test-stream (stream-cons 'generate
                    (stream-cons 'generate
                    (stream-cons 'generate '()))))

(define s0 (random-numbers test-stream))

(define test-reset-stream (stream-cons (stream-ref s0 0)
                          (stream-cons 'generate
                          (stream-cons 'generate
                          (stream-cons 'generate
                          (stream-cons 'generate '()))))))

(define s1 (random-numbers test-reset-stream))

(check-equal? (stream-ref s0 0) (stream-ref s1 0))
(check-equal? (stream-ref s0 0) (stream-ref s1 1))
(check-equal? (stream-ref s0 1) (stream-ref s1 2))
(check-equal? (stream-ref s0 2) (stream-ref s1 3))
