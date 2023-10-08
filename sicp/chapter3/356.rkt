#lang racket

#|
  Упражнение 3.56

  Существует знаменитая задача, впервые сформулированная Р. Хэммингом: породить в возрастающем порядке
  и без повторений все положительные целые числа, у которых нет других простых делителей, кроме 2, 3 и
  5. Очевидное решение состоит в том, чтобы перебирать все натуральные числа по очереди и проверять,
  есть ли у них простые множители помимо 2, 3 и 5. Однако эта процедура весьма неэффективна, поскольку
  чем больше числа, тем меньшая их доля соответствует условию. Применим альтернативный подход: назовем
  искомый поток чисел S и обратим внимание на следующие факты:

    • S начинается с 1.

    • Элементы (scale-streams 2) также принадлежат S.

    • То же верно и для (scale-stream S 3) и (scale-stream S 5).

    • Других элементов S нет.

  Теперь требуется только соединить элементы из этих источников. Для этого мы определяем процедуру merge,
  которая сливает два упорядоченных потока в один упорядоченный поток, убирая при этом повторения:

    (define (merge s1 s2)
      (cond ((stream-null? s1) s2)
            ((stream-null? s2) s1)
            (else
             (let ((s1car (stream-car s1))
                   (s2car (stream-car s2)))
               (cond ((< s1car s2car)
                      (cons-stream s1car (merge (stream-cdr s1) s2)))
                     ((> s1car s2car)
                      (cons-stream s2car (merge s1 (stream-cdr s2))))
                     (else
                      (cons-stream s1car
                                   (merge (stream-cdr s1)
                                          (stream-cdr s2)))))))))

  Тогда требуемый поток можно получить с помощью merge таким образом:

    (define S (cons-stream 1 (merge <??> <??>)))

  Заполните пропуски в местах, обозначенных знаком <??>
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

(define (mul-streams stream1 stream2)
  (stream-map * stream1 stream2))

(define (scale-stream stream mul)
  (stream-map (lambda (x) (* mul x))
              stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (stream-cons s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (stream-cons s2car (merge s1 (stream-cdr s2))))
                  (else
                   (stream-cons s1car
                                (merge (stream-cdr s1)
                                       (stream-cdr s2)))))))))

(define S (stream-cons 1 (merge (merge (scale-stream S 5) (scale-stream S 3))
                                (scale-stream S 2))))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(define output1 (open-output-string))

(display-stream S output1 10)

(check-equal? (get-output-string output1) "\n1\n2\n3\n4\n5\n6\n8\n9\n10\n12")

(close-output-port output1)
