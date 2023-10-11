#lang racket

#|
  Упражнение 3.73

  Можно моделировать электрические цепи с помощью потоков, представляющих значения тока или напряжения
  в определенные моменты времени. Допустим, например, что у нас имеется цепь RC (RC circuit), состоящая
  из резистора с сопротивлением R и конденсатора емкостью C, соединенных последовательно. Значение
  напряжения v в зависимости от заданного тока i определяется формулой, показанной на рис. 3.33. Структура
  формулы показана на прилагаемой диаграмме потока сигналов.

  Напишите процедуру RC, моделирующую эту цепь. На входе RC должна получать значения R, C и dt, и выдавать
  процедуру, которая принимает на входе поток значений тока i и начальное значение напряжения v₀, а на
  выходе выдает поток значений напряжения v. Например, у Вас должна быть возможность смоделировать при
  помощи RC RC-цепь с R = 5 ом, C = 1 фарад, и временным шагом в 0.5 секунды, вычислив (define RC1 (RC
  5 1 0.5)). Здесь RC1 определяется как процедура, которая принимает на входе поток, представляющий
  временную последовательность токов, и исходное напряжение на конденсаторе, а на выходе дает временной
  поток напряжений.
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (stream-null? stream) (stream-empty? stream))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-ref stream n)
  (if (<= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (stream-cons
        (apply proc (map stream-car streams))
        (apply stream-map
               (cons proc (map stream-cdr streams))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams ones integers)))

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))

  int)

(define (RC R C dt)
  (define 1/C (/ 1 C))

  (lambda (input initial-value)
    (add-streams (scale-stream input R)
                 (integral (scale-stream input 1/C) initial-value dt))))

(define RC1 (RC 5 1 0.5))

(define voltage-stream (RC1 integers 1))

(check-equal? (stream-ref voltage-stream 0) 6)
(check-equal? (stream-ref voltage-stream 1) 11.5)
(check-equal? (stream-ref voltage-stream 2) 17.5)
