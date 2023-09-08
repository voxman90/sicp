#lang sicp

#|
  Упражнение 2.61

  Напишите реализацию adjoin-set для упорядоченного представления. По аналогии с element-of-set? покажите,
  как использовать упорядочение, чтобы получить процедуру, которая в среднем требует только половину
  числа шагов, которое требуется при неупорядоченном представлении.
|#

(#%require rackunit)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define x (adjoin-set 2 '()))
(define y (adjoin-set 0 x))
(define z (adjoin-set 1 y))
(define q (adjoin-set 1 z))

(check-equal? z q)
(check-equal? (adjoin-set 1 (adjoin-set 0 (adjoin-set 2 '())))
              (adjoin-set 1 (adjoin-set 2 (adjoin-set 0 '()))))
