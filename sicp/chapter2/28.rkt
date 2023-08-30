#lang sicp

#|
  Упражнение 2.8

  Рассуждая в духе Лизы, опишите, как можно вычислить разность двух интервалов. Напишите
  соответствующую процедуру вычитания, называемую sub-interval.
|#

(#%require rackunit)

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

(define lower1 5)
(define upper1 10)

(define lower2 15)
(define upper2 25)

(define interval1 (make-interval lower1 upper1))

(define interval2 (make-interval lower2 upper2))

(check-equal? (sub-interval interval1 interval2) '(-20 . -5))
(check-equal? (sub-interval interval2 interval1) '(5 . 20))
(check-equal? (sub-interval (make-interval (- 3) (- 3)) (make-interval 1 2)) '(-5 . -4))
