#lang sicp

#|
  Упражнение 2.12

  Определите конструктор make-center-percent, который принимает среднее значение и погрешность в
  процентах и выдает требуемый интервал. Нужно также определить селектор percent, который для
  данного интервала выдает погрешность в процентах. Селектор center остается тем же, что приведен
  выше.
|#

(#%require rackunit)

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (make-center-percent center width-percent)
  (let ((width (* (abs center) (/ width-percent 100))))
    (make-interval (- center width) (+ center width))))

(define (center interval)
  (/ (+ (lower-bound interval)
        (upper-bound interval))
     2))

(define (percent interval)
  (* 100
     (/ (/ (- (upper-bound interval)
              (lower-bound interval))
        2)
        (abs (center interval)))))

(define center-test 4)
(define interval (make-interval 3 5))
(define center-interval (make-center-percent center-test 25))

(define center-test2 100)
(define interval2 (make-interval 90 110))
(define center-interval2 (make-center-percent center-test2 10))

(check-equal? (center interval) center-test)
(check-equal? interval center-interval)
(check-equal? (center interval2) center-test2)
(check-equal? interval2 center-interval2)
(check-equal? (percent interval) 25)
(check-equal? (percent center-interval2) 10)
