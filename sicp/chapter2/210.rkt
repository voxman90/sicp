#lang racket

#|
  Упражнение 2.10

  Бен Битобор, системный программист-эксперт, смотрит через плечо Лизы и замечает: неясно, что должно
  означать деление на интервал, пересекающий ноль. Модифицируйте код Лизы так, чтобы программа
  проверяла это условие и сообщала об ошибке, если оно возникает.
|#

(#%require rackunit
           racket/exn)

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (zero-boundary? interval)
  (= (* (lower-bound interval)
        (upper-bound interval))
     0))

(define (div-interval x y)
  (if (zero-boundary? y)
      (error "division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define x (make-interval 5.0 15.0))
(define y (make-interval -5.0 0.0))
(define z (make-interval 0.0 5.0))
(define g (make-interval -5.0 5.0))

(define (attempt a b)
  (with-handlers ([exn:fail?
                   (lambda (e) (exn->string e))])
    (div-interval a b)))

(define error-message "division by zero")
; substring used because checking system has extra output
(check-equal? (substring (attempt x y) 0 16) error-message)
(check-equal? (substring (attempt x y) 0 16) error-message)
(check-equal? (substring (attempt x z) 0 16) error-message)
(check-equal? (attempt z x) (make-interval 0.0 1.0))
(check-equal? (attempt z g) (make-interval -1.0 1.0))
(check-equal? (attempt x (make-interval 1 1)) x)
