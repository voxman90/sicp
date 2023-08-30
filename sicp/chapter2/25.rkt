#lang sicp

#|
  Упражнение 2.5

  Покажите, что можно представлять пары неотрицательных целых чисел, используя только числа и
  арифметические операции, если представлять пару a и b как произведение 2ᵃ3ᵇ. Дайте соответствующие
  определения процедур cons, car и cdr.
|#

(#%require rackunit)

(define (^ base exp)
  (define (iter base acc n)
    (cond ((= n 0) acc)
          ((even? n) (iter (* base base) acc (/ n 2)))
          (else (iter base (* acc base) (- n 1)))))

  (iter base 1 exp))

(define (cons a b)
  (* (^ 2 a) (^ 3 b)))

(define (division-count a divisor)
  (define (iter count div)
    (if (not (= (remainder a div) 0))
        count
        (iter (+ count 1) (* div divisor))))

  (iter 0 divisor))

(define (car pair)
  (division-count pair 2))

(define (cdr pair)
  (division-count pair 3))

(define a 2)
(define b 3)
(define x (cons a b))
(define x2 (cons 0 1))
(define x3 (cons 1 0))

(check-equal? x 108)
(check-equal? x2 3)
(check-equal? x3 2)
(check-equal? (car x) a)
(check-equal? (cdr x) b)
(check-equal? (car x2) 0)
(check-equal? (cdr x2) 1)
(check-equal? (car x3) 1)
(check-equal? (cdr x3) 0)
