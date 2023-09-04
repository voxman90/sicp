#lang sicp

#|
  Упражнение 2.35

  Переопределите count-leaves из раздела 2.2.2 в виде накопления:

    (define (count-leaves t)
      (accumulate <??> <??> (map <??> <??>)))
|#

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x)
                         (cond ((pair? x) (count-leaves x))
                               ((null? x) 0)
                               (else 1)))
                       tree)))

(check-equal? (count-leaves '()) 0)
(check-equal? (count-leaves '(1 2 3 4 5)) 5)
(check-equal? (count-leaves '(1 (2 (3) 4) () (1 (2 (() ()) (3 4) 5) 6))) 10)
