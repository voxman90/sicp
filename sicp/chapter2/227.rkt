#lang sicp

#|
  Упражнение 2.27

  Измените свою процедуру reverse из упражнения 2.18 так, чтобы получилась процедура deep-reverse,
  которая принимает список в качестве аргумента и возвращает в качестве значения список, где порядок
  элементов обратный и подсписки также обращены. Например:

    (define x (list (list 1 2) (list 3 4)))

    x
    ((1 2) (3 4))

    (reverse x)
    ((3 4) (1 2))

    (deep-reverse x)
    ((4 3) (2 1))
|#

(#%require rackunit)

(define nil '())

(define (deep-reverse l)
  (define (iter l base)
    (cond ((pair? l)
           (let ((head (car l))
                 (tail (cdr l)))
             (cond ((null? head)
                    (iter tail (cons nil base)))
                   ((pair? head)
                    (iter tail (cons (iter head nil) base)))
                   (else
                    (iter tail (cons head base))))))
          (else base)))

  (iter l nil))

(check-equal? (deep-reverse '()) '())
(check-equal? (deep-reverse '(() (() ()))) '((() ()) ()))
(check-equal? (deep-reverse '((() ()) (() ((()))) (1) 2)) '(2 (1) (((())) ()) (() ())))
(check-equal? (deep-reverse '((1 2) (3 4) (5 6))) '((6 5) (4 3) (2 1)))
(check-equal? (deep-reverse '((1 (1 2)) (3 (3 4)) (5 6))) '((6 5) ((4 3) 3) ((2 1) 1)))
(check-equal? (deep-reverse (list (list 1 2) (list 3 4))) (list (list 4 3) (list 2 1)))
