#lang sicp

#|
  Упражнение 3.19

  Переделайте упражнение 3.18, используя фиксированное количество памяти. (Тут нужна достаточно хитрая
  идея.)
|#

(#%require rackunit)

(define (cycle? L)
  (define (rec lst)
    (if (null? lst)
        #f
        (let ((item (car lst)))
        (if (and (pair? item)
                 (eq? (cdr item) 'passed))
            #t
            (begin
              (set-car! lst (cons item 'passed))
              (rec (cdr lst)))))))

  (rec L))

(define lE '())
(define l0 '(1 2 3))

(define lx '(1 2 3))
(define l1 (cons lx lx))

(define l2 '(1 2 3))
(set-cdr! (cddr l2) l2)

(define l3 '(1 2 3))
(set-cdr! (cddr l3) (cdr l3))

(define l4 '(1 2 3 4))
(set-car! (cddr l4) (cdr l4))

(check-false (cycle? lE))
(check-false (cycle? l0))
(check-false (cycle? l1))
(check-false (cycle? l4))
(check-true (cycle? l2))
(check-true (cycle? l3))
