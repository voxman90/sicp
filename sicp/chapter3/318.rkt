#lang sicp

#|
  Упражнение 3.18

  Напишите процедуру, которая рассматривает список и определяет, содержится ли в нем цикл, то есть, не
  войдет ли программа, которая попытается добраться до конца списка, продвигаясь по полям cdr, в бесконечный
  цикл. Такие списки порождались в упражнении 3.13.
|#

(#%require rackunit)

(define (cycle? L)
  (define (rec lst passed-cons)
    (if (null? lst)
        #f
        (if (memq lst passed-cons)
            #t
            (rec (cdr lst) (cons lst passed-cons)))))

  (rec L '()))

(define lE '())
(define l0 '(1 2 3))
(define l1 (cons l0 l0))
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
