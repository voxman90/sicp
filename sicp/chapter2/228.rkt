#lang sicp

#|
  Упражнение 2.28

  Напишите процедуру fringe, которая берет в качестве аргумента дерево (представленное в виде списка)
  и возвращает список, элементы которого — все листья дерева, упорядоченные слева направо. Например,

    (define x (list (list 1 2) (list 3 4)))

    (fringe x)
    (1 2 3 4)

    (fringe (list x x))
    (1 2 3 4 1 2 3 4)
|#

(#%require rackunit)

(define nil '())

(define (fringe l)
  (cond ((null? l) nil)
        ((pair? l)
         (let ((head (car l))
               (tail (cdr l)))
           (cond ((pair? head)
                  (append (fringe head) (fringe tail)))
                 (else
                  (cons head (fringe tail))))))
        (else l)))

(check-equal? (fringe '()) '())
(check-equal? (fringe '(1 2 3)) '(1 2 3))
(check-equal? (fringe '(1 (2 (3) 4) (1 (2 (3 4) 5) 6))) '(1 2 3 4 1 2 3 4 5 6))
(check-equal? (fringe '(1 (2 (3) 4) () (1 (2 (() ()) (3 4) 5) 6))) '(1 2 3 4 () 1 2 () () 3 4 5 6))
