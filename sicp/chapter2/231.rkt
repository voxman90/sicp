#lang sicp

#|
  Упражнение 2.31

  Абстрагируйте свой ответ на упражнение 2.30, получая процедуру tree-map, так, чтобы square-tree можно
  было определить следующим образом:

    (define (square-tree tree) (tree-map square tree))
|#

(#%require rackunit)

(define (tree-map callback tree)
  (cond ((null? tree) tree)
        ((pair? tree)
         (cons (tree-map callback (car tree))
               (tree-map callback (cdr tree))))
        (else (callback tree))))

(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))

(define tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(check-equal? (square-tree '()) '())
(check-equal? (square-tree tree1) '(1 (4 (9 16) 25) (36 49)))
(check-equal? (square-tree '(1 (() 7) 2 () 3)) '(1 (() 49) 4 () 9))
