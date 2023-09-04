#lang sicp

#|
  Упражнение 2.30

  Определите процедуру square-tree, подобную процедуре square-list из упражнения 2.21. А именно,
  square-tree должна вести себя следующим образом:

    (square-tree
      (list 1
            (list 2 (list 3 4) 5)
            (list 6 7)))
    (1 (4 (9 16) 25) (36 49))

  Определите square-tree как прямо (то есть без использования процедур высших порядков), так и с помощью
  map и рекурсии.
|#

(#%require rackunit)

(define (square-tree tree)
  (cond ((null? tree) '())
        ((pair? tree)
         (cons (square-tree (car tree)) (square-tree (cdr tree))))
        (else (* tree tree))))

(define (square-tree-alt tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-alt sub-tree)
             (* sub-tree sub-tree)))
       tree))

(define tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(check-equal? (square-tree '()) '())
(check-equal? (square-tree '(1 2 3)) '(1 4 9))
(check-equal? (square-tree tree1) '(1 (4 (9 16) 25) (36 49)))
(check-equal? (square-tree-alt '()) '())
(check-equal? (square-tree-alt '(1 2 3)) '(1 4 9))
(check-equal? (square-tree-alt tree1) '(1 (4 (9 16) 25) (36 49)))
