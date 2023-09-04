#lang sicp

#|
  Упражнение 2.26

  Допустим, мы определили x и y как два списка:

    (define x (list 1 2 3))

    (define y (list 4 5 6))

  Какой результат напечатает интерпретатор в ответ на следующие выражения:

    (append x y)

    (cons x y)

    (list x y)
|#

#|
    (append x y) => (1 2 3 4 5 6)

    (cons x y) => ((1 2 3) 4 5 6)

    (list x y) => ((1 2 3) (4 5 6))
|#

(#%require rackunit)

(define x (list 1 2 3))
(define y (list 4 5 6))

(check-equal? (append x y) '(1 2 3 4 5 6))
(check-equal? (cons x y) '((1 2 3) 4 5 6))
(check-equal? (list x y) '((1 2 3) (4 5 6)))

#|
  Так как:

    (list x y) =>
    => (cons x (cons y nil)) =>
    => (cons (cons 1 (cons 2 (cons 3 nil))) (cons (cons 4 (cons 5 (cons 6 nil))) nil))

  В свою очередь:

    (cons x y) => (cons (cons 1 (cons 2 (cons 3 nil)) (cons 4 (cons 5 (cons 6 nil)))))
|#
