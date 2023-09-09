#lang sicp

#|
  Упражнение 2.64

  Следующая процедура list->tree преобразует упорядоченный список в сбалансированное бинарное дерево.
  Вспомогательная процедура partial-tree принимает в качестве аргументов целое число n и список по
  крайней мере из n элементов, и строит сбалансированное дерево из первых n элементов дерева. Результат,
  который возвращает partial-tree, — это пара (построенная через cons), car которой есть построенное
  дерево, а cdr — список элементов, не включенных в дерево.

    (define (list->tree elements)
      (car (partial-tree elements (length elements))))

    (define (partial-tree elts n)
      (if (= n 0)
          (cons '() elts)
          (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
              (let ((left-tree (car left-result))
                    (non-left-elts (cdr left-result))
                    (right-size (- n (+ left-size 1))))
                (let ((this-entry (car non-left-elts))
                      (right-result (partial-tree (cdr non-left-elts)
                                                  right-size)))
                  (let ((right-tree (car right-result))
                        (remaining-elts (cdr right-result)))
                    (cons (make-tree this-entry left-tree right-tree)
                          remaining-elts))))))))

  а. Дайте краткое описание, как можно более ясно объясняющее работу partial-tree. Нарисуйте дерево,
  которое list->tree строит из списка (1 3 5 7 9 11)

  б. Каков порядок роста по отношению к числу шагов, которые требуются процедуре list->tree для
  преобразования дерева из n элементов?
|#

(#%require rackunit)

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))

  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

#|
  Процедура partial-tree рекурсивно строит дерево из списка. На каждом шаге, где n не равно 0, на основе n
  (количества оставшихся элементов списка) определяется размер левого поддерева (так, чтобы правое и
  левое поддерево были по размеру равны между собой, если же это невозможно, то правое поддерево оказывается
  на один элемент больше) и рекурсивно строится левое поддерево фиксированного размера, из оставшихся
  элементов извлекается вершина и рекурсивно строится правое поддерево фиксированного размера, после
  процедура возращает получившееся поддерево и оставшиеся элементы списка. Для n = 0 процедура возвращает
  пустой список и оставшиеся элементы списка.

  Разберём работу процедуры для списка (1 3 5 7 9 11), ветви с числами здесь символизируют вершины:

    (partial-tree (1 3 5 7 9 11) 6)
      ├── (partial-tree (1 3 5 7 9 11) 2)
      │     ├── (partial-tree (1 3 5 7 9 11) 0)
      │     ├── 1
      │     └── (partial-tree (3 5 7 9 11) 1)
      │           ├── (partial-tree (3 5 7 9 11) 0)
      │           ├── 3
      │           └── (partial-tree (5 7 9 11) 0)
      ├── 5
      └── (partial-tree (7 9 11) 3)
            ├── (partial-tree (7 9 11) 1)
            │     ├── (partial-tree (7 9 11) 0)
            │     ├── 7
            │     └── (partial-tree (9 11) 0)
            ├── 9
            └── (partial-tree (11) 1)
                  ├── (partial-tree (11) 0)
                  ├── 11
                  └── (partial-tree () 0)

  Получаем дерево вида:

       5
      / \
     /   \
    1     9
     \   / \
      3 7   11

|#

(define list1 '(1 3 5 7 9 11))

(define tree1 (make-tree 5
                         (make-tree 1
                                   '()
                                    (make-tree 3
                                              '()
                                              '()))
                         (make-tree 9
                                    (make-tree 7
                                              '()
                                              '())
                                    (make-tree 11
                                              '()
                                              '()))))

(check-equal? (tree->list (list->tree list1)) list1)
(check-equal? (list->tree list1) tree1)

#|
  Оценим количество шагов, которое требуется процедуре, чтобы преобразовать список длины n в дерево:

    S(n) = S(⌊(n - 1)/2⌋) + C + S(⌈(n - 1)/2⌉),

  C - это некоторое постоянное количество шагов, которые затрачивает процедура на каждом шаге, когда
  n не равно нулю. Несложно заметить, что количество таких шагов будет в точности равно количеству
  вершин. Таким образом:

    S(n) = n * C + (n - 1) * S(0)

  Процедура возвращает пустой список ровно n - 1 раз. А количество шагов S(0) можно принять за 1.

  Получаем, что количество шагов растёт линейно: S(n) ∈ O(n).
|#
