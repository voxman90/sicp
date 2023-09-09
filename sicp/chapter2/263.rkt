#lang sicp

#|
  Упражнение 2.63

  Каждая из следующих двух процедур преобразует дерево в список.

    (define (tree->list-1 tree)
      (if (null? tree)
         '()
          (append (tree->list-1 (left-branch tree))
                  (cons (entry tree)
                        (tree->list-1 (right-branch tree))))))

    (define (tree->list-2 tree)
      (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
      (copy-to-list tree '()))

  а. Для всякого ли дерева эти процедуры дают одинаковый результат? Если нет, то как их результаты
  различаются? Какой результат дают эти две процедуры для деревьев с рисунка 2.16?

  б. Одинаков ли порядок роста этих процедур по отношению к числу шагов, требуемых для преобразования
  сбалансированного дерева с n элементами в список? Если нет, которая из них растет медленнее?

  Рис. 2.16. Различные бинарные деревья, представляющие множество {1, 3, 5, 7, 9, 11}.

         7                  3                  5
        / \                / \                / \
       3   9              1   7              3   9
      / \   \                / \            /   / \
     1   5   11             5   9          1   7   11
                                 \
                                  11
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

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
     '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))

  (copy-to-list tree '()))

(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
(define tree4 '(5 (4 (3 (2 (1 () ()) ()) ()) ()) ()))
(define tree5 '(10 (9 () ()) (11 () (14 (12 () ()) (15 () ())))))

(define list1 '(1 3 5 7 9 11))
(define list2 '(1 2 3 4 5))
(define list3 '(9 10 11 12 14 15))

(check-equal? (tree->list-1 tree1) list1)
(check-equal? (tree->list-2 tree1) list1)
(check-equal? (tree->list-1 tree2) list1)
(check-equal? (tree->list-2 tree2) list1)
(check-equal? (tree->list-1 tree3) list1)
(check-equal? (tree->list-2 tree3) list1)
(check-equal? (tree->list-1 tree4) list2)
(check-equal? (tree->list-2 tree4) list2)
(check-equal? (tree->list-1 tree5) list3)
(check-equal? (tree->list-2 tree5) list3)

#|
  Эти процедуры дают одинаковый результат. Но в них по разному организована сборка списка, первая
  процедура использует для этого append, вторая использует аккумулятор.

  Процедура tree->list-1 на каждом шаге (здесь и далее имеется в виду, что в среднем) вызывается дважды
  для сбалансированного дерева глубины n.

    T(n) = 2T(n - 1) + O(2ⁿ⁻¹ - 1), где O(2ⁿ⁻¹) - оценка времени затрачиваемого на append двух списков
    величины 2ⁿ⁻¹ - 1.

    T(n) = 2(2T(n - 2) + O(2ⁿ⁻²)) + O(2ⁿ⁻¹) =

    = 2ⁿT(0) + 2ⁿ⁻¹O(1) + 2ⁿ⁻²O(2) + ... + O(2ⁿ⁻¹) ~ O(n * 2ⁿ⁻¹)

  Между тем, n - это не количество узлов, а глубина дерева. Количество узлов k в сбалансированном дереве
  глубины n можно оценить как 2ⁿ - 1:

    k = 2ⁿ - 1 ⇒ k + 1 = 2ⁿ ⇒ log₂(k + 1) = n

    T(k) = O(log₂(k + 1) * (k + 1)/2) ~ O(k * log₂k)

  Процедура tree->list-2 обходит все вершины, но использует аккумулятор подпроцедуры, а не append.
  Соответственно, получаем такую оценку:

    T(k) = 2T(k/2) + O(1) = 2(2T(k/4) + O(1)) + O(1) =

    = 2^{log₂k}*O(1) + ... + O(1) = k*O(1) ~ O(k)

  Следовательно, tree->list-2 растёт медленнее, чем tree->list-1.
|#
