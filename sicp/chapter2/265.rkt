#lang sicp

#|
  Упражнение 2.65

  Используя результаты упражнений 2.63 и 2.64, постройте реализации порядка Θ(n) union-set и
  intersection-set для множеств, реализованных как (сбалансированные) бинарные деревья.
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

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))

  (copy-to-list tree '()))

(define (union-list list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else
          (let ((x1 (car list1)) (x2 (car list2)))
            (cond ((= x1 x2)
                   (cons x1 (union-list (cdr list1) (cdr list2))))
                  ((< x1 x2)
                   (cons x1 (union-list (cdr list1) list2)))
                  ((< x2 x1)
                   (cons x2 (union-list list1 (cdr list2)))))))))

(define (intersection-list list1 list2)
  (if (or (null? list1) (null? list2))
     '()
      (let ((x1 (car list1)) (x2 (car list2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list (cdr list1)
                                           (cdr list2))))
              ((< x1 x2)
               (intersection-list (cdr list1) list2))
              ((< x2 x1)
               (intersection-list list1 (cdr list2)))))))

(define (union-set tree1 tree2)
  (list->tree (union-list (tree->list tree1)
                          (tree->list tree2))))

(define (intersection-set tree1 tree2)
  (list->tree (intersection-list (tree->list tree1)
                                 (tree->list tree2))))

(define first (list->tree '(1 2 3 5)))
(define second (list->tree '(3 4 5 6)))

(check-equal? (tree->list (intersection-set first second)) '(3 5))
(check-equal? (tree->list (union-set first second)) '(1 2 3 4 5 6))
(check-equal? (tree->list (intersection-set '() second)) '())
(check-equal? (union-set '() second) second)
(check-equal? (tree->list (intersection-set first '())) '())
(check-equal? (union-set first '()) first)
