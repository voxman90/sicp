#lang sicp

#|
  Упражнение 2.62

  Дайте представление порядка Θ(n) для операции union-set с представлением в виде упорядоченных списков.
|#

(#%require rackunit)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                  ((< x2 x1)
                  (cons x2 (union-set set1 (cdr set2)))))))))


(define x '(1 3 5 8))
(define y '(0 1 2 4 9))
(define z '(0 1 2 3 4 5 8 9))

(check-equal? (union-set x y) z)
(check-equal? (union-set x z) z)
(check-equal? (union-set z y) z)
(check-equal? (union-set x z) (union-set y x))
(check-equal? (union-set x y) (union-set y x))
(check-equal? (union-set '() y) y)
(check-equal? (union-set x '()) x)
(check-equal? (union-set x x) x)
