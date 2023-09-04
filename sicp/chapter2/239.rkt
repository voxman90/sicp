#lang sicp

#|
  Упражнение 2.39

  Закончите следующие определения reverse (упражнение 2.18) в терминах процедур fold-right и fold-left
  из упражнения 2.38:

    (define (reverse-right sequence)
      (fold-right (lambda (x y) <??>) nil sequence))

    (define (reverse-left sequence)
      (fold-left (lambda (x y) <??>) nil sequence))
|#

(#%require rackunit)

(define nil '())

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))

  (iter initial sequence))

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))

  (iter initial sequence))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (cons x y)) nil sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(check-equal? (reverse-right '()) '())
(check-equal? (reverse-left '()) '())
(check-equal? (reverse-right '(1 2 3)) '(3 2 1))
(check-equal? (reverse-left '(1 2 3)) '(3 2 1))
(check-equal? (reverse-left '((1 2) 3 ())) '(() 3 (1 2)))
(check-equal? (reverse-right '((1 2) 3 ())) '(() 3 (1 2)))
