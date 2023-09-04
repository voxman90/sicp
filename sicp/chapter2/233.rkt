#lang sicp

#|
  Упражнение 2.33

  Заполните пропущенные выражения, так, чтобы получились определения некоторых базовых операций по работе
  со списками в виде накопления:

    (define (map p sequence)
      (accumulate (lambda (x y) <??>) nil sequence))

    (define (append seq1 seq2)
      (accumulate cons <??> <??>))

    (define (length sequence)
      (accumulate <??> 0 sequence))
|#

(#%require rackunit)

(define nil '())

(define (inc x) (+ x 1))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

(check-equal? (map inc '(-5 7 5)) '(-4 8 6))
(check-equal? (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-equal? (append '(1 2 3) '()) '(1 2 3))
(check-equal? (append '() '(1 2 3)) '(1 2 3))
(check-equal? (append '() '()) '())
(check-equal? (length '()) 0)
(check-equal? (length '(1 2 3 4)) 4)
