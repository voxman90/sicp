#lang sicp

#|
  Упражнение 2.41

  Напишите процедуру, которая находит все такие упорядоченные тройки различных положительных целых
  чисел i, j и k, меньших или равных данному целому числу n, сумма которых равна данному числу s.
|#

(#%require rackunit)

(define (accumulate prop initial sequence)
  (define (iter acc sub-sequence)
    (if (null? sub-sequence)
        acc
        (iter (prop acc (car sub-sequence)) (cdr sub-sequence))))

  (iter initial sequence))

(define (filter predicate? sequence)
  (define (rec sub-sequence)
    (cond ((null? sub-sequence) '())
          ((predicate? (car sub-sequence))
           (cons (car sub-sequence) (rec (cdr sub-sequence))))
          (else (rec (cdr sub-sequence)))))

  (rec sequence))

(define (flatmap callback sequence)
  (accumulate append '() (map callback sequence)))

(define (enumerate-interval start end)
  (if (< end start)
     '()
      (cons start (enumerate-interval (+ start 1) end))))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
    (enumerate-interval 2 n)))

(define (unique-triples n)
  (flatmap
    (lambda (k)
      (map (lambda (pair) (cons k pair))
           (unique-pairs (- k 1))))
    (enumerate-interval 3 n)))

(define (make-triple-sum n s)
  (define (triple-sum triple)
    (+ (car triple)
       (cadr triple)
       (caddr triple)))

  (define (equal-to-s? triple)
    (= s (triple-sum triple)))

  (filter
    equal-to-s?
    (unique-triples n)))

(check-equal? (unique-triples 4) '((3 2 1) (4 2 1) (4 3 1) (4 3 2)))
(check-equal? (make-triple-sum 4 6) '((3 2 1)))
(check-equal? (make-triple-sum 3 6) '((3 2 1)))
(check-equal? (length (make-triple-sum 9 9)) 3)
