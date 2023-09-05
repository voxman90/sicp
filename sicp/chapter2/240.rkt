#lang sicp

#|
  Упражнение 2.40

  Определите процедуру unique-pairs, которая, получая целое число n, порождает последовательность пар
  (i,j), таких, что 1 ≤ j < i ≤ n. С помощью unique-pairs упростите данное выше определение
  prime-sum-pairs.
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

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (flatmap callback sequence)
  (accumulate append nil (map callback sequence)))

(define (square x) (* x x))

(define (divides? div x)
  (= (remainder x div) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

(define (prime? n)
  (if (divides? 2 n)
    (= 2 n)
    (= n (find-divisor n 3))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (enumerate-interval start end)
  (if (> start end)
     '()
      (cons start
            (enumerate-interval (+ start 1) end))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

(check-equal? (prime-sum-pairs 1) '())
(check-equal? (prime-sum-pairs 5) '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7)))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(check-equal? (unique-pairs 1) '())
(check-equal? (unique-pairs 2) '((2 1)))
(check-equal? (unique-pairs 3) '((2 1) (3 1) (3 2)))
(check-equal? (length (unique-pairs 2)) 1)
(check-equal? (length (unique-pairs 3)) 3)
(check-equal? (length (unique-pairs 6)) 15)

(define (prime-sum-pairs-alt n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(check-equal? (prime-sum-pairs-alt 1) '())
(check-equal? (prime-sum-pairs-alt 5) '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7)))
