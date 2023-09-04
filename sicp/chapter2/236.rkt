#lang sicp

#|
  Упражнение 2.36

  Процедура accumulate-n подобна accumulate, только свой третий аргумент она воспринимает как последов-
  ательность последовательностей, причем предполагается, что все они содержат одинаковое количество
  элементов. Она применяет указанную процедуру накопления ко всем первым элементам последовательностей,
  вторым элементам последовательностей и так далее, и возвращает последовательность результатов.
  Например, если s есть последовательность, состоящая из четырех последовательностей, ((1 2 3) (4 5 6)
  (7 8 9) (10 11 12)), то значением (accumulate-n + 0 s) будет последовательность (22 26 30). Заполните
  пробелы в следующем определении accumulate-n:

    (define (accumulate-n op init seqs)
      (if (null? (car seqs))
          nil
          (cons (accumulate op init <??>)
                (accumulate-n op init <??>))))
|#

(#%require rackunit)

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial sequence)
  (if (null? (car sequence))
      nil
      (cons (accumulate op initial (map car sequence))
            (accumulate-n op initial (map cdr sequence)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(check-equal? (accumulate-n + 0 '((1 2 3))) '(1 2 3))
(check-equal? (accumulate-n + 0 '((1 2) (-1 -2))) '(0 0))
(check-equal? (accumulate-n + 0 s) '(22 26 30))
