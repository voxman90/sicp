#lang sicp

#|
  Упражнение 2.38

  Процедура accumulate известна также как fold-right (правая свертка), поскольку она комбинирует первый
  элемент последовательности с результатом комбинирования всех элементов справа от него. Существует
  также процедура fold-left (левая свертка), которая подобна fold-right, но комбинирует элементы в
  противоположном направлении:

    (define (fold-left op initial sequence)
      (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
      (iter initial sequence))

  Каковы значения следующих выражений?

    (fold-right / 1 (list 1 2 3))

    (fold-left / 1 (list 1 2 3))

    (fold-right list nil (list 1 2 3))

    (fold-left list nil (list 1 2 3))

  Укажите свойство, которому должна удовлетворять op, чтобы для любой последовательности fold-right и
  fold-left давали одинаковые результаты.
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
  (define (rec rest)
    (if (null? rest)
        initial
        (op (car rest) (rec (cdr rest)))))

  (rec sequence))

(check-equal? (fold-right / 1 (list 1 2 3)) 3/2)
(check-equal? (fold-left / 1 (list 1 2 3)) 1/6)
(check-equal? (fold-right list nil (list 1 2 3)) '(1 (2 (3 ()))))
(check-equal? (fold-left list nil (list 1 2 3)) '(((() 1) 2) 3))

#|
  Для того, чтобы для любой последовательности выдавать одинаковые результаты, op должна обладать
  свойством ассоциативности (op (op a b) c) = (op a (op b c)), коммутативности (op a b) = (op b a) и
  для операции должен существовать нейтральный элемент (это не так критично, но без этого для множества
  операций придётся задавать начальное значение как одно из значений представленных в списке, например,
  для операции поиска минимального или максимального результата):
|#

(check-equal? (fold-right * 1 (list 2 8 12)) (fold-left * 1 (list 2 8 12)))
(check-equal? (fold-right max 2 (list 2 8 12)) (fold-left max 2 (list 2 8 12)))
(check-equal? (fold-right min 12 (list 2 8 12)) (fold-left min 12 (list 2 8 12)))
