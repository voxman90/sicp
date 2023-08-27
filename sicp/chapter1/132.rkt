#lang sicp

#|
  Упражнение 1.32

  а. Покажите, что sum и product (упражнение 1.31) являются частными случаями еще более общего
  понятия, называемого накопление (accumulation), которое комбинирует множество термов с помощью
  некоторой общей функции накопления

    (accumulate combiner null-value term a next b)

  Accumulate принимает в качестве аргументов те же описания термов и диапазона, что и sum с product,
  а еще процедуру combiner (двух аргументов), которая указывает, как нужно присоединить текущий терм
  к результату накопления предыдущих, и null-value, базовое значение, которое нужно использовать,
  когда термы закончатся. Напишите accumulate и покажите, как и sum, и product можно определить в
  виде простых вызовов accumulate.

  б. Если Ваша процедура accumulate порождает рекурсивный процесс, перепишите ее так, чтобы она
  порождала итеративный. Если она порождает итеративный процесс, перепишите ее так, чтобы она
  порождала рекурсивный.
|#

(#%require rackunit)

(define (accumulate-rec combiner null-value term a next b)
  (define (accumulate a)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate (next a)))))
  (accumulate a))

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter acc a)
    (if (> a b)
        acc
        (accumulate-iter (combiner acc (term a)) (next a))))
  (accumulate-iter null-value a))

(define (inc n) (+ n 1))

(define (square x) (* x x))

(define (identity x) x)

(check-equal? (accumulate * 1 square 3 inc 1) 1)
(check-equal? (accumulate * 1 square 1 inc 3) 36)
(check-equal? (accumulate * 1 identity 3 inc 5) 60)
(check-equal? (accumulate + 0 identity 1 inc 10) 55)

(check-equal? (accumulate-rec * 1 square 3 inc 1) 1)
(check-equal? (accumulate-rec * 1 square 1 inc 3) 36)
(check-equal? (accumulate-rec * 1 identity 3 inc 5) 60)
(check-equal? (accumulate-rec + 0 identity 1 inc 10) 55)
