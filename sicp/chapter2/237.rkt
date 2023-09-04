#lang sicp

#|
  Упражнение 2.37

  Предположим, что мы представляем векторы v = (vᵢ) как последовательности чисел, а матрицы m = (mᵢⱼ)
  как последовательности векторов (рядов матрицы). Например, матрица

     ┏            ┓
     ┃ 1  2  3  4 ┃
     ┃ 4  5  6  6 ┃
     ┃ 6  7  8  9 ┃
     ┗            ┛

  представляется в виде последовательности ((1 2 3 4) (4 5 6 6) (6 7 8 9)). Имея такое представление,
  мы можем использовать операции над последовательностями, чтобы кратко выразить основные действия над
  матрицами и векторами. Эти операции (описанные в любой книге по матричной алгебре) следующие:

  Скалярное произведение (dot-product v w) возвращает сумму ∑ᵢvᵢwᵢ

  Произведение матрицы и вектора (matrix-*-vector m v) возвращает вектор t, где tᵢ = ∑ⱼmᵢⱼvᵢ

  Произведение матриц (matrix-*-matrix m n) возвращает матрицу p, где pᵢⱼ = ∑ₖmᵢₖnₖⱼ

  Транспозиция (transpose m) возвращает матрицу n, где nᵢⱼ = mⱼᵢ

  Скалярное произведение мы можем определить так:

    (define (dot-product v w)
      (accumulate + 0 (map * v w)))

  Заполните пропуски в следующих процедурах для вычисления остальных матричных операций. (Процедура
  accumulate-n описана в упражнении 2.36.)

    (define (matrix-*-vector m v)
      (map <??> m))

    (define (transpose mat)
      (accumulate-n <??> <??> mat))

    (define (matrix-*-matrix m n)
      (let ((cols (transpose n)))
        (map <??> m)))
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

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mv) (dot-product mv v)) m))

(define (transpose matrix)
  (accumulate-n cons nil matrix))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mv)
           (map (lambda (nv) (dot-product nv mv)) cols))
         m)))

(define matrix '((1 2)
                 (3 4)
                 (5 6)))
(define matrix-tr '((1 3 5)
                    (2 4 6)))
(define matrix2 '((1 6 0)
                  (1 1 0)
                  (0 2 1)))
(define matrix2-inv '((-2/10 12/10 0)
                      (2/10 -2/10 0)
                      (-4/10 4/10 1)))
(define vector1 '(1 2))
(define vector2 '(1 1 1))
(define vector3 '(0 5 25))

(check-equal? (dot-product vector2 vector3) 30)
(check-equal? (matrix-*-vector matrix-tr vector2) '(9 12))
(check-equal? (matrix-*-vector matrix vector1) '(5 11 17))
(check-equal? (transpose matrix) matrix-tr)
(check-equal? (transpose matrix-tr) matrix)
(check-equal? (matrix-*-matrix matrix2 matrix2-inv) '((1 0 0)
                                                      (0 1 0)
                                                      (0 0 1)))
