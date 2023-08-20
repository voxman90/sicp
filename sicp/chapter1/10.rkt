#lang sicp

(#%require rackunit)

(define (Akkerman x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (Akkerman (- x 1)
                        (Akkerman x (- y 1))))))

(check-equal? (Akkerman 1 10) 1024)
(check-equal? (Akkerman 2 4) 65536)
(check-equal? (Akkerman 3 3) 65536)

(define (f n) (Akkerman 0 n))

#|
  Проведура f удваивает число n:
  f(n) = 2n
|#

(check-equal? (f 10) 20)
(check-equal? (f 0) 0)
(check-equal? (f -10) -20)

(define (g n) (Akkerman 1 n))

#|
  (g 1) => (A 1 1) => 2

  (g 2) =>
  (A 1 2) =>
  (A 0 (A 1 1)) =>
  (A 0 2) =>
  4

  (g 4) =>
  (A 1 4) =>
  (A 0 (A 1 3)) =>
  (A 0 (A 0 (A 1 2))) =>
  (A 0 (A 0 (A 0 (A 1 1)))) =>
  (A 0 (A 0 (A 0 2))) =>
  (A 0 (A 0 4)) =>
  (A 0 8) =>
  16
|#

(define (^ base degree)
  (cond ((= base 1) 1)
        ((< degree 1) 1)
        (else (* base
                 (^ base (dec degree))))))

(check-equal? (g 0) 0)
(check-equal? (g 1) 2)
(check-equal? (g 5) (^ 2 5))
(check-equal? (g 10) (^ 2 10))
(check-equal? (g 100) (^ 2 100))

#|
  g(n) = 2^n | n > 0,
  g(0) = 0

  Если n < 0, то процедура g(n) зацикливается и не возвращает результат.
|#

(define (h n) (Akkerman 2 n))

#|
  (h 3) =>
  (A 2 3) =>
  (A 1 (A 2 2)) =>
  (A 1 (A 1 (A 2 1)) =>
  (A 1 (A 1 2)) =>
  { Мы уже знаем, как ведёт себя (A 1 n), поэтому подставим результат }
  (A 1 2^{2}) =>
  2^{2^{2}}

  (h 5) =>
  (A 2 5) =>
  (A 1 (A 2 4)) =>
  (A 1 (A 1 (A 2 3))) =>
  (A 1 (A 1 (A 1 (A 2 2)))) =>
  (A 1 (A 1 (A 1 (A 1 (A 2 1))))) =>
  (A 1 (A 1 (A 1 (A 1 2)))) =>
  (A 1 (A 1 (A 1 2^{2}))) =>
  (A 1 (A 1 2^{2^{2}})) =>
  (A 1 2^{2^{2^{2}}}) =>
  2^{2^{2^{2^{2}}}}

  Стало быть, процедура h возвращает 2 возведённую во вторую степень, которая
  возведена во вторую степень и так (n - 1) раз.
  h(n) = 2^{2^{2^{...^{2^{2}}}}}
            -------------------
                n - 1 раз
  h(0) = 0, h(1) = 2
  Если n < 0, то процедура зацикливается и не возвращает значение.
|#

(check-equal? (h 0) 0)
(check-equal? (h 1) 2)
(check-equal? (h 2) (^ 2 2))
(check-equal? (h 3) (^ 2 (^ 2 2)))
(check-equal? (h 4) (^ 2 (^ 2 (^ 2 2))))
(check-equal? (h 5) (^ 2 (^ 2 (^ 2 (^ 2 2)))))
