#lang sicp

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (get-part x percent)
  (* (/ x 100)
     percent))

(define (good-enough? guess improved-guess)
  (< (abs (- improved-guess guess))
     (get-part guess 0.01)))


(define (sqrt-iter guess improved-guess x)
  (if (good-enough? guess improved-guess)
      improved-guess
      (sqrt-iter improved-guess (improve improved-guess x) x)))

(define (sqrt x)
  (sqrt-iter 1 (improve 1 x) x))

(define (good-enough-alt? guess x)
  (< (abs (- (square guess) x))
     0.01))

(define (sqrt-iter-alt guess x)
  (if (good-enough-alt? guess x)
      guess
      (sqrt-iter-alt (improve guess x) x)))

(define (sqrt-alt x)
  (sqrt-iter-alt 1 x))

(define (get-difference-with-approximation x approx-func)
  (abs (- (square (approx-func x)) x)))

(define (sign x)
  (if (< x 0)
      -1
      (if (> x 0)
          1
          0)))

(define (who-is-closer x approx-func-1 approx-func-2)
  (define diff-1 (get-difference-with-approximation x approx-func-1))
  (define diff-2 (get-difference-with-approximation x approx-func-2))
  (case (sign (- diff-2 diff-1))
        (1 approx-func-1)
        (-1 approx-func-2)
        (0 "equal")))

(define (displayln str)
  (display str)
  (newline))

(define (display-result open-str close-str var result)
  (display open-str)
  (display var)
  (display close-str)
  (displayln result))

(define (display-better-sqrt-approx x)
  (define sqrt-result (sqrt x))
  (define sqrt-alt-result (sqrt-alt x))
  (display-result "(sqrt " ") => " x (exact->inexact sqrt-result))
  (display-result "(sqrt-alt " ") => " x (exact->inexact sqrt-alt-result))
  (display-result "(who-is-closer " ") => " x (who-is-closer x sqrt-alt sqrt)))

(display-better-sqrt-approx 1)
(display-better-sqrt-approx 16)
(display-better-sqrt-approx 100)
(display-better-sqrt-approx 10000)
(display-better-sqrt-approx 0.01)
(display-better-sqrt-approx 0.0001)

#|
  Функция sqrt-alt полноценно не работает для достаточно маленьких чисел (меньших погрешности).
  Функция sqrt учитывет этот недостаток и хорошо справляется с маленькими числами.
  Тесты показывают равный результат для больших чисел и я пока не понимаю, связано ли это с
  особенностями представления чисел в языке или чем-то ещё. Я вернусь к этой части вопроса, когда
  узнаю об этом больше.
  По логике вещей, погрешность для достаточно больших чисел у функции sqrt-alt должна давать более
  приближенный результат, вопрос только в том, насколько точны вычесления в lisp для таких чисел.
  В JS наблюдается такое поведение, что число n выше MAX_SAFE_INTEGER, может быть n === n + 1.
  Тут может быть такая же проблема.
|#
