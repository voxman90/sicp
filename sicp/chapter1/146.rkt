#lang sicp

#|
  Упражнение 1.46

  Некоторые из вычислительных методов, описанных в этой главе, являются примерами чрезвычайно общей
  вычислительной стратегии, называемой пошаговое улучшение (iterative improvement). Пошаговое
  улучшение состоит в следующем: чтобы что-то вычислить, нужно взять какое-то начальное значение,
  проверить, достаточно ли оно хорошо, чтобы служить ответом, и если нет, то улучшить это значение
  и продолжить процесс с новым значением. Напишите процедуру iterative-improve, которая принимает
  в качестве аргументов две процедуры: проверку, достаточно ли хорошо значение, и метод улучшения
  значения. Iterative-improve должна возвращать процедуру, которая принимает начальное значение в
  качестве аргумента и улучшает его, пока оно не станет достаточно хорошим. Перепишите процедуру sqrt
  из раздела 1.1.7 и процедуру fixed-point из раздела 1.3.3 в терминах iterative-improve.
|#

(#%require rackunit)

(define (iterative-improve close-enough? improve)
  (define (loop guess)
    (let ((improved-guess (improve guess)))
      (if (close-enough? guess improved-guess)
          improved-guess
          (loop improved-guess))))

  (lambda (initial-guess) (loop initial-guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define tolerance 0.001)

  (let ((initial-guess 1.0)
        (close-enough? (lambda (guess improved-guess)
          (< (abs (- (square guess) (square improved-guess))) (* tolerance guess))))
        (improve (lambda (guess)
          (average guess (/ x guess)))))
    ((iterative-improve close-enough? improve) initial-guess)))

(define (square x) (* x x))

(define (fixed-point func initial-guess)
  (define tolerance 0.00001)

  (let ((close-enough? (lambda (guess improved-guess)
          (< (abs (- guess improved-guess)) tolerance)))
        (improve (lambda (guess) (func guess))))
    ((iterative-improve close-enough? improve) initial-guess)))

(define (cube-root x)
    (define dx 0.00001)
    (define (close-enough? x1 x2)
      (< (abs (- x1 x2)) dx))
    (define (improve guess)
      (/ (+ (* 2 guess) (/ x (square guess))) 3))
    ((iterative-improve close-enough? improve) 1.0))

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define (sqrt-fixed-point y) (fixed-point (lambda (x) (/ (+ x (/ y x)) 2)) 1.0))

(check-equal? (round (sqrt 81)) 9.0)
(check-equal? (round (sqrt 10000)) 100.0)
(check-equal? (round (* 100000 golden-ratio)) (round (* 100000 (/ (+ (sqrt 5) 1) 2))))
(check-equal? (round (sqrt-fixed-point 169)) 13.0)
(check-equal? (round (cube-root 27)) 3.0)
(check-equal? (round (cube-root 64)) 4.0)
