#lang lazy

#|
  Провёл тестирование количества вызовов remainder при нормальном порядке вычислений.
  Пришлось использовать промисы с отложенной обработкой (call-by-name). По умолчанию в lazy
  режиме работает call-by-need обработка с мемоизацией.

  Полученные результаты согласуются с формулой подсчёта вызовов из упражнения 1.20.
|#

(#%require racket/promise)

(define (gcd-steps a b steps)
  (if (= b 0)
      (+ steps 1)
      (gcd-steps b (remainder a b) (+ steps 1))))

(define (gcd-remainder-calls a b)
  (define counter 0)
  (define (remainder-spy a b)
    (delay/name
      (begin
        (set! counter (+ counter 1))
        (remainder (force a)
                   (force b)))))
  (define (gcd-remainder a b)
      (if (= (force b) 0)
          a
          (gcd-remainder b (remainder-spy a b))))
  (force (gcd-remainder a b))
  counter)

(gcd-steps 1 0 0)    ; 1
(gcd-steps 0 1 0)    ; 2
(gcd-steps 8 2 0)    ; 2
(gcd-steps 11 3 0)   ; 4
(gcd-steps 206 40 0) ; 5

(gcd-remainder-calls 1 0)    ; 0
(gcd-remainder-calls 0 1)    ; 1
(gcd-remainder-calls 8 2)    ; 1
(gcd-remainder-calls 11 3)   ; 9
(gcd-remainder-calls 206 40) ; 18
