#lang racket

#|
  Упражнение 3.78

                         𝑑𝑦₀                 𝑦₀
                          ⇣                   ⇣
           d²y      ┏━━━━━━━━━━━┓  dy   ┏━━━━━━━━━━━┓   y
    ┌──────────────►┃ integral  ┠───·───┨ integral  ┠───·───►
    │               ┗━━━━━━━━━━━┛   │   ┗━━━━━━━━━━━┛   │
    │                  ┏━━━━━━━━━━┓ │                   │
    │               ┌──┨ scale: a ┠◄┘                   │
    │ ┌───────────┒◄┘  ┗━━━━━━━━━━┛                     │
    └─┤ add       ┃                                     │
      └───────────┚◄┐  ┏━━━━━━━━━━┓                     │
                    └──┨ scale: b ┠◄────────────────────┘
                       ┗━━━━━━━━━━┛

  Рис. 3.35. Диаграмма потока сигналов для решения линейного дифференциального уравнения второго порядка.

  Рассмотрим задачу проектирования системы обработки сигналов для решения гомогенных линейных дифференциальных
  уравнений второго порядка

    𝑑²𝑦/𝑑𝑡² − 𝑎𝑑𝑦/𝑑𝑡 − 𝑏𝑦 = 0

  Выходной поток, моделирующий 𝑦, порождается сетью, содержащей цикл. Этот цикл возникает потому, что
  значение 𝑑²𝑦/𝑑𝑡² зависит от значений 𝑦 и 𝑑𝑦/𝑑𝑡, а они оба получаются интегрированием 𝑑²𝑦/𝑑𝑡². Диаграмма,
  которую нам хотелось бы закодировать, показана на рис. 3.35. Напишите процедуру solve-2nd, которая в
  качестве аргументов берет константы 𝑎, 𝑏 и 𝑑𝑡 и начальные значения 𝑦₀ и 𝑑𝑦₀ для 𝑦 и 𝑑𝑦/𝑑𝑡, и порождает
  поток последовательных значений 𝑦.
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (stream-null? stream) (stream-empty? stream))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (stream-cons
        (apply proc (map stream-car streams))
        (apply stream-map
               (cons proc (map stream-cdr streams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integral delayed-integrand initial-value dt)
  (stream-cons
    initial-value
    (let ((integrand (force delayed-integrand)))
      (if (stream-null? integrand)
          the-empty-stream
          (integral (delay (stream-cdr integrand))
                    (+ (* dt (stream-car integrand))
                        initial-value)
                    dt)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream y b)
                           (scale-stream dy a)))
  y)

(check-equal? (stream-ref (solve-2nd 1 0 1 1 0.001) 1000) 2.716923932235896)
