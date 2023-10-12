#lang racket

#|
  Упражнение 3.80

  Последовательная 𝑅𝐿𝐶-цепь (series RLC circuit) состоит из резистора, конденсатора и катушки индуктивности,
  соединенных последовательно, как показано на рис. 3.36. Если сопротивление, индуктивность и емкость
  равны, соответственно, 𝑅, 𝐿 и 𝐶, то отношения между напряжением 𝑣 и током 𝑖 на трех элементах описываются
  уравнениями

    𝑣ᵣ = 𝑖ᵣ𝑅
    𝑣ₗ = 𝐿𝑑𝑖ₗ/𝑑𝑡
    𝑖꜀ = 𝐶𝑑𝑣꜀/𝑑𝑡

  а цепь диктует соотношения

    𝑖ᵣ = 𝑖ₗ = −𝑖꜀
    𝑣꜀ = 𝑣ₗ + 𝑣ᵣ

  Сочетание этих условий показывает, что состояние цепи (характеризуемое через 𝑣꜀, напряжение на конденсаторе,
  и 𝑖ₗ, ток через катушку) описывается парой дифференциальных уравнений

    𝑑𝑣꜀/𝑑𝑡 = −𝑖꜀/𝐶
    𝑑𝑖ₗ/𝑑𝑡 = 𝑣꜀/𝐿 − 𝑖ₗ𝑅/𝐿

  Диаграмма потока сигналов, представляющая эту систему дифференциальных уравнений, показана на рисунке
  3.37.

                      ┏━━━━━━━━━━━━┓
    ┌─────────────────┨ scale:−1/C ┠◄─┐
    │                 ┗━━━━━━━━━━━━┛  │
    │                 ┏━━━━━━━━━━┓    │ 𝑣꜀
    │              ┌─►┃ integral ┠────·───►
    │              │  ┗━━━━━━━━━━┛
    │              │𝑑𝑣꜀     ⇡𝑣꜀₀
    │              │  ┏━━━━━━━━━━━━┓
    │              └──┨ scale:−1/C ┠◄─┐
    │                 ┗━━━━━━━━━━━━┛  │
    └►┎─────────┐ d𝑖ₗ  ┏━━━━━━━━━━┓    │ 𝑖ₗ
      ┃ add     ├────►┃ integral ┠────·───►
    ┌►┖─────────┘     ┗━━━━━━━━━━┛    │
    │                       ⇡𝑖ₗ₀       │
    │                 ┏━━━━━━━━━━━━┓  │
    └─────────────────┨ scale:−R/L ┠◄─┘
                      ┗━━━━━━━━━━━━┛

  Рис. 3.37. Диаграмма потока сигналов для решения уравнений последовательной 𝑅𝐿𝐶–цепи.

  Напишите процедуру 𝑅𝐿𝐶, которая в качестве аргументов берет параметры цепи 𝑅, 𝐿 и 𝐶 и точность по
  времени 𝑑𝑡. Подобно процедуре 𝑅𝐶 из упражнения 3.73, 𝑅𝐿𝐶 должна порождать процедуру, которая берет
  начальные значения переменных состояния 𝑣꜀₀ и 𝑖ₗ₀ и порождает (через cons) пару потоков состояния 𝑣꜀ и
  𝑖ₗ. 𝐶 помощью 𝑅𝐿𝐶 породите пару потоков, которая моделирует поведение 𝑅𝐿𝐶-цепи c 𝑅 = 1 ом, 𝐶 = 0.2
  фарад, 𝐿 = 1 генри, 𝑑𝑡 = 0.1 секунды, и начальными значениями 𝑖ₗ₀ = 0 ампер и 𝑣꜀₀ = 10 вольт.
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (stream-null? stream) (stream-empty? stream))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (stream-cons
        (apply proc (map stream-car streams))
        (apply stream-map
               (cons proc (map stream-cdr streams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

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

(define (RLC R L C dt)
  (define -R/L (- (/ R L)))
  (define -1/C (- (/ 1 C)))
  (define 1/L (/ 1 L))

  (lambda (iL0 vC0)
    (define iL (integral (delay diL) iL0 dt))
    (define vC (integral (delay dcL) vC0 dt))
    (define dcL (scale-stream iL -1/C))
    (define diL (add-streams (scale-stream iL -R/L)
                             (scale-stream vC 1/L)))
    (cons iL vC)))

(define RLC1 ((RLC 1 1 0.2 0.1) 0 10.0))

(define current-stream (stream-map (lambda (x) (/ (round (* 100 x)) 100)) (car RLC1)))
(define voltage-stream (stream-map (lambda (x) (/ (round (* 100 x)) 100)) (cdr RLC1)))

(define output1 (open-output-string))

(display-stream current-stream output1 30)

(check-equal? (get-output-string output1)
  "\n0\n1.0\n1.9\n2.66\n3.25\n3.65\n3.84\n3.83\n3.64\n3.27\n2.75\n2.12\n1.42\n0.69\n-0.05\n-0.75\n-1.37\
\n-1.89\n-2.3\n-2.56\n-2.69\n-2.68\n-2.53\n-2.26\n-1.9\n-1.45\n-0.96\n-0.44\n0.07\n0.55")

(close-output-port output1)

(define output2 (open-output-string))

(display-stream voltage-stream output2 100)

(check-equal? (get-output-string output2)
  "\n10.0\n10.0\n9.5\n8.55\n7.22\n5.6\n3.77\n1.85\n-0.07\n-1.88\n-3.52\n-4.89\n-5.95\n-6.66\n-7.01\
\n-6.98\n-6.61\n-5.92\n-4.98\n-3.83\n-2.55\n-1.2\n0.14\n1.4\n2.53\n3.48\n4.21\n4.69\n4.91\n4.87\n4.6\
\n4.1\n3.43\n2.62\n1.72\n0.78\n-0.16\n-1.04\n-1.82\n-2.48\n-2.97\n-3.3\n-3.44\n-3.4\n-3.2\n-2.84\
\n-2.36\n-1.79\n-1.16\n-0.5\n0.16\n0.77\n1.31\n1.76\n2.1\n2.32\n2.41\n2.37\n2.22\n1.97\n1.63\n1.22\
\n0.78\n0.32\n-0.14\n-0.56\n-0.94\n-1.25\n-1.48\n-1.63\n-1.69\n-1.66\n-1.54\n-1.36\n-1.12\n-0.83\
\n-0.52\n-0.2\n0.12\n0.41\n0.67\n0.89\n1.05\n1.14\n1.18\n1.15\n1.07\n0.94\n0.77\n0.57\n0.35\n0.12\
\n-0.1\n-0.3\n-0.48\n-0.63\n-0.74\n-0.8\n-0.83\n-0.81")

(close-output-port output2)
