#lang racket

#|
  Упражнение 3.59

  В разделе 2.5.3 мы увидели, как реализовать систему арифметики многочленов, используя представление
  многочленов в виде списка термов. Подобным же образом можно работать со степенными рядами (power series),
  например

    eˣ = 1 + 𝑥 + 𝑥²/2 + 𝑥³/(2·3) + 𝑥⁴/(2·3·4) + ···,

    cos 𝑥 = 1 - 𝑥²/2 + 𝑥⁴/(2·3·4) - ···,

    sin 𝑥 = 𝑥 - 𝑥³/(2·3) + 𝑥⁵/(2·3·4·5) - ···,

  представленными в виде бесконечных потоков. Будем представлять последовательность 𝑎₀ + 𝑎₁𝑥 + 𝑎₂𝑥² +
  𝑎₃𝑥³ + ··· как поток, элементами которого являются коэффициенты 𝑎₀, 𝑎₁, 𝑎₂, 𝑎₃, ... .

  а. Интеграл последовательности 𝑎₀ + 𝑎₁𝑥 + 𝑎₂𝑥² + 𝑎₃𝑥³ + ··· есть последовательность

    𝑐 + 𝑎₀𝑥 + 𝑎₁𝑥²/2 + 𝑎₂𝑥³/3 + 𝑎₃𝑥⁴/4 + ···

  где c — произвольная константа. Определите процедуру integrate-series, которая на входе принимает поток
  𝑎₀, 𝑎₁, 𝑎₂, ... , представляющую степенной ряд, и возвращает поток 𝑎₀, 𝑎₁/2, 𝑎₂/3, ... коэффициентов
  при неконстантных членах интеграла последовательности. (Поскольку в результате отсутствует постоянный
  член, он не представляет собой степенной ряд; при использовании integrate-series мы через cons будем
  присоединять к началу соответствующую константу.)

  б. Функция 𝑥 → eˣ равна своей собственной производной. Отсюда следует, что eˣ и интеграл eˣ суть одна
  и та же последовательность, с точностью до постоянного члена, который равен e⁰ = 1. Соответственно,
  можно породить последовательность для eˣ через

    (define exp-series
      (cons-stream 1 (integrate-series exp-series)))

  Покажите, как породить последовательности для синуса и косинуса, опираясь на то, что производная синуса
  равна косинусу, а производная косинуса равна минус синусу:

    (define cosine-series
      (cons-stream 1 <??>))
    (define sine-series
      (cons-stream 0 <??>))
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (stream-null? stream) (stream-empty? stream))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams stream1 stream2)
  (stream-map + stream1 stream2))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out n)
  (when (not (<= n 0))
        (display-line (stream-car stream) out)
        (display-stream (stream-cdr stream) out (- n 1))))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams ones integers)))

(define (integrate-series coeff-stream)
  (define (rec stream int)
    (stream-cons (/ (stream-car stream) (stream-car int))
                 (rec (stream-cdr stream) (stream-cdr int))))

  (rec coeff-stream integers))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define cosine-series
  (stream-cons 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define output1 (open-output-string))

(display-stream cosine-series output1 8)

(check-equal? (get-output-string output1) "\n1\n0\n-1/2\n0\n1/24\n0\n-1/720\n0")

(close-output-port output1)

(define output2 (open-output-string))

(display-stream sine-series output2 8)

(check-equal? (get-output-string output2) "\n0\n1\n0\n-1/6\n0\n1/120\n0\n-1/5040")

(close-output-port output2)
