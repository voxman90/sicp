#lang sicp

#|
  Упражнение 1.26

  У Хьюго Дума большие трудности в упражнении 1.24. Процедура fast-prime? у него работает
  медленнее, чем prime?. Хьюго просит помощи у своей знакомой Евы Лу Атор. Вместе изучая
  код Хьюго, они обнаруживают, что тот переписал процедуру expmod с явным использованием
  умножения вместо того, чтобы вызывать square:

    (define (expmod base exp m)
      (cond ((= exp 0) 1)
            ((even? exp)
             (remainder (* (expmod base (/ exp 2) m)
                           (expmod base (/ exp 2) m))
                        m))
            (else
             (remainder (* base (expmod base (- exp 1) m))
                        m))))

  Хьюго говорит: «Я не вижу здесь никакой разницы». «Зато я вижу, — отвечает Ева. — Переписав
  процедуру таким образом, ты превратил процесс порядка Θ(log n) в процесс порядка Θ(n)».
  Объясните.
|#

(define (square n)
  (* n n))

(define (for i pred? inc body)
  (if (pred? i)
      (begin
        (body i)
        (for (inc i) pred? inc body))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (expmod-squareless base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod-squareless base (/ exp 2) m)
                       (expmod-squareless base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod-squareless base (- exp 1) m))
                    m))))

(define (expmod-memo base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ((lambda (expmod-result) (remainder (* expmod-result expmod-result) m))
          (expmod-memo base (/ exp 2) m)))
        (else
         (remainder (* base (expmod-memo base (- exp 1) m))
                    m))))

(define (remainder-test a m expmod)
  (= (expmod a m m) m))

(define (get-number-test-time a m start-time expmod)
  (remainder-test a m expmod)
  (- (runtime) start-time))

(define RUN_COUNT 100.0)

(define (get-remainder-test-time-median a m expmod)
  (define acc 0)

  (for 0
       (lambda (i) (< i RUN_COUNT))
       (lambda (i) (+ i 1))
       (lambda (_) (set! acc (+ acc (get-number-test-time a m (runtime) expmod)))))
  (/ acc RUN_COUNT))

(define (print-result f-name a m time)
  (map display
    (list f-name ": a = " a "; m = " m "; avrg. time: " time " ms"))
  (display "\n"))

(define (get-test-time-median-squareless a m)
  (print-result "expmod-squareless" a m (get-remainder-test-time-median a m expmod-squareless)))

(define (get-test-time-median-memo a m)
  (print-result "expmod-memo" a m (get-remainder-test-time-median a m expmod-memo)))

(define (get-test-time-median a m)
  (print-result "expmod" a m (get-remainder-test-time-median a m expmod)))

(define (print-test-time args)
  (define a (car args))
  (define m (car (cdr args)))
  (get-test-time-median a m)
  (get-test-time-median-memo a m)
  (get-test-time-median-squareless a m)
)

((lambda ()
  (map print-test-time
  '((500 1000) (999 1000)
    (5000 10000) (9999 10000)
    (50000 100000) (99999 100000)))
  (display "end")))

#|
  Процесс приобретает порядок Θ(n) из-за того, что на каждом шаге, когда exp чётно, expmod
  будет вычисляться дважды. В случае square, из-за аппликативного характера подстановки, на
  каждом шаге expmod вычисляется единожды (т.к. выражения переданные в качестве аргументов
  вычисляются первее тела процедуры).
  Но почему после первого вычисления выраджения в вариации Хьюго Дума программа не подставляет
  его вместо второго? Почему не происходит мемоизации? Как минимум потому, что процедура может
  быть недетерминированной и иметь побочные эффекты. Подобная подстановка определённо имела бы
  смысл для чистых функций, но интерпретатор заранее не знает, является ли функция чистой или
  нет.
|#
