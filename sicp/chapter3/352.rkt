#lang racket

#|
  Упражнение 3.52

  Рассмотрим последовательность выражений

    (define sum 0)

    (define (accum x)
      (set! sum (+ x sum))
      sum)

    (define seq (stream-map accum (stream-enumerate-interval 1 20)))

    (define y (stream-filter even? seq))

    (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                             seq))

    (stream-ref y 7)

    (display-stream z)

  Каково значение sum после вычисления каждого из этих выражений? Что печатается при вычислении выражений
  stream-ref и display-stream? Изменился бы этот результат, если бы мы реализовали (delay <exp>) просто
  как (lambda () <exp>), не применяя оптимизацию через memo-proc? Объясните свой ответ.
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (stream-null? stream) (stream-empty? stream))

(define (cons-stream first rest) (stream-cons first rest))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin
        (proc (stream-car stream))
        (stream-for-each proc (stream-cdr stream)))))

(define (stream-map proc stream)
  (if (stream-null? stream)
      the-empty-stream
      (stream-cons (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

(define (stream-filter pred? stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred? (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred? (stream-cdr stream))))
        (else
         (stream-filter pred? (stream-cdr stream)))))

(define (display-line x out)
  (newline out)
  (display x out))

(define (display-stream stream out)
  (stream-for-each
    (lambda (item) (display-line item out))
    stream))

(define output1 (open-output-string))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

#|
  x отображается в сумму от 1 до x, но только при первом проходе, т.к. переменная sum не обнуляется
  seq в развёрнутом виде можно представить, как:

    [1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210]

  Если же мы будем обращаться к элементам через stream-ref и обнулять sum после этого, то seq для нас
  будет иметь вид:

    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
|#

(check-equal? sum 0)

#|
  Так как мы не обращались к элементам потока, то их вычисление находится в отложенном состоянии,
  соответственно, сумма всё ещё равна нулю.
|#

(define y (stream-filter even? seq))

#|
  y поток, который выводит чётные элементы потока seq:

    [6, 10, 28, 36, 66, 78, 120, 136, 190, 210]
|#

(check-equal? sum 6)

#|
  Если использовать встроенные методы Racket, то sum так же будет равно 0. В данном случае, сумма стала
  равна шести, т.к. определение фильтрации из книги SICP не откладывает вычисление первого элемента потока
  в процедуре фильтрации потока.
|#

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

#|
  z поток, который выводит элементы потока seq кратные пяти:

    [10, 15, 45, 55, 105, 120, 190, 210]
|#

(check-equal? sum 10)

#|
  Опять же, если бы использовался встроенный метод, то sum всё ещё была бы равна 0. Процедура фильтрации
  пробегает первые 4 элемента потока, но т.к. испольщуется мемоизация, то первый три обращения не повлияли
  на sum, их значение было взято из замыканий процедуры-мемоизатора. Но четвёртой обращение привело к
  вычислениям и изменению sum.
|#

(check-equal? (stream-ref y 7) 136) ; y[7] = 136

(check-equal? sum 136)

(display-stream z output1)

(check-equal? (get-output-string output1) "\n10\n15\n45\n55\n105\n120\n190\n210")

(check-equal? sum 210)

(close-output-port output1)

#|
  Результат бы изменился, если бы stream-cons не использовал мемоизацию. Продемонстрируем это:
|#

(define sum-alt 0)

(define (accum-alt x)
  (set! sum-alt (+ x sum-alt))
  sum-alt)

(define (stream-null-alt? stream) (null? stream))

(define (stream-car-alt stream) (car stream))

(define (stream-cdr-alt stream) ((cdr stream)))

(define (stream-enumerate-interval-alt low high)
  (if (> low high)
     '()
      (cons low
            (lambda () (stream-enumerate-interval-alt (+ low 1) high)))))

(define (stream-for-each-alt proc stream)
  (if (stream-null-alt? stream)
      'done
      (begin
        (proc (stream-car-alt stream))
        (stream-for-each-alt proc (stream-cdr-alt stream)))))

(define (display-stream-alt stream out)
  (stream-for-each-alt
    (lambda (item) (display-line item out))
    stream))

(define output2 (open-output-string))

(define (stream-ref-alt stream i)
  (if (= i 0)
      (stream-car-alt stream)
      (stream-ref-alt (stream-cdr-alt stream) (- i 1))))

(define (stream-map-alt proc stream)
  (if (stream-null-alt? stream)
     '()
      (cons (proc (stream-car-alt stream))
            (lambda () (stream-map-alt proc (stream-cdr-alt stream))))))

(define (stream-filter-alt pred? stream)
  (cond ((stream-null-alt? stream) '())
        ((pred? (stream-car-alt stream))
         (cons (stream-car-alt stream)
               (lambda () (stream-filter-alt pred? (stream-cdr-alt stream)))))
        (else
         (stream-filter-alt pred? (stream-cdr-alt stream)))))

(check-equal? sum-alt 0)

(define seq-alt (stream-map-alt accum-alt (stream-enumerate-interval-alt 1 20)))

(check-equal? sum-alt 1)

#|
  Разница с предыдущими рассчётами обусловлена тем, что мы используем cons, вместо специальной конструкции
  stream-cons, поэтому первый элемент потока вычисляется сразу же.
|#

(define y-alt (stream-filter-alt even? seq-alt))

(check-equal? sum-alt 6)

(define z-alt (stream-filter-alt (lambda (x) (= (remainder x 5) 0))
                                 seq-alt))

(check-equal? sum-alt 15)

#|
  Ещё одно расхождение, которое вызвано тем, что в процессе фильтрации, вычисления первого элемента
  z-alt заново подсчитывались элементы seq-alt (что их изменило, потому что изменилась sum-alt и отсутствует
  мемоизация). Последующие расхождения связаны с тем же накоплением побочных эффектов повторных вычислений.
|#

(check-equal? (stream-ref-alt y-alt 7) 162)

(display-stream-alt z-alt output2)

(check-equal? (get-output-string output2) "\n15\n180\n230\n305")

(check-equal? sum-alt 362)

(close-output-port output2)
