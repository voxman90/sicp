#lang racket

#|
  Упражнение 3.77

  Вышеприведенная процедура integral была аналогична «непрямому» определению бесконечного потока
  натуральных чисел из раздела 3.5.2. В виде альтернативы можно дать определение integral, более похожее
  на integers-starting-from (также в разделе 3.5.2):

    (define (integral integrand initial-value dt)
      (cons-stream initial-value
        (if (stream-null? integrand)
            the-empty-stream
            (integral (stream-cdr integrand)
                      (+ (* dt (stream-car integrand))
                         initial-value)
                      dt))))

  В системах с циклами эта реализация порождает такие же проблемы, как и наша исходная версия integral.
  Модифицируйте процедуру так, чтобы она ожидала integrand как задержанный аргумент, а следовательно,
  могла быть использована в процедуре solve.
|#

(#%require rackunit
           racket/stream)

(define the-empty-stream empty-stream)

(define (stream-null? stream) (stream-empty? stream))

(define (stream-car stream) (stream-first stream))

(define (stream-cdr stream) (stream-rest stream))

(define (stream-ref stream n)
  (if (<= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (stream-cons
        (apply proc (map stream-car streams))
        (apply stream-map
               (cons proc (map stream-cdr streams))))))

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

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(check-equal? (stream-ref (solve (lambda (y) y) 1 0.001) 1000) 2.716923932235896)
