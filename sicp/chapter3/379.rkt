#lang racket

#|
  Упражнение 3.79

  Обобщите процедуру solve-2nd из упражнения 3.78 так, чтобы с ее помощью можно было решать дифференциальные
  уравнения второго порядка общего вида d²y/dt² = f(dy/dt, y).
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

(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(check-equal?
  (stream-ref
    (solve-2nd (lambda (dy _) dy)
               1 1 0.001)
    1000) 2.716923932235896)

; cos 𝜋 = -1

(check-equal?
  (round (* 1000
     (stream-ref
       (solve-2nd (lambda (_ y) (* -1 y))
                  1 0 0.0001)
        31418))) -1000.0)
