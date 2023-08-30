#lang racket

#|
  Упражнение 2.11

  Проходя мимо, Бен делает туманное замечание: «Если проверять знаки концов интервалов, можно разбить
  mul-interval на девять случаев, из которых только в одном требуется более двух умножений».
  Перепишите эту процедуру в соответствии с предложением Бена.
|#

(#%require rackunit)

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (sign a)
  (if (>= a 0)
      1
     -1))

(define (mul-interval a b)
  (let ((al (lower-bound a))
        (au (upper-bound a))
        (bl (lower-bound b))
        (bu (upper-bound b)))
    (cond ((= (sign al) 1)
           (cond ((= (sign bl) 1) (make-interval (* al bl) (* au bu)))
                 ((= (sign bu) -1) (make-interval (* au bl) (* al bu)))
                 (else (make-interval (* au bl) (* au bu)))))
          ((= (sign au) -1)
           (cond ((= (sign bl) 1) (make-interval (* al bu) (* au bl)))
                 ((= (sign bu) -1) (make-interval (* au bu) (* al bl)))
                 (else (make-interval (* al bu) (* al bl)))))
          (else
           (cond ((= (sign bl) 1) (make-interval (* al bu) (* au bu)))
                 ((= (sign bu) -1) (make-interval (* al bu) (* al bl)))
                 (else (make-interval (min (* al bu) (* au bl))
                                      (max (* al bl) (* au bu)))))))))

(define intervals (list (make-interval 5 10)
                        (make-interval -10 -5)
                        (make-interval 0 10)
                        (make-interval -10 0)
                        (make-interval 0 0)
                        (make-interval -10 10)))

(for/and ([interval-a intervals] [interval-b (cdr intervals)])
  (check-equal? (mul-interval interval-a interval-b)
                (mul-interval interval-b interval-a)))

(check-equal? (mul-interval (make-interval -8 4)
                            (make-interval -12 10))
              (make-interval -80 96))
