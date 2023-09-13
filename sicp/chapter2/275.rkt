#lang sicp

#|
  Упражнение 2.75

  Реализуйте в стиле передачи сообщений конструктор make-from-mag-ang. Он должен быть аналогичен
  приведенной выше процедуре make-from-real-imag.
|#

(#%require rackunit)

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* mag (cos ang)))
          ((eq? op 'imag-part)
           (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error "Unknown operation -- MAKE-FROM-MAG-ANG" op))))

  dispatch)

(define (apply-generic op arg)
  (arg op))

(define (get-real-part val)
  (apply-generic 'real-part val))

(define (get-imag-part val)
  (apply-generic 'imag-part val))

(define (get-magnitude val)
  (apply-generic 'magnitude val))

(define (get-angle val)
  (apply-generic 'angle val))

(define magnitude 2)
(define angle 90.0)

(define test-value (make-from-mag-ang magnitude angle))

(check-equal? (get-magnitude test-value) magnitude)
(check-equal? (get-angle test-value) angle)
(check-equal? (get-real-part test-value) (* magnitude (cos angle)))
(check-equal? (get-imag-part test-value) (* magnitude (sin angle)))
