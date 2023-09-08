#lang sicp

#|
  Упражнение 2.57

  Расширьте программу дифференцирования так, чтобы она работала с суммами и произведениями любого (больше
  двух) количества термов. Тогда последний из приведенных выше примеров мог бы быть записан как

    (deriv '(* x y (+ x 3)) 'x)

  Попытайтесь сделать это, изменяя только представление сумм и произведений, не трогая процедуру deriv.
  Тогда, например, процедура addend будет возвращать первое слагаемое суммы, а augend сумму остальных.
|#

(#%require rackunit)

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown type of expression -- DERIV" exp))))

(check-equal? (deriv '(+ x 3) 'x) 1)
(check-equal? (deriv '(+ x x) 'x) 2)
(check-equal? (deriv '(+ x x a x x) 'x) 4)
(check-equal? (deriv '(* a x) 'x) 'a)
(check-equal? (deriv '(+ 5 x (* 4 x) x) 'x) 6)
(check-equal? (deriv '(* x y) 'x) 'y)
(check-equal? (deriv '(* x y (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))
