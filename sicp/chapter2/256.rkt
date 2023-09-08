#lang sicp

#|
  Упражнение 2.56

  Покажите, как расширить простейшую программу дифференцирования так, чтобы она воспринимала больше
  разных типов выражений. Например, реализуйте правило взятия производной

    𝑑(𝑢ⁿ)/𝑑𝑥 = 𝑛𝑢ⁿ⁻¹(𝑑𝑢/𝑑𝑥)

  добавив еще одну проверку к программе deriv и определив соответствующие процедуры exponentiation?,
  base, exponent и make-exponentiation (обозначать возведение в степень можно символом **). Встройте
  правила, что любое выражение, возведенное в степень 0, дает 1, а возведенное в степень 1 равно самому
  себе.
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
  (caddr s))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

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

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((and (number? exponent) (number? base)) (expt base exponent))
        (else (list '** base exponent))))

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
        ((exponentiation? exp)
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp)
                                              (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))
        (else
          (error "unknown type of expression -- DERIV" exp))))

(check-equal? (exponentiation? '(** x 3)) #t)
(check-equal? (exponentiation? '(+ x 3)) #f)

(check-equal? (exponent '(** x 3)) 3)
(check-equal? (base '(** (+ x 1) 3)) '(+ x 1))

(check-equal? (make-exponentiation 'x 2) '(** x 2))
(check-equal? (make-exponentiation 'x 0) 1)
(check-equal? (make-exponentiation 'x 1) 'x)
(check-equal? (make-exponentiation 1 '(* x (sin 10))) 1)
(check-equal? (make-exponentiation 1 0) 1)
(check-equal? (make-exponentiation 4 2) 16)
(check-equal? (make-exponentiation '(+ (/ x y) 4) 1) '(+ (/ x y) 4))

(check-equal? (deriv '(** x 3) 'x) '(* 3 (** x 2)))
(check-equal? (deriv '(+ (+ (** x 0) (* y x)) 5) 'x) 'y)
