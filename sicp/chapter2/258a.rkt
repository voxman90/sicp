#lang sicp

#|
  Упражнение 2.58

  Предположим, что нам захотелось изменить программу дифференцирования так, чтобы она работала с обычной
  математической нотацией, где + и * не префиксные, а инфиксные операции. Поскольку программа взятия
  производных определена в терминах абстрактных данных, мы можем изменять представление выражений, с
  которыми она работает, меняя только предикаты, селекторы и конструкторы, определяющие представление
  алгебраических выражений, с которыми должен работать дифференциатор.

  а. Покажите, как это сделать так, чтобы брать производные от выражений, представленных в инфиксной
  форме, например (x + (3 * (x + (y + 2)))). Для упрощения задачи предположите, что + и * всегда
  принимают по два аргумента, и что в выражении расставлены все скобки.

  б. Задача становится существенно сложней, если мы разрешаем стандартную алгебраическую нотацию,
  например (x + 3 * (x + y + 2)), которая опускает ненужные скобки и предполагает, что умножение
  выполняется раньше, чем сложение. Можете ли Вы разработать соответствующие предикаты, селекторы и
  конструкторы для этой нотации так, чтобы наша программа взятия производных продолжала работать?
|#

#|
  a
|#

(#%require rackunit)

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x)
       (eq? (cadr x) '+)))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? x)
  (and (pair? x)
       (eq? (cadr x) '*)))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

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

(check-equal? (deriv '(x + 3) 'x) 1)
(check-equal? (deriv '(x + x) 'x) 2)
(check-equal? (deriv '(a * x) 'x) 'a)
(check-equal? (deriv '(x * a) 'x) 'a)
(check-equal? (deriv '(x + (4 * x)) 'x) 5)
(check-equal? (deriv '(x + (x + (x + y))) 'x) 3)
