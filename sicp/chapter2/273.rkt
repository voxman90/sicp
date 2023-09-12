#lang racket

#|
  Упражнение 2.73

  В разделе 2.3.2 описывается программа, которая осуществляет символьное дифференцирование:

    (define (deriv exp var)
      (cond ((number? exp) 0)
            ((variable? exp) (if (same-variable? exp var) 1 0))
            ((sum? exp)
             (make-sum (deriv (addend exp) var)
                       (deriv (augend exp) var)))
            ((product? exp)
             (make-sum
               (make-product (multiplier exp)
                             (deriv (multiplicand exp) var))
               (make-product (deriv (multiplier exp) var)
                             (multiplicand exp))))
            <more rules can be added here>
            (else (error "unknown expression type -- DERIV" exp))))

  Можно считать, что эта программа осуществляет диспетчеризацию по типу выражения, которое требуется
  продифференцировать. В этом случае «меткой типа» элемента данных является символ алгебраической
  операции (например, +), а операция, которую нужно применить – deriv. Эту программу можно преобразовать
  в управляемый данными стиль, если переписать основную процедуру взятия производной в виде

    (define (deriv exp var)
      (cond ((number? exp) 0)
            ((variable? exp) (if (same-variable? exp var) 1 0))
            (else ((get 'deriv (operator exp)) (operands exp)
                                                var))))
    (define (operator exp) (car exp))
    (define (operands exp) (cdr exp))

  а. Объясните, что происходит в приведенном фрагменте кода. Почему нельзя включить в операцию выбора,
  управляемого данными, предикаты number? и same-variable? ?

  б. Напишите процедуры для вычисления производных от суммы и произведения, а также дополнительный код,
  чтобы добавить их к таблице, которой пользуется приведенный фрагмент.

  в. Выберите еще какое-нибудь правило дифференцирования, например для возведения в степень (упражнение
  2.56), и установите его в систему.

  г. В этой простой алгебраической системе тип выражения — это алгебраическая операция верхнего уровня.
  Допустим, однако, что мы индексируем процедуры противоположным образом, так что строка диспетчеризации
  в deriv выглядит как

    ((get (operator exp) 'deriv) (operands exp) var)

  Какие изменения потребуются в системе дифференцирования?
|#

(#%require rackunit
           compatibility/mlist)

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
          (let ((record (massoc key-2 (mcdr subtable))))
            (if record
                (mcdr record)
                #f))
          #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
          (let ((record (massoc key-2 (mcdr subtable))))
            (if record
                (set-mcdr! record value)
                (set-mcdr! subtable
                           (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
          (set-mcdr! local-table
                     (mcons (mlist key-1
                                   (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp)) (operands exp) var))))

(put 'deriv 'deriv deriv)

#|
  б. Напишите процедуры для вычисления производных от суммы и произведения, а также дополнительный код,
  чтобы добавить их к таблице, которой пользуется приведенный фрагмент.
|#

(define (install-sum-package)
  (define (deriv exp var)
    ((get 'deriv 'deriv) exp var))

  (define (addend s)
    (car s))

  (define (augend s)
    (cadr s))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else
           (list '+ a1 a2))))

  (define (sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  (put 'deriv '+ sum)
  (put 'make '+ make-sum))

(define (install-product-package)
  (define (deriv exp var)
    ((get 'deriv 'deriv) exp var))

  (define (make-sum operand1 operand2)
    ((get 'make '+) operand1 operand2))

  (define (multiplier p)
    (car p))

  (define (multiplicand p)
    (cadr p))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else
           (list '* m1 m2))))

  (define (product operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))

  (put 'deriv '* product)
  (put 'make '* make-product))

#|
  в. Выберите еще какое-нибудь правило дифференцирования, например для возведения в степень (упражнение
  2.56), и установите его в систему.
|#

(define (install-exponentiation-package)
  (define (deriv exp var)
      ((get 'deriv 'deriv) exp var))

  (define (make-sum operand1 operand2)
      ((get 'make '+) operand1 operand2))

  (define (make-product operand1 operand2)
      ((get 'make '*) operand1 operand2))

  (define (base exp)
    (car exp))

  (define (exponent exp)
    (cadr exp))

  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((=number? base 1) 1)
          ((and (number? exponent) (number? base))
           (expt base exponent))
          (else
           (list '^ base exponent))))

  (define (exponentiation exp var)
    (make-product
      (make-product (exponent exp)
                    (make-exponentiation (base exp)
                                         (make-sum (exponent exp) -1)))
      (deriv (base exp) var)))

  (put 'deriv '^ exponentiation)
  (put 'make '^ make-exponentiation))

(install-sum-package)
(install-product-package)
(install-exponentiation-package)

(check-equal? (deriv '(+ x 3) 'x) 1)
(check-equal? (deriv '(+ 3 x) 'x) 1)
(check-equal? (deriv '(* 5 x) 'x) 5)
(check-equal? (deriv '(* x 3) 'x) 3)
(check-equal? (deriv '(+ (* x 3) x) 'x) 4)
(check-equal? (deriv '(* (+ x 3) (+ x 4)) 'x) '(+ (+ x 3) (+ x 4)))
(check-equal? (deriv '(^ x 2) 'x) '(* 2 x))
(check-equal? (deriv '(^ x 0) 'x) 0)
(check-equal? (deriv '(^ 2 2) 'x) 0)

#|
    (define (deriv exp var)
      (cond ((number? exp) 0)
            ((variable? exp)
             (if (same-variable? exp var) 1 0))
            (else
             ((get 'deriv (operator exp)) (operands exp)
                                          var))))

    (define (operator exp) (car exp))

    (define (operands exp) (cdr exp))

  а. Объясните, что происходит в приведенном фрагменте кода. Почему нельзя включить в операцию выбора,
  управляемого данными, предикаты number? и same-variable? ?
|#

#|
  В приведённом фрагменте кода выражение exp проверяется на то, является ли оно числом и на то, является
  ли оно переменной по которой производится дифференцирование. Если не то и не другое, то обработка
  выражения делегируется соответствующему обработчику (ссылку на который процедура получает из таблицы).

  Их можно включить в операцию выбора, но это лишено смысла.
  Во-первых, в случае нашей процедуры числа и символы - встроенные типы, а не абстракции данных,
  требующие особого подхода.
  Во-вторых, по умолчанию числа и символы не имеют тега и попытки этот тег им сопоставить, представив
  их как унарные операции, ничего не привнесёт в программу.
  В-третьих, предикаты, в некотором смысле, сами занимаются диспетчеризацией, "маркируя" выражения как
  числа или не числа, переменные или не переменные и сопоставляя им #t или #а. Что позволяет заниматься
  диспетчерезацией того, что в ней нуждается.
|#

#|
  г. В этой простой алгебраической системе тип выражения — это алгебраическая операция верхнего уровня.
  Допустим, однако, что мы индексируем процедуры противоположным образом, так что строка диспетчеризации
  в deriv выглядит как

    ((get (operator exp) 'deriv) (operands exp) var)

  Какие изменения потребуются в системе дифференцирования?
|#

#|
  Пришлось бы изменить только порядок аргумнетов в put пакетов с процедурами для суммы, произведения и
  экспоненциирования.
|#
