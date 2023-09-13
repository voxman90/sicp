#lang racket

#|
  Упражнение 2.78

  В пакете scheme-number внутренние процедуры, в сущности, ничего не делают, только вызывают элементарные
  процедуры +, -, и т.д. Прямо использовать примитивы языка не было возможности, поскольку наша система
  меток типов требует, чтобы каждый объект данных был снабжен меткой. Однако на самом деле все реализации
  Лиспа имеют систему типов, которую они используют внутри себя. Элементарные процедуры вроде symbol? или
  number? определяют, относится ли объект к определенному типу. Измените определения type-tag, contents
  и attach-tag из раздела 2.4.2 так, чтобы наша обобщенная система использовала внутреннюю систему типов
  Scheme. То есть, система должна работать так же, как раньше, но только обычные числа должны быть
  представлены просто в виде чисел языка Scheme, а не в виде пары, у которой первый элемент символ
  scheme-number.
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

(define (square x) (* x x))

; Решение упражнения:

(define (attach-tag type-tag contents)
  (cond ((eq? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else
         (error "Incorrect tagged data -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else
         (error "Incorrect tagged data -- TYPE-TAG" datum))))

; Пакет арифметических операций над числами

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
    (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
    (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
    (lambda (x) (tag x)))
  'done)

; Пакеты арифметических операций над комплексными числами в полярном, декартовом и обобщённом
; представлении

(define (install-polar-package)
  (define (magnitude z) (car z))

  (define (angle z) (cdr z))

  (define (make-from-mag-ang r a) (cons r a))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-rectangular-package)
  (define (real-part z) (car z))

  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  (define (real-part z)
    (apply-generic 'real-part z))

  (define (imag-part z)
    (apply-generic 'imag-part z))

  (define (magnitude z)
    (apply-generic 'magnitude z))

  (define (angle z)
    (apply-generic 'angle z))

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex)
    (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-scheme-number-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; Обобщённые процедуры

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "There is no method for this type -- APPLY-GENERIC"
            (list op type-tags))))))

(define (add x y)
  (apply-generic 'add x y))

; Тесты

(define (make-scheme-number num)
  ((get 'make 'scheme-number) num))

(define num1 (make-scheme-number 10))
(define num2 (make-scheme-number 15))
(define sum1 (add num1 num2))

(check-equal? sum1 25)
(check-equal? (type-tag sum1) 'scheme-number)

(define (make-from-real-imag r i)
  ((get 'make-from-real-imag 'complex) r i))

(define complex1 (make-from-real-imag 2 0))
(define complex2 (make-from-real-imag 2 5))
(define sum2 (make-from-real-imag 4 5))

(check-equal? (add complex1 complex2) sum2)

(check-equal? (attach-tag 'scheme-number 42) 42)
(check-equal? (attach-tag 'complex (cons 1 2)) (cons 'complex (cons 1 2)))
(check-equal? (attach-tag 'scheme-number 42) 42)
(check-equal? (attach-tag 'complex (cons 1 2)) (cons 'complex (cons 1 2)))
(check-equal? (contents 42) 42)
(check-equal? (contents (cons 'complex (cons 1 2))) (cons 1 2))
(check-equal? (type-tag (cons 'complex (cons 1 2))) 'complex)
(check-equal? (type-tag 10) 'scheme-number)
