#lang racket

#|
  Упражнение 2.82

  Покажите, как обобщить apply-generic так, чтобы она обрабатывала приведение в общем случае с несколькими
  аргументами. Один из способов состоит в том, чтобы попытаться сначала привести все аргументы к типу
  первого, потом к типу второго, и так далее. Приведите пример, когда эта стратегия (а также двухаргументная
  версия, описанная выше) недостаточно обща. (Подсказка: рассмотрите случай, когда в таблице есть какие-то
  подходящие операции со смешанными типами, но обращения к ним не произойдет.)
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

; Обобщённые процедуры

(define (make-scheme-number num)
  ((get 'make 'scheme-number) num))

(define (make-complex-from-real-imag r i)
  ((get 'make-from-real-imag 'complex) r i))

(install-scheme-number-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; Решение упражнения

(define type-coercion-table (make-table))

(define put-coercion (type-coercion-table 'insert-proc!))

(define get-coercion (type-coercion-table 'lookup-proc))

(define (add x y)
  (apply-generic 'add x y))

(define (type-conversion args type-tags target-type)
  (define (bypass args args-types acc)
    (if (null? args)
        acc
        (let ((arg  (car args))
              (type (car args-types)))
          (if (eq? type target-type)
              (bypass (cdr args)
                      (cdr args-types)
                      (append acc (list arg)))
              (let ((converter (get-coercion type target-type)))
                (if converter
                    (bypass (cdr args)
                            (cdr args-types)
                            (append acc (list (converter arg))))
                   '()))))))

  (bypass args type-tags '()))

(define (apply-generic op . args)
  (define (send-error op type-tags)
    (error "There is no method for this types -- APPLY-GENERIC"
           (list op type-tags)))

  (define (make-proc-signature type)
    (define (iter i)
      (if (= i 0)
         '()
          (cons type (iter (- i 1)))))

    (iter (length args)))

  (define (apply-with-type-conversion args-type-tags type-list)
    (if (null? type-list)
        (send-error op args-type-tags)
        (let ((type (car type-list)))
          (let ((proc (get op (make-proc-signature type))))
            (if proc
                (let ((converted-args (type-conversion args args-type-tags type)))
                  (if (null? converted-args)
                      (apply-with-type-conversion args-type-tags
                                                  (cdr type-list))
                      (apply proc (map contents converted-args))))
                (apply-with-type-conversion args-type-tags
                                            (cdr type-list)))))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
         (apply proc (map contents args))
         (apply-with-type-conversion type-tags type-tags)))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define schemenum1 (make-scheme-number 3))
(define schemenum2 (make-scheme-number 2))

(check-equal? (add schemenum1 schemenum2) 5)

(define complex1 (make-complex-from-real-imag 10 5))
(define complex2 (make-complex-from-real-imag 2 7))
(define complex-sum1 (make-complex-from-real-imag 12 12))

(check-equal? (add complex1 complex2) complex-sum1)

(define complex1-schemenum1-sum (make-complex-from-real-imag 13 5))
(define complex2-schemenum1-sum (make-complex-from-real-imag 5 7))

(check-equal? (add schemenum1 complex1) complex1-schemenum1-sum)
(check-equal? (add complex1 schemenum1) complex1-schemenum1-sum)
(check-equal? (add schemenum1 complex2) complex2-schemenum1-sum)
(check-equal? (add complex2 schemenum1) complex2-schemenum1-sum)

#|
  Приведите пример, когда эта стратегия (а также двухаргументная версия, описанная выше) недостаточно обща.
|#

#|
  Apply-generic в такой реализации не проверяет существование операций со "смешанными" сигнатурами типов,
  кроме, собственно, сигнатуры типов аргументов (если она смешанная), а так же не проверяет сигнатуры
  включающие типы, которые не присутствуют в наборе типов аргументов.

  Приведём два примера:

  Пусть существует операция op1, которая работает только над парами ('type2 'type1) и 'type2 приводится
  к 'type1, а 'type1 приводится к 'type2. Если попытаться применить операцию op1 через apply-generic к
  паре аргуметов типа ('type1 'type2), то будут проверены сигнатуры ('type1 'type2), ('type1 'type1),
  ('type2 'type2) и, соответственно, apply-generic вернёт ошибку.

  Пусть существует операция op2, которая работает только над парами ('type3 'type3) и 'type1 приводится
  к 'type3, а 'type2 приводится к 'type3. Если попытаться применить операцию op2 через apply-generic к
  паре аргуметов типа ('type1 'type2), то будут проверены сигнатуры ('type1 'type2), ('type1 'type1),
  ('type2 'type2) и, соответственно, apply-generic вернёт ошибку.
|#
