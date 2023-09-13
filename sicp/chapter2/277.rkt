#lang racket

#|
  Упражнение 2.77

  Хьюго Дум пытается вычислить выражение (magnitude z), где z — объект, показанный на рис. 2.24. К своему
  удивлению, вместо ответа 5 он получает сообщение об ошибке от apply-generic, гласящее, что у операции
  magnitude нет методов для типа (complex). Он показывает результат Лизе П. Хакер. Та заявляет: "Дело в
  том, что селекторы комплексных чисел для чисел с меткой complex определены не были, а были только для
  чисел с меткой polar и rectangular. Все, что требуется, чтобы заставить это работать — это добавить к
  пакету complex следующее:"

    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)

  Подробно опишите, почему это работает. В качестве примера, проследите все процедуры, которые вызываются
  при вычислении (magnitude z), где z — объект, показанный на рис. 2.24. В частности, сколько раз
  вызывается apply-generic? На какую процедуру она диспетчирует в каждом случае?

     ┏━━━┳━━━┓    ┏━━━┳━━━┓    ┏━━━┳━━━┓
  -→ ┃ • ┃ • ┃ -→ ┃ • ┃ • ┃ -→ ┃ • ┃ • ┃
     ┗━━━┻━━━┛    ┗━━━┻━━━┛    ┗━━━┻━━━┛
       ↓            ↓            ↓   ↓
   ┏━━━━━━━━━┓ ┏━━━━━━━━━━━━━┓ ┏━━━┓┏━━━┓
   ┃ complex ┃ ┃ rectangular ┃ ┃ 3 ┃┃ 4 ┃
   ┗━━━━━━━━━┛ ┗━━━━━━━━━━━━━┛ ┗━━━┛┗━━━┛

  Рис. 2.24. Представление 3 + 4i в декартовой форме
|#

#|
  (magnitude z) =>
  (magnitude ('complex . ('rectangular . (3 . 4)))) =>
  (apply-generic 'magnitude ('complex . ('rectangular . (3 . 4)))) => ... =>
  (apply magnitude* (map contents ('complex . ('rectangular . (3 . 4))))) => ... =>
  (magnitude* ('rectangular . (3 . 4))) => ... =>
  (apply-generic 'magnitude ('rectangular . (3 . 4))) => ... =>
  (apply magnitude** (map contents ('rectangular . (3 . 4)))) => ... =>
  (magnitude** (3 . 4)) =>
  (sqrt (+ (square (real-part (3 . 4))) (square (imag-part (3 . 4))))) => ... =>
  (sqrt (+ (square 3) (square 4))) => ... =>
  (sqrt (+ 9 16)) => (sqrt 25) => 5

  * - это версия процедуры из пакета install-complex-package
  ** - это версия процедуры из пакета install-rectangular-package

  Приведённый выше порядок вызовов исчерпывающе объясняет почему это работает.

  Apply-generic вызывается дважды. В первый раз в рамках обобщённой процедуры magnitude, второй раз в
  рамках процедуры magnitude из пакета complex.

  В каждом случае идёт диспетчеризация на процедуру magnitude.
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

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Incorrect tagged data -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Incorrect tagged data -- CONTENTS" datum)))

(define (square x) (* x x))

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

(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "There is no method for this type -- APPLY-GENERIC"
            (list op type-tags))))))

(define z (cons 'complex
                (cons 'rectangular
                      (cons 3 4))))

(define (magnitude num)
  (apply-generic 'magnitude num))

(check-equal? (magnitude z) 5)
