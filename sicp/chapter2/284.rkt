#lang racket

#|
  Упражнение 2.84

  Используя операцию raise из упражнения 2.83, измените процедуру apply-generic так, чтобы она приводила
  аргументы к одному типу путем последовательного подъема, как описано в этом разделе. Потребуется придумать
  способ проверки, какой из двух типов выше по башне. Сделайте это способом, «совместимым» с остальной
  системой, так, чтобы не возникало проблем при добавлении к башне новых типов.
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

(define type-coercion-table (make-table))
(define put-coercion (type-coercion-table 'insert-proc!))
(define get-coercion (type-coercion-table 'lookup-proc))

; Служебные процедуры

(define (square x) (* x x))

; Процедуры для присоединения тега, извлечения тега и извлечения контента данных

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

; Пакет арифметических операций над целыми числами

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))

  (define (make-integer x)
    (if (exact-integer? x)
        (tag x)
        (error "Incorrect argument -- MAKE-INTEGER" x)))

  (define (div x y)
    (let ((res (/ x y)))
      (if (exact-integer? res)
          (tag res)
          res)))

  (put 'add '(integer integer)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
    (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) div)
  (put 'make 'integer make-integer)
  'done)

; Пакет арифметических операций над рациональными числами

(define (install-rational-package)
  (define (tag x) (attach-tag 'rational x))
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
    (if (and (exact-integer? n)
             (natural? (abs d)))
        (let ((g (gcd n d)))
          (tag (cons (/ n g) (/ d g))))
        (error "Incorrect argument -- MAKE-RAT" n d)))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational)
    (lambda (x y) (add-rat x y)))
  (put 'sub '(rational rational)
    (lambda (x y) (sub-rat x y)))
  (put 'mul '(rational rational)
    (lambda (x y) (mul-rat x y)))
  (put 'div '(rational rational)
    (lambda (x y) (div-rat x y)))
  (put 'make 'rational make-rat)
  'done)

; Пакеты арифметических операций над вещественными числами

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))

  (define (make-real n)
    (if (real? n)
        (tag (* 1.0 n))
        (error "Incorrect argument -- MAKE" n)))

  (put 'add '(real real)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
    (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
    (lambda (x y) (tag (/ x y))))
  (put 'make 'real make-real)
  'done)

; Пакеты арифметических операций над комплексными числами в полярном, декартовом и обобщённом
; представлении

(define (install-polar-package)
  (define (tag x) (attach-tag 'polar x))
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
  (define (tag x) (attach-tag 'rectangular x))
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
  (define (tag z) (attach-tag 'complex z))

  (define (real-part z)
    (apply-generic 'real-part z))

  (define (imag-part z)
    (apply-generic 'imag-part z))

  (define (magnitude z)
    (apply-generic 'magnitude z))

  (define (angle z)
    (apply-generic 'angle z))

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) (* 1.0 x) (* 1.0 y)))

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

(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-real n)
  ((get 'make 'real) n))

(define (make-complex-from-real-imag r i)
  ((get 'make-from-real-imag 'complex) r i))

(define (add x y)
  (apply-generic 'add x y))

(define (mul x y)
  (apply-generic 'mul x y))

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; Решение упражнения

(define types-tower (list 'integer 'rational 'real 'complex))

(define (next-type type)
  (let ((types-above (cdr (memq type types-tower))))
    (if (null? types-above)
        type
        (car types-above))))

(define (raise x)
  (let ((arg-type (type-tag x)))
    (let ((type-above (next-type arg-type)))
      (let ((type-converter (get-coercion arg-type type-above)))
        (if type-converter
            (type-converter x)
            #f)))))

(define (raise-to x type)
  (define (iter x x-type)
    (if (eq? x-type type)
        x
        (let ((raised-x (raise x)))
          (if raised-x
              (iter raised-x
                    (type-tag raised-x))
              #f))))

  (let ((x-type (type-tag x)))
    (if (type-above? type x-type)
        (iter x x-type)
        #f)))

(define (type-above? type1 type2)
  (not (not (memq type1 (memq type2 types-tower)))))

(define (type-conversion args target-type)
  (define (bypass args acc)
    (if (null? args)
        acc
        (let ((raised-arg (raise-to (car args) target-type)))
          (if raised-arg
              (bypass (cdr args)
                      (append acc (list raised-arg)))
             '()))))

  (bypass args '()))

(define (apply-generic op . args)
  (define type-tags (map type-tag args))

  (define (send-error)
    (error "There is no method for this types -- APPLY-GENERIC"
           (list op type-tags)))

  (define (make-proc-signature type)
    (define (iter i)
      (if (= i 0)
         '()
          (cons type (iter (- i 1)))))

    (iter (length args)))

  (define (apply-with-type-conversion type-list)
    (if (null? type-list)
        (send-error)
        (let ((target-type (car type-list)))
          (let ((proc (get op (make-proc-signature target-type))))
            (if proc
                (let ((converted-args (type-conversion args target-type)))
                  (if (null? converted-args)
                      (apply-with-type-conversion (cdr type-list))
                      (apply proc (map contents converted-args))))
                (apply-with-type-conversion (cdr type-list)))))))

  (let ((proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (apply-with-type-conversion type-tags))))

(define (type-coercion-package)
  (define (integer->rational int)
    ((get 'make 'rational) (contents int) 1))

  (define (rational->real rat)
    (let ((rat-contents (contents rat)))
      (let ((numer (car rat-contents))
            (denom (cdr rat-contents)))
        ((get 'make 'real) (* 1.0 (/ numer denom))))))

  (define (real->complex real)
    ((get 'make-from-real-imag 'complex) (contents real) 0))

  (put-coercion 'integer 'rational integer->rational)
  (put-coercion 'rational 'real rational->real)
  (put-coercion 'real 'complex real->complex)
  'done)

(type-coercion-package)

; Тесты

(define int (make-integer 10))
(define rat (make-rational 10 1))
(define real (make-real 10))
(define complex (make-complex-from-real-imag 10 0))

(check-equal? (raise int) rat)
(check-equal? (raise rat) real)
(check-equal? (raise real) complex)
(check-equal? (raise complex) #f)

(define int1 (make-integer 1))
(define int2 (make-integer 2))
(define int1+int2 (make-integer 3))

(check-equal? (add int1 int2) int1+int2)

(define rat1 (make-rational 1 1))
(define rat2 (make-rational 5 2))
(define rat1+rat2 (make-rational 7 2))

(check-equal? (add rat1 rat2) rat1+rat2)
(check-equal? (add int1 rat2) rat1+rat2)
(check-equal? (add rat2 rat1) rat1+rat2)

(define int2+rat2 (make-rational 9 2))

(check-equal? (add int2 rat2) int2+rat2)
(check-equal? (add rat2 int2) int2+rat2)

(define real1 (make-real 1.0))
(define real2 (make-real 2.5))
(define real1+real2 (make-real 3.5))

(check-equal? (add real1 real2) real1+real2)

(define int1+real2 (make-real 3.5))

(check-equal? (add int1 real2) int1+real2)
(check-equal? (add real2 int1) int1+real2)
(check-equal? (add rat1 real2) int1+real2)
(check-equal? (add real2 rat1) int1+real2)

(define complex1 (make-complex-from-real-imag 1.0 0))
(define complex2 (make-complex-from-real-imag 2.5 1))
(define complex1+complex2 (make-complex-from-real-imag 3.5 1))

(check-equal? (add complex1 complex2) complex1+complex2)

(check-equal? (add int1 complex2) complex1+complex2)
(check-equal? (add complex2 int1) complex1+complex2)
(check-equal? (add rat1 complex2) complex1+complex2)
(check-equal? (add complex2 rat1) complex1+complex2)
(check-equal? (add real1 complex2) complex1+complex2)
(check-equal? (add complex2 real1) complex1+complex2)
