#lang racket

#|
  Упражнение 2.88

  Расширьте систему многочленов так, чтобы она включала вычитание многочленов. (Подсказка: может оказаться
  полезным определить обобщенную операцию смены знака.)
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

; Процедуры для присоединения тега, извлечения тега и извлечения контента данных

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'real)
        (else (error "Incorrect tagged data -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) (* 1.0 datum))
        (else (error "Incorrect tagged data -- CONTENTS" datum))))

; Пакет арифметических операций над целыми числами

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))

  (define (make-integer x)
    (if (exact-integer? x)
        (tag x)
        (error "Incorrect argument -- MAKE-INTEGER" x)))

  (define (integer->rational int)
    ((get 'make 'rational) int 1))

  (define (div x y)
    (let ((res (/ x y)))
      (if (exact-integer? res)
          (tag res)
          res)))

  (put 'make 'integer make-integer)
  (put-coercion 'integer 'rational
    (lambda (int) (integer->rational (contents int))))
  (put 'equ? '(integer integer)
    (lambda (x y) (= x y)))
  (put 'zero? '(integer)
    (lambda (x) (= x 0)))
  (put 'reverse-sign '(integer)
    (lambda (x) (tag (* -1 x))))
  (put 'add '(integer integer)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
    (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) div)
  (put 'cos '(integer)
    (lambda (x) (cos x)))
  (put 'sin '(integer)
    (lambda (x) (sin x)))
  (put 'atan '(integer integer)
    (lambda (x y) (atan x y)))
  (put 'sqrt '(integer)
    (lambda (x) (sqrt x)))
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

  (define (rational->integer rat)
    ((get 'make 'integer) (exact-round (/ (numer rat)
                                          (denom rat)))))

  (define (rational->real rat)
    ((get 'make 'real) (* 1.0
                          (/ (numer rat) (denom rat)))))

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

  (define (cosine rat)
    (cos (/ (numer rat) (denom rat))))

  (define (sine rat)
    (sin (/ (numer rat) (denom rat))))

  (define (arctg x y)
    (atan (/ (numer x) (denom x))
          (/ (numer y) (denom y))))

  (define (square-root rat)
    (sqrt (/ (numer rat) (denom rat))))

  (put 'make 'rational make-rat)
  (put-coercion 'rational 'integer
    (lambda (rat) (rational->integer (contents rat))))
  (put-coercion 'rational 'real
    (lambda (rat) (rational->real (contents rat))))
  (put 'equ? '(rational rational)
    (lambda (x y)
      (and (= (car x) (car y))
           (= (cdr x) (cdr y)))))
  (put 'zero? '(rational)
    (lambda (x)
      (and      (= (car x) 0)
           (not (= (cdr x) 0)))))
  (put 'reverse-sign '(rational)
    (lambda (x) (make-rat (* -1 (numer x))
                          (denom x))))
  (put 'add '(rational rational)
    (lambda (x y) (add-rat x y)))
  (put 'sub '(rational rational)
    (lambda (x y) (sub-rat x y)))
  (put 'mul '(rational rational)
    (lambda (x y) (mul-rat x y)))
  (put 'div '(rational rational)
    (lambda (x y) (div-rat x y)))
  (put 'cos '(rational) cosine)
  (put 'sin '(rational) sine)
  (put 'atan '(rational rational) arctg)
  (put 'sqrt '(rational) square-root)
  'done)

; Пакеты арифметических операций над вещественными числами

(define (install-real-package)
  (define rationalize-tolerance 1/100000)
  (define (tag x) (attach-tag 'real x))

  (define (make-real n)
    (if (real? n)
        (tag (* 1.0 n))
        (error "Incorrect argument -- MAKE" n)))

  (define (real->rational n)
    (let ((rat-n (rationalize n rationalize-tolerance)))
      ((get 'make 'rational) (exact-round (numerator rat-n))
                             (exact-round (denominator rat-n)))))

  (define (real->complex n)
    ((get 'make-from-real-imag 'complex) (tag n) (tag 0.0)))

  (put 'make 'real make-real)
  (put-coercion 'real 'rational
    (lambda (real) (real->rational (contents real))))
  (put-coercion 'real 'complex
    (lambda (real) (real->complex (contents real))))
  (put 'equ? '(real real)
    (lambda (x y) (= x y)))
  (put 'zero? '(real)
    (lambda (x) (= x 0)))
  (put 'reverse-sign '(real)
    (lambda (x) (tag (* -1.0 x))))
  (put 'add '(real real)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
    (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
    (lambda (x y) (tag (/ x y))))
  (put 'cos '(real)
    (lambda (x) (cos x)))
  (put 'sin '(real)
    (lambda (x) (sin x)))
  (put 'atan '(real real)
    (lambda (x y) (atan x y)))
  (put 'sqrt '(real)
    (lambda (x) (sqrt x)))
  'done)

; Пакеты арифметических операций над комплексными числами в полярном, декартовом и обобщённом
; представлении

(define (install-polar-package)
  (define (tag x) (attach-tag 'polar x))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))

  (define (real-part z)
    (mul (magnitude z)
         (cosine (angle z))))

  (define (imag-part z)
    (mul (magnitude z)
         (sine (angle z))))

  (define (make-from-real-imag x y)
    (cons (square-root (add (square x)
                            (square y)))
          (arctg y x)))

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
    (square-root (add (square (real-part z))
                      (square (imag-part z)))))

  (define (angle z)
    (arctg (imag-part z)
           (real-part z)))

  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a))
          (mul r (sine a))))

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

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (real-part z)
    (apply-generic 'real-part z))

  (define (imag-part z)
    (apply-generic 'imag-part z))

  (define (magnitude z)
    (apply-generic 'magnitude z))

  (define (angle z)
    (apply-generic 'angle z))

  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1)
                              (real-part z2))
                         (add (imag-part z1)
                              (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1)
                              (real-part z2))
                         (sub (imag-part z1)
                              (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1)
                            (magnitude z2))
                       (add (angle z1)
                            (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1)
                            (magnitude z2))
                       (sub (angle z1)
                            (angle z2))))

  (define (complex->real complex)
    ((get 'make 'real) (shift-to (real-part complex)
                                 'real)))

  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
    (lambda (x y)
      (and (equ? (real-part x) (real-part y))
           (equ? (imag-part x) (imag-part y)))))
  (put 'zero? '(complex)
    (lambda (x)
      (and (=zero? (real-part x))
           (=zero? (imag-part x)))))
  (put-coercion 'complex 'real complex->real)
  (put 'reverse-sign '(complex)
    (lambda (x) (tag (make-complex-from-real-imag (reverse-sign (real-part x))
                                                  (reverse-sign (imag-part x))))))
  (put 'add '(complex complex)
    (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2))))
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

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

(define (square x)
  (mul x x))

(define (cosine x)
  (apply-generic 'cos x))

(define (sine x)
  (apply-generic 'sin x))

(define (arctg x y)
  (apply-generic 'atan x y))

(define (square-root x)
  (apply-generic 'sqrt x))

(define (reverse-sign x)
  (apply-generic 'reverse-sign x))

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define types-tower (list 'integer 'rational 'real 'complex))

(define reversed-types-tower (reverse types-tower))

(define (equ? datum1 datum2)
  (apply-generic 'equ? datum1 datum2))

(define (shift-type type dir)
  (define (shift types-order)
    (let ((subsequent-types (cdr (memq type types-order))))
      (if (null? subsequent-types)
          type
          (car subsequent-types))))

  (if (eq? dir 'forward)
      (shift types-tower)
      (shift reversed-types-tower)))

(define (raise-type type)
  (shift-type type 'forward))

(define (project-type type)
  (shift-type type 'backward))

(define (shift x dir)
  (define (iter get-subsequent-type)
    (let ((x-type (type-tag x)))
      (let ((x-subs-type (get-subsequent-type x-type)))
      (let ((type-converter (get-coercion x-type x-subs-type)))
        (if type-converter
            (type-converter x)
            #f)))))

  (if (eq? dir 'forward)
      (iter raise-type)
      (iter project-type)))

(define (raise x)
  (shift x 'forward))

(define (project datum)
  (let ((projected-datum (shift datum 'backward)))
    (if projected-datum
        (let ((raised-projected-datum (raise projected-datum)))
          (if (equ? datum raised-projected-datum)
              projected-datum
              #f))
        #f)))

(define (shift-to datum target-type)
  (define (iter x x-type next)
    (if (eq? x-type target-type)
        x
        (let ((next-x (next x)))
          (if next-x
              (iter next-x
                    (type-tag next-x)
                    next)
              #f))))

  (let ((datum-type (type-tag datum)))
    (if (type-above? target-type datum-type)
        (iter datum datum-type raise)
        (iter datum datum-type project))))

(define (type-above? type1 type2)
  (not (not (memq type1 (memq type2 types-tower)))))

(define (type-conversion args target-type)
  (define (bypass args acc)
    (if (null? args)
        acc
        (let ((shifted-datum (shift-to (car args) target-type)))
          (if shifted-datum
              (bypass (cdr args)
                      (append acc (list shifted-datum)))
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

(define (drop datum)
  (let ((projected-datum (project datum)))
    (if projected-datum
        (drop projected-datum)
        datum)))

; Тесты

(define int (make-integer 10))
(define rat (make-rational 10 1))
(define real (make-real 10))
(define complex (make-complex-from-real-imag 10 0))

(check-equal? (equ? (raise int) rat) #t)
(check-equal? (equ? (raise rat) real) #t)
(check-equal? (equ? (raise real) complex) #t)
(check-equal? (raise complex) #f)

(check-equal? (project int) #f)
(check-equal? (equ? (project rat) int) #t)
(check-equal? (equ? (project real) rat) #t)
(check-equal? (equ? (project complex) real) #t)

(check-equal? (equ? (drop int) int) #t)
(check-equal? (equ? (drop rat) int) #t)
(check-equal? (equ? (drop real) int) #t)
(check-equal? (equ? (drop complex) int) #t)

(check-equal? (equ? int int) #t)
(check-equal? (equ? int rat) #t)
(check-equal? (equ? int real) #t)
(check-equal? (equ? int complex) #t)

(check-equal? (equ? rat rat) #t)
(check-equal? (equ? rat int) #t)
(check-equal? (equ? rat real) #t)
(check-equal? (equ? rat complex) #t)

(check-equal? (equ? real real) #t)
(check-equal? (equ? real int) #t)
(check-equal? (equ? real rat) #t)
(check-equal? (equ? real complex) #t)

(check-equal? (equ? complex complex) #t)
(check-equal? (equ? complex int) #t)
(check-equal? (equ? complex rat) #t)
(check-equal? (equ? complex real) #t)

(define int1 (make-integer 1))
(define int2 (make-integer 2))
(define int1+int2 (make-integer 3))

(check-equal? (equ? (add int1 int2) int1+int2) #t)

(define rat1 (make-rational 1 1))
(define rat2 (make-rational 5 2))
(define rat1+rat2 (make-rational 7 2))

(check-equal? (equ? (add rat1 rat2) rat1+rat2) #t)
(check-equal? (equ? (add int1 rat2) rat1+rat2) #t)
(check-equal? (equ? (add rat2 rat1) rat1+rat2) #t)

(define int2+rat2 (make-rational 9 2))

(check-equal? (equ? (add int2 rat2) int2+rat2) #t)
(check-equal? (equ? (add rat2 int2) int2+rat2) #t)

(define real1 (make-real 1.0))
(define real2 (make-real 2.5))
(define real1+real2 (make-real 3.5))

(check-equal? (equ? (add real1 real2) real1+real2) #t)

(define int1+real2 (make-real 3.5))

(check-equal? (equ? (add int1 real2) int1+real2) #t)
(check-equal? (equ? (add real2 int1) int1+real2) #t)
(check-equal? (equ? (add rat1 real2) int1+real2) #t)
(check-equal? (equ? (add real2 rat1) int1+real2) #t)

(define complex1 (make-complex-from-real-imag 1.0 0))
(define complex2 (make-complex-from-real-imag 2.5 1))
(define complex1+complex2 (make-complex-from-real-imag 3.5 1))

(check-equal? (equ? (add complex1 complex2) complex1+complex2) #t)

(check-equal? (equ? (add int1 complex2) complex1+complex2) #t)
(check-equal? (equ? (add complex2 int1) complex1+complex2) #t)
(check-equal? (equ? (add rat1 complex2) complex1+complex2) #t)
(check-equal? (equ? (add complex2 rat1) complex1+complex2) #t)
(check-equal? (equ? (add real1 complex2) complex1+complex2) #t)
(check-equal? (equ? (add complex2 real1) complex1+complex2) #t)

(define rat3 (make-rational 7 15))
(define complex3 (make-complex-from-real-imag 1.5 7))

(check-equal? (drop rat3) rat3)
(check-equal? (drop complex3) complex3)

(define rat4 (make-rational 7 2))
(define real4 (make-real 3.5))
(define complex4 (make-complex-from-real-imag 3.5 0))

(check-equal? (drop rat4) rat4)
(check-equal? (drop real4) rat4)
(check-equal? (drop complex4) rat4)

(define real5 (make-real (/ 1 333)))
(define complex5 (make-complex-from-real-imag (/ 1 333) 0))

(check-equal? (drop real5) real5)
(check-equal? (drop complex5) real5)

(check-equal? (equ? (mul int1 int2) '(integer . 2)) #t)
(check-equal? (equ? (mul int2 rat2) '(integer . 5)) #t)

(define complex-int (make-complex-from-real-imag int1 int2))
(define complex-rat (make-complex-from-real-imag rat1 rat2))
(define complex-real (make-complex-from-real-imag real1 real2))
(define complex-num (make-complex-from-real-imag 3 4))

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (angle z)
  (apply-generic 'angle z))

(check-equal? (real-part complex-int) int1)
(check-equal? (imag-part complex-int) int2)
(check-equal? (magnitude complex-int) (square-root (add (square int1)
                                                        (square int2))))
(check-equal? (angle complex-int) (arctg int2 int1))
(check-equal? (real-part complex-rat) rat1)
(check-equal? (imag-part complex-rat) rat2)
(check-equal? (magnitude complex-rat) (square-root (add (square rat1)
                                                        (square rat2))))
(check-equal? (angle complex-rat) (arctg rat2 rat1))
(check-equal? (real-part complex-real) real1)
(check-equal? (imag-part complex-real) real2)
(check-equal? (magnitude complex-real) (square-root (add (square real1)
                                                        (square real2))))
(check-equal? (angle complex-real) (arctg real2 real1))
(check-equal? (real-part complex-num) 3)
(check-equal? (imag-part complex-num) 4)
(check-equal? (magnitude complex-num) 5.0)
(check-equal? (angle complex-num) (atan 4 3))

(define complex-int+complex-rat (make-complex-from-real-imag 2 4.5))

(check-equal? (equ? (add complex-int complex-rat) complex-int+complex-rat) #t)
(check-equal? (equ? (add complex-rat complex-int) complex-int+complex-rat) #t)

(define complex-rat+complex-real (make-complex-from-real-imag 2 5))

(check-equal? (equ? (add complex-rat complex-real) complex-rat+complex-real) #t)
(check-equal? (equ? (add complex-real  complex-rat) complex-rat+complex-real) #t)

(define complex-int+complex-real (make-complex-from-real-imag 2 4.5))

(check-equal? (equ? (add complex-int complex-real) complex-int+complex-real) #t)
(check-equal? (equ? (add complex-real complex-int) complex-int+complex-real) #t)

(define complex-int+complex-real-alt (make-complex-from-real-imag (make-real 2.0)
                                                                  (make-rational 9 2)))

(check-equal? (equ? (add complex-int complex-real) complex-int+complex-real-alt) #t)
(check-equal? (equ? (add complex-real complex-int) complex-int+complex-real-alt) #t)

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))

  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p)
    (car p))

  (define (term-list p)
    (cdr p))

  (define (variable? x)
    (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  ; по условию задачи термы предполагаются упорядоченными от большего к меньшему, поэтому процедура не
  ; пытается их упорядочивать
  (define (equ-poly? p1 p2)
    (define (iter L1 L2)
      (cond ((and (empty-termlist? L1)
                  (empty-termlist? L2))
             #t)
            ((empty-termlist? L1)
             (zero-termlist? L2))
            ((empty-termlist? L2)
             (zero-termlist? L1))
            (else
             (let ((t1 (first-term L1)) (t2 (first-term L2)))
               (cond ((> (order t1) (order t2))
                      (if (=zero? (coeff t1))
                          (iter (rest-terms L1) L2)
                          #f))
                     ((< (order t1) (order t2))
                      (if (=zero? (coeff t2))
                          (iter L1 (rest-terms L2))
                          #f))
                     (else
                      (if (equ? (coeff t1) (coeff t2))
                          (iter (rest-terms L1) (rest-terms L2))
                          #f)))))))

    (iter (term-list p1) (term-list p2)))

  (define (zero-poly? p)
    (define (iter terms)
      (if (null? terms)
          #t
          (let ((term (first-term terms)))
            (if (=zero? (coeff term))
                (iter (rest-terms terms))
                #f))))

    (iter (term-list p)))

  (define (apply-op-poly op error-msg p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (op (term-list p1)
                       (term-list p2)))
        (error error-msg
               (list p1 p2))))

  (define (add-poly p1 p2)
    (apply-op-poly add-terms
                   "Polynomials of different variables -- ADD-POLY"
                   p1 p2))

  (define (sub-poly p1 p2)
    (apply-op-poly sub-terms
                   "Polynomials of different variables -- SUB-POLY"
                   p1 p2))

  (define (mul-poly p1 p2)
    (apply-op-poly mul-terms
                   "Polynomials of different variables -- MUL-POLY"
                   p1 p2))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())

  (define (first-term term-list)
    (car term-list))

  (define (rest-terms term-list)
    (cdr term-list))

  (define (empty-termlist? term-list)
    (null? term-list))

  (define (zero-termlist? term-list)
    (if (null? term-list)
        #t
        (let ((term (first-term term-list)))
          (if (=zero? (coeff term))
              (zero-termlist? (rest-terms term-list))
              #f))))

  (define (make-term order coeff)
    (list order coeff))

  (define (order term)
    (car term))

  (define (coeff term)
    (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L1) (reverse-terms-sign L2))
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     (make-term (order t2)
                                (reverse-sign (coeff t2)))
                     (sub-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (reverse-terms-sign terms)
    (if (null? terms)
       '()
        (let ((term (first-term terms)))
          (adjoin-term (make-term (order term)
                                  (reverse-sign (coeff term)))
                       (reverse-terms-sign (rest-terms terms))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

  (put 'make 'polynomial
    (lambda (var terms) (tag (make-poly var terms))))
  (put 'equ? '(polynomial polynomial) equ-poly?)
  (put 'zero? '(polynomial) zero-poly?)
  (put 'reverse-sign '(polynomial)
    (lambda (x) (tag (make-poly (variable x) 
                                (reverse-terms-sign (term-list x))))))
  (put 'add '(polynomial polynomial)
    (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
    (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
    (lambda (p1 p2) (tag (mul-poly p1 p2))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (=zero? x)
  (apply-generic 'zero? x))

(install-polynomial-package)

(check-true (=zero? (make-integer 0)))
(check-true (=zero? (make-rational 0 5)))
(check-true (=zero? (make-real 0)))
(check-true (=zero? (make-complex-from-real-imag 0 0)))
(check-true (=zero? (make-complex-from-real-imag (make-real 0) (make-rational 0 5))))
(check-true (=zero? (make-polynomial 'x '())))
(check-true (=zero? (make-polynomial 'x '((1 0)))))
(check-true (=zero? (make-polynomial 'x '((100 0) (10 0) (1 0)))))
(check-true (=zero? (make-polynomial 'x  (list '(1 0)
                                                (list 2 (make-integer 0))
                                                (list 3 (make-real 0))
                                                (list 4 (make-rational 0 1))
                                                (list 5 (make-complex-from-real-imag 0 0))))))
(check-false (=zero? (make-integer 1)))
(check-false (=zero? (make-real 1)))
(check-false (=zero? (make-rational 1 1)))
(check-false (=zero? (make-complex-from-real-imag 0 1)))
(check-false (=zero? (make-complex-from-real-imag (make-real 0) (make-rational 1 2))))
(check-false (=zero? (make-polynomial 'x '((0 1)))))
(check-false (=zero? (make-polynomial 'x  (list (list 2 (make-rational 1 2))))))
(check-false (=zero? (make-polynomial 'x  (list '(1 0)
                                                 (list 2 (make-integer 0))
                                                 (list 3 (make-real 0))
                                                 (list 4 (make-rational 0 1))
                                                 (list 5 (make-complex-from-real-imag 0 1))))))

(define p1 (make-polynomial 'x '((100 5) (10 5) (1 0))))
(define p2 (make-polynomial 'x '((100 0) (10 5) (1 0))))
(define p3 (make-polynomial 'x '((100 0) (10 0) (1 5))))

(check-equal? (equ? (make-polynomial 'x '()) (make-polynomial 'x '())) #t)
(check-equal? (equ? (make-polynomial 'x '((10 0) (5 1) (3 0) (2 0)))
                    (make-polynomial 'x '((100 0) (5 1)))) #t)
(check-equal? (equ? (make-polynomial 'x '((2 1) (1 4) (0 16)))
                    (make-polynomial 'x '((2 1) (1 4) (0 16)))) #t)
(check-equal? (equ? (add p1 p2) (make-polynomial 'x '((100 5) (10 10)))) #t)
(check-equal? (equ? (add p1 p3) (make-polynomial 'x '((100 5) (10 5) (1 5)))) #t)

(check-equal? (reverse-sign int) (make-integer -10))
(check-equal? (reverse-sign rat) (make-rational -10 1))
(check-equal? (reverse-sign real) (make-real -10.0))
(check-equal? (reverse-sign 0) (make-real 0))
(check-equal? (reverse-sign 10) (make-real -10))
(check-equal? (reverse-sign -10) (make-real 10))
(check-equal? (equ? (reverse-sign complex) (make-complex-from-real-imag -10 0)) #t)
(check-equal? (equ? (reverse-sign p1) (make-polynomial 'x '((100 -5) (10 -5)))) #t)
(check-equal? (equ? (reverse-sign p2) (make-polynomial 'x '((10 -5)))) #t)
(check-equal? (equ? (reverse-sign p3) (make-polynomial 'x '((1 -5)))) #t)

(check-equal? (equ? (sub p1 p2) (make-polynomial 'x '((100 5)))) #t)
(check-equal? (equ? (sub p2 p1) (make-polynomial 'x '((100 -5)))) #t)
(check-equal? (equ? (sub p1 p3) (make-polynomial 'x '((100 5) (10 5) (1 -5)))) #t)
(check-equal? (equ? (sub p3 p1) (make-polynomial 'x '((100 -5) (10 -5) (1 5)))) #t)
(check-equal? (equ? (sub p2 p3) (make-polynomial 'x '((10 5) (1 -5)))) #t)
(check-equal? (equ? (sub p3 p2) (make-polynomial 'x '((10 -5) (1 5)))) #t)
(check-equal? (equ? (sub p1 p1) (make-polynomial 'x '())) #t)
