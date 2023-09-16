#lang racket

#|
  Упражнение 2.85

  В этом разделе упоминался метод «упрощения» объекта данных путем спуска его по башне насколько возможно
  вниз. Разработайте процедуру drop, которая делает это для башни, описанной в упражнении 2.83. Ключ к
  задаче состоит в том, что надо решить некоторым общим способом, можно ли понизить объект в типе. Например,
  комплексное число 1.5 + 0i можно опустить до real, комплексное число 1 + 0i до integer, а комплексное
  число 2 + 3i никуда понизить нельзя. Вот план того, как определить, можно ли понизить объект: для начала
  определите обобщенную операцию project, которая «сталкивает» объект вниз по башне. Например, проекция
  комплексного числа будет состоять в отбрасывании его мнимой части. Тогда число можно сдвинуть вниз в
  том случае, если, спроецировав его, а затем подняв обратно до исходного типа, мы получаем нечто, равное
  исходному числу. Покажите как реализовать эту идею в деталях, написав процедуру drop, которая опускает
  объект как можно ниже. Потребуется разработать различные операции проекции и установить project в системе
  в качестве обобщенной операции. Вам также потребуется обобщенный предикат равенства, подобный описанному
  в упражнении 2.79. Наконец, используя drop, перепишите apply-generic из упражнения 2.84, чтобы она
  «упрощала» свои результаты.
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

; Решение упражения

(define types-tower (list 'integer 'rational 'real 'complex))

(define reversed-types-tower (reverse types-tower))

(define (update-integer-package)
  (define (integer->rational int)
    ((get 'make 'rational) (contents int) 1))

  (put 'equ? '(integer integer)
    (lambda (x y) (= x y)))

  (put-coercion 'integer 'rational integer->rational)
  'done)

(define (update-rational-package)
  (define (numer rat) (apply-generic 'numer rat))
  (define (denom rat) (apply-generic 'denom rat))

  (define (rational->integer rat)
    ((get 'make 'integer) (exact-round (/ (numer rat)
                                          (denom rat)))))

  (define (rational->real rat)
    ((get 'make 'real) (* 1.0
                          (/ (numer rat) (denom rat)))))

  (put 'equ? '(rational rational)
    (lambda (x y)
      (and (= (car x) (car y))
           (= (cdr x) (cdr y)))))
           
  (put-coercion 'rational 'integer rational->integer)
  (put-coercion 'rational 'real    rational->real)
  'done)

(define (update-real-package)
  (define rationalize-tolerance 1/100000)

  (define (real->rational n)
    (let ((rat-n (rationalize (contents n) rationalize-tolerance)))
      ((get 'make 'rational) (exact-round (numerator rat-n))
                             (exact-round (denominator rat-n)))))

  (define (real->complex n)
    ((get 'make-from-real-imag 'complex) (contents n) 0))

  (put 'equ? '(real real)
    (lambda (x y) (= x y)))
    
  (put-coercion 'real 'rational real->rational)
  (put-coercion 'real 'complex  real->complex)
  'done)

(define (update-complex-package)
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))

  (define (complex->real complex)
    ((get 'make 'real) (real-part complex)))
  
  (put 'equ? '(complex complex)
    (lambda (x y)
      (and (= (real-part x) (real-part y))
           (= (imag-part x) (imag-part y)))))

  (put-coercion 'complex 'real complex->real)
  'done)

(update-integer-package)
(update-rational-package)
(update-real-package)
(update-complex-package)

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

(check-equal? (raise int) rat)
(check-equal? (raise rat) real)
(check-equal? (raise real) complex)
(check-equal? (raise complex) #f)

(check-equal? (project int) #f)
(check-equal? (project rat) int)
(check-equal? (project real) rat)
(check-equal? (project complex) real)

(check-equal? (drop int) int)
(check-equal? (drop rat) int)
(check-equal? (drop real) int)
(check-equal? (drop complex) int)

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
