#lang racket

#|
  Упражнение 2.93

  Модифицируйте пакет арифметики рациональных чисел, заставив его пользоваться обобщенными операциями,
  но при этом измените make-rat, чтобы она не пыталась сокращать дроби. Проверьте систему, применив
  make-rational к двум многочленам, и получив рациональную функцию

    (define p1 (make-polynomial 'x '((2 1)(0 1))))

    (define p2 (make-polynomial 'x '((3 1)(0 1))))

    (define rf (make-rational p2 p1))

  Сложите теперь rf саму с собой, используя add. Вы увидите, что процедура сложения не приводит дроби
  к наименьшему знаменателю.
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
        ((number? datum) datum)
        (else (error "Incorrect tagged data -- CONTENTS" datum))))

; Процедуры для приведения типов

(define types-tower (list 'integer 'real 'complex 'polynomial 'rational))

(define reversed-types-tower (reverse types-tower))

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

; Пакет арифметических операций над целыми числами

(define (install-integer-package)
  (define (tag n) (attach-tag 'integer n))

  (define (make-int n)
    (if (exact-integer? n)
        (tag n)
        (error "Incorrect argument -- MAKE-INT" n)))

  (define (div x y)
    (let ((q (/ x y)))
      (if (exact-integer? q)
          (tag q)
          q)))

  (define (integer->rational n)
    ((get 'make 'rational) n 1))

  (define (integer->real n)
    ((get 'make 'real) n))

  (put 'make 'integer make-int)
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
  (put 'sqrt '(integer)
    (lambda (x) (sqrt x)))
  (put 'cos '(integer)
    (lambda (x) (cos x)))
  (put 'sin '(integer)
    (lambda (x) (sin x)))
  (put 'atan '(integer integer)
    (lambda (x y) (atan x y)))

  (put-coercion 'integer 'rational
    (lambda (int) (integer->rational (contents int))))
  (put-coercion 'integer 'real
    (lambda (int) (integer->real (contents int))))
  'done)

; Пакеты арифметических операций над вещественными числами

(define (install-real-package)
  (define (make-real n)
    (if (real? n)
        n
        (error "Incorrect argument -- MAKE-REAL" n)))

  (define (real->integer n)
    ((get 'make 'integer) (exact-round n)))

  (define (real->complex n)
    ((get 'make-from-real-imag 'complex) n 0))

  (put 'make 'real make-real)
  (put 'equ? '(real real)
    (lambda (x y) (= x y)))
  (put 'zero? '(real)
    (lambda (x) (= x 0)))
  (put 'reverse-sign '(real)
    (lambda (x) (* -1 x)))
  (put 'add '(real real)
    (lambda (x y) (+ x y)))
  (put 'sub '(real real)
    (lambda (x y) (- x y)))
  (put 'mul '(real real)
    (lambda (x y) (* x y)))
  (put 'div '(real real)
    (lambda (x y) (/ x y)))
  (put 'sqrt '(real)
    (lambda (x) (sqrt x)))
  (put 'cos '(real)
    (lambda (x) (cos x)))
  (put 'sin '(real)
    (lambda (x) (sin x)))
  (put 'atan '(real real)
    (lambda (x y) (atan x y)))

  (put-coercion 'real 'integer
    (lambda (real) (real->integer real)))
  (put-coercion 'real 'complex
    (lambda (real) (real->complex real)))
  'done)

; Пакеты арифметических операций над комплексными числами в полярном и декартовом представлении

(define (install-polar-package)
  (define (tag x) (attach-tag 'polar x))

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (real-part z)
    (mul (magnitude z)
         (cosine (angle z))))

  (define (imag-part z)
    (mul (magnitude z)
         (sine (angle z))))

  (define (make-from-mag-ang r a) (cons r a))

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

  (define (magnitude z)
    (square-root (add (square (real-part z))
                      (square (imag-part z)))))

  (define (angle z)
    (arctg (imag-part z)
           (real-part z)))

  (define (make-from-real-imag x y) (cons x y))

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
    (make-from-real-imag (sub (mul (real-part z1)
                                   (real-part z2))
                              (mul (imag-part z1)
                                   (imag-part z2)))
                         (add (mul (real-part z1)
                                   (imag-part z2))
                              (mul (real-part z2)
                                   (imag-part z1)))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1)
                              (magnitude z2))
                         (sub (angle z1)
                              (angle z2))))

  (define (complex->real complex)
    ((get 'make 'real) (real-part complex)))

  (define (complex->polynomial complex)
    ((get 'make 'polynomial) 'x (list (list 0 complex))))

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

  (put-coercion 'complex 'real complex->real)
  (put-coercion 'complex 'polynomial complex->polynomial)
  'done)

; Пакеты арифметических операций над многочленами

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

  ; Решение упражнения:

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((quotent-rest-list (div-terms (term-list p1)
                                            (term-list p2))))
          (list (tag (make-poly (variable p1)
                                (car quotent-rest-list)))
                (tag (make-poly (variable p2)
                                (cadr quotent-rest-list)))))
        (error "Polynomials of different variables -- DIV-POLY"
               (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (sub-terms
                                   L1 (mul-terms
                                       L2 (adjoin-term (make-term new-o
                                                                  new-c)
                                                       (the-empty-termlist))))
                       L2)))
                  (list (cons (make-term new-o new-c)
                              (car rest-of-result))
                        (cadr rest-of-result))))))))

  (define (polynomial->rational poly)
    ((get 'make 'rational) poly 1))

  (define (free-coeff p)
    (define (last-coeff TL)
      (let ((tail (rest-terms TL)))
        (if (empty-termlist? tail)
            (first-term TL)
            (last-coeff tail))))

    (let ((TL (term-list p)))
      (if (zero-termlist? TL)
          0
          (let ((last-c (last-coeff TL)))
            (if (= (order last-c) 0)
                (coeff last-c)
                0)))))

  (define (polynomial->complex poly)
    (let ((free-c (free-coeff (term-list poly))))
      (let ((free-coeff-type (type-tag free-c)))
        (cond ((eq? free-coeff-type 'complex) free-c)
              ((eq? free-coeff-type 'polynomial) (polynomial->complex free-c))
              (else (shift-to free-c 'complex))))))

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
  (put 'div '(polynomial polynomial)
    (lambda (p1 p2) (div-poly p1 p2)))

  (put-coercion 'polynomial 'complex polynomial->complex)
  (put-coercion 'polynomial 'rational polynomial->rational)
  'done)

; Пакет арифметических операций над дробями

(define (install-rational-package)
  (define (tag x) (attach-tag 'rational x))
  (define (numer contents) (car contents))
  (define (denom contents) (cdr contents))

  (define (make-rat n d)
    (if (not (=zero? d))
        (tag (cons n d))
        (error "Incorrect argument -- MAKE-RAT" n d)))

  (define (equ-rat? x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))

  (define (=zero-rat? rat)
    (and (=zero? (numer rat))
         (not (=zero? (denom rat)))))

  (define (reverse-sign-rat rat)
    (make-rat (reverse-sign (numer rat))
              (denom rat)))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (rational->polynomial tagged-rat)
    (let ((rat (contents tagged-rat)))
      (let ((div-res (div (numer rat) (denom rat)))
            (numer-type (type-tag (numer rat))))
        (cond ((eq? numer-type 'polynomial) (car div-res))
              (else (shift-to div-res 'polynomial))))))

  (put 'make 'rational make-rat)
  (put 'equ? '(rational rational) equ-rat?)
  (put 'zero? '(rational) =zero-rat?)
  (put 'reverse-sign '(rational) reverse-sign-rat)
  (put 'add '(rational rational) add-rat)
  (put 'sub '(rational rational) sub-rat)
  (put 'mul '(rational rational) mul-rat)
  (put 'div '(rational rational) div-rat)
  (put-coercion 'rational 'polynomial rational->polynomial)
  'done)

; Обобщённые процедуры

(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-real n)
  ((get 'make 'real) n))

(define (make-complex-from-real-imag r i)
  ((get 'make-from-real-imag 'complex) r i))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (angle z)
  (apply-generic 'angle z))

(define (=zero? datum)
  (apply-generic 'zero? datum))

(define (equ? datum1 datum2)
  (apply-generic 'equ? datum1 datum2))

(define (add datum1 datum2)
  (apply-generic 'add datum1 datum2))

(define (sub datum1 datum2)
  (apply-generic 'sub datum1 datum2))

(define (mul datum1 datum2)
  (apply-generic 'mul datum1 datum2))

(define (div datum1 datum2)
  (apply-generic 'div datum1 datum2))

(define (square datum)
  (mul datum datum))

(define (cosine datum)
  (apply-generic 'cos datum))

(define (sine datum)
  (apply-generic 'sin datum))

(define (arctg datum1 datum2)
  (apply-generic 'atan datum1 datum2))

(define (square-root datum)
  (apply-generic 'sqrt datum))

(define (reverse-sign datum)
  (apply-generic 'reverse-sign datum))

(install-integer-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-package)
(install-rational-package)

; Тесты

(define int (make-integer 10))
(define real (make-real 10))
(define complex (make-complex-from-real-imag 10 0))
(define poly (make-polynomial 'x '((0 10))))
(define rat (make-rational poly 1))

(check-true (equ? (raise int) real))
(check-true (equ? (raise real) complex))
(check-true (equ? (raise complex) poly))
(check-true (equ? (raise poly) rat))
(check-false (raise rat))

(check-false (project int))
(check-true (equ? (project real) int))
(check-true (equ? (project complex) real))
(check-true (equ? (project poly) complex))
(check-true (equ? (project rat) poly))

(check-true (equ? (drop int) int))
(check-true (equ? (drop real) int))
(check-true (equ? (drop complex) int))
(check-true (equ? (drop poly) int))
(check-true (equ? (drop rat) int))

(check-true (equ? int int))
(check-true (equ? int real))
(check-true (equ? int complex))
(check-true (equ? int poly))
(check-true (equ? int rat))

(check-true (equ? real real))
(check-true (equ? real int))
(check-true (equ? real complex))
(check-true (equ? real poly))
(check-true (equ? real rat))

(check-true (equ? complex complex))
(check-true (equ? complex int))
(check-true (equ? complex real))
(check-true (equ? complex poly))
(check-true (equ? complex rat))

(check-true (equ? rat rat))
(check-true (equ? rat int))
(check-true (equ? rat real))
(check-true (equ? rat complex))
(check-true (equ? rat poly))

(define int1 (make-integer 1))
(define int2 (make-integer 2))
(define int1+int2 (make-integer 3))

(check-true (equ? (add int1 int2) int1+int2))

(define rat1 (make-rational 1 1))
(define rat2 (make-rational 5 2))
(define rat1+rat2 (make-rational 7 2))

(check-true (equ? (add rat1 rat2) rat1+rat2))
(check-true (equ? (add int1 rat2) rat1+rat2))
(check-true (equ? (add rat2 rat1) rat1+rat2))

(define int2+rat2 (make-rational 9 2))

(check-true (equ? (add int2 rat2) int2+rat2))
(check-true (equ? (add rat2 int2) int2+rat2))

(define real1 (make-real 1.0))
(define real2 (make-real 2.5))
(define real1+real2 (make-real 3.5))

(check-true (equ? (add real1 real2) real1+real2))

(define int1+real2 (make-real 3.5))

(check-true (equ? (add int1 real2) int1+real2))
(check-true (equ? (add real2 int1) int1+real2))
(check-true (equ? (add rat1 real2) int1+real2))
(check-true (equ? (add real2 rat1) int1+real2))

(define complex1 (make-complex-from-real-imag 1.0 0))
(define complex2 (make-complex-from-real-imag 2.5 1))
(define complex1+complex2 (make-complex-from-real-imag 3.5 1))

(check-true (equ? (add complex1 complex2) complex1+complex2))

(check-true (equ? (add int1 complex2) complex1+complex2))
(check-true (equ? (add complex2 int1) complex1+complex2))

(check-true (equ? (add rat1 complex2) complex1+complex2))
(check-true (equ? (add complex2 rat1) complex1+complex2))
(check-true (equ? (add real1 complex2) complex1+complex2))
(check-true (equ? (add complex2 real1) complex1+complex2))

(define rat3 (make-rational 7 15))
(define complex3 (make-complex-from-real-imag 1.5 7))

(check-true (equ? (drop rat3) rat3))
(check-true (equ? (drop complex3) complex3))

(define rat4 (make-rational 7 2))
(define real4 (make-real 3.5))
(define complex4 (make-complex-from-real-imag 3.5 0))

(check-true (equ? (drop rat4) rat4))
(check-true (equ? (drop real4) real4))
(check-true (equ? (drop complex4) real4))

(define real5 (make-real (/ 1 333)))
(define complex5 (make-complex-from-real-imag (/ 1 333) 0))

(check-true (equ? (drop real5) real5))
(check-true (equ? (drop complex5) real5))

(check-true (equ? (mul int1 int2) '(integer . 2)))
; Должно быть true, но false из-за того, что не реализовано сокращение дробей для rational
(check-false (equ? (mul int2 rat2) '(integer . 5)))

(check-true (=zero? (make-integer 0)))
(check-true (=zero? (make-rational 0 5)))
(check-true (=zero? (make-real 0)))
(check-true (=zero? (make-complex-from-real-imag 0 0)))
(check-false (=zero? (make-integer 1)))
(check-false (=zero? (make-real 1)))
(check-false (=zero? (make-rational 1 1)))
(check-false (=zero? (make-complex-from-real-imag 0 1)))

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

(check-true (equ? (make-polynomial 'x '()) (make-polynomial 'x '())))
(check-true (equ? (make-polynomial 'x '((10 0) (5 1) (3 0) (2 0)))
                    (make-polynomial 'x '((100 0) (5 1)))))
(check-true (equ? (make-polynomial 'x '((2 1) (1 4) (0 16)))
                    (make-polynomial 'x '((2 1) (1 4) (0 16)))))
(check-true (equ? (add p1 p2) (make-polynomial 'x '((100 5) (10 10)))))
(check-true (equ? (add p1 p3) (make-polynomial 'x '((100 5) (10 5) (1 5)))))

(check-true (equ? (reverse-sign int) (make-integer -10)))
(check-true (equ? (reverse-sign rat) (make-rational -10 1)))
(check-true (equ? (reverse-sign real) (make-real -10)))
(check-true (equ? (reverse-sign 0) (make-real 0)))
(check-true (equ? (reverse-sign 10) (make-real -10)))
(check-true (equ? (reverse-sign -10) (make-real 10)))
(check-true (equ? (reverse-sign complex) (make-complex-from-real-imag -10 0)))
(check-true (equ? (reverse-sign p1) (make-polynomial 'x '((100 -5) (10 -5)))))
(check-true (equ? (reverse-sign p2) (make-polynomial 'x '((10 -5)))))
(check-true (equ? (reverse-sign p3) (make-polynomial 'x '((1 -5)))))

(check-true (equ? (sub p1 p2) (make-polynomial 'x '((100 5)))))
(check-true (equ? (sub p2 p1) (make-polynomial 'x '((100 -5)))))
(check-true (equ? (sub p1 p3) (make-polynomial 'x '((100 5) (10 5) (1 -5)))))
(check-true (equ? (sub p3 p1) (make-polynomial 'x '((100 -5) (10 -5) (1 5)))))
(check-true (equ? (sub p2 p3) (make-polynomial 'x '((10 5) (1 -5)))))
(check-true (equ? (sub p3 p2) (make-polynomial 'x '((10 -5) (1 5)))))
(check-true (equ? (sub p1 p1) (make-polynomial 'x '())))

(define poly1 (make-polynomial 'x '((5 1) (0 -1))))
(define poly2 (make-polynomial 'x '((2 1) (0 -1))))
(define poly-div-q (make-polynomial 'x '((3 1) (1 1))))
(define poly-div-r (make-polynomial 'x '((1 1) (0 -1))))

(check-equal? (div poly1 poly2) (list poly-div-q poly-div-r))
(check-true (equ? (sub poly1 (mul poly2 poly-div-q)) poly-div-r))
(check-true (equ? (add poly-div-r (mul poly2 poly-div-q)) poly1))

; Тесты для упражнения

(define pol1 (make-polynomial 'x '((2 1) (0 1))))
(define pol2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational pol2 pol1))

(define sum-numer (make-polynomial 'x '((5 2) (3 2) (2 2) (0 2))))
(define sum-denom (make-polynomial 'x '((4 1) (2 2) (0 1))))
(define rf+rf (make-rational sum-numer sum-denom))

(check-true (equ? (add rf rf) rf+rf))
