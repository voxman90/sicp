#lang racket

#|
  Упражнение 2.81

  Хьюго Дум заметил, что apply-generic может пытаться привести аргументы к типу друг друга даже тогда,
  когда их типы и так совпадают. Следовательно, решает он, нам нужно вставить в таблицу приведения
  процедуры, которые «приводят» аргументы каждого типа к нему самому. Например, в дополнение к приведению
  scheme-number->complex, описанному выше, он бы написал еще:

    (define (scheme-number->scheme-number n) n)
    (define (complex->complex z) z)
    (put-coercion 'scheme-number 'scheme-number
                   scheme-number->scheme-number)
    (put-coercion 'complex 'complex complex->complex)

  а. Если установлены процедуры приведения типов, написанные Хьюго, что произойдет, когда apply-generic
  будет вызвана с двумя аргументами типа scheme-number или двумя аргументами типа complex для операции,
  которая не находится в таблице для этих типов? Допустим, например, что мы определили обобщенную процедуру
  возведения в степень:

    (define (exp x y) (apply-generic 'exp x y))

  и добавили процедуру возведения в степень в пакет scheme-number и ни в какой другой:

    ;; Следующие строки добавляются в пакет scheme-number
    (put 'exp '(scheme-number scheme-number)
         (lambda (x y) (tag (expt x y)))) ; using primitive expt

  Что произойдет, если мы позовем exp с двумя комплексными числами в качестве аргументов?

  б. Прав ли Хьюго, что нужно что-то сделать с приведением однотипных аргументов, или все и так работает
  правильно?

  в. Измените apply-generic так, чтобы она не пыталась применить приведение, если у обоих аргументов
  один и тот же тип.
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

(define (update-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

  (put 'exp '(scheme-number scheme-number)
    (lambda (x y) (tag (expt x y)))))

(update-scheme-number-package)

(define type-coercion-table (make-table))

(define put-coercion (type-coercion-table 'insert-proc!))

(define get-coercion (type-coercion-table 'lookup-proc))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "There is no method for this types -- APPLY-GENERIC"
                                (list op type-tags))))))
              (error "There is no method for this typea -- APPLY-GENERIC"
                     (list op type-tags)))))))

(define (scheme-number->scheme-number n) n)

(define (complex->complex z) z)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (exp x y) (apply-generic 'exp x y))

(put-coercion 'scheme-number 'complex scheme-number->complex)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

#|
  а. Если установлены процедуры приведения типов, написанные Хьюго, что произойдет, когда apply-generic
  будет вызвана с двумя аргументами типа scheme-number или двумя аргументами типа complex для операции,
  которая не находится в таблице для этих типов?
|#

#|
  В этом случае процедура apply-generic зациклится. Если в таблице не существует операции для аргументов
  соответствующего типа, то процедура apply-generic версии Хьюго попытается привести типы аргументов.
  Будет последовательно проверена возможность преобразовать первый аргумент к типу второго, а после
  возможность преобразовать второй аргумент к типу первого. Но так как у нас аргументы одного типа и есть
  процедура преобразующая тип в самого себя (например, complex->complex), то apply-generic будет повторно
  вызывана с теми же аргументами и повторно попытается их преобразовать и так до бесконечности.

  Собственно, именно такое поведение и наблюдается, когда мы пытается вызывать обобщённую операцию exp
  с двумя аргументами типа complex.
|#

(define schemenum1 (make-scheme-number 3))
(define schemenum2 (make-scheme-number 2))

(check-equal? (exp schemenum1 schemenum2) 9)

(define complex1 (make-complex-from-real-imag 10 5))
(define complex2 (make-complex-from-real-imag 2 7))

;(exp complex1 complex2) - а это зацикливается

#|
  б. Прав ли Хьюго, что нужно что-то сделать с приведением однотипных аргументов, или все и так работает
  правильно?
|#

#|
  Прав, но решение Хьюго только усугубит проблему. Процедура должна пытаться преобразовать аргументы к
  тому типу для которого существует версия операции, если это возможно. Одно из возможных решений - это
  построения графа преобразования типов, извлечение поддерживаемых сигнатур типов для интересующей нас
  операции и поиск спобов преобразования аргументов по графу. Если таких путей несколько, то выбирать
  наикратчайший (или предпочитаемый, если таковой указан). Но на самом базовом уровне нужно хотя бы
  проверять перед попыткой приведения, что типы аргументов не совпадают.
|#

#|
  в. Измените apply-generic так, чтобы она не пыталась применить приведение, если у обоих аргументов
  один и тот же тип.
|#

(define (apply-generic-alt op . args)
  (define (send-error op type-tags)
    (error "There is no method for this types -- APPLY-GENERIC"
           (list op type-tags)))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (send-error op type-tags))))
                    (send-error op type-tags)))
              (send-error op type-tags))))))

(define (exp-alt x y) (apply-generic-alt 'exp x y))

(check-equal? (exp-alt schemenum1 schemenum2) 9)

#|
  Теперь процедура не пытается привести одинаковые типы и не зацикливается, а завершается с ошибкой:
  "There is no method for this types -- APPLY-GENERIC '(exp (complex complex))"
|#

(check-exn exn:fail? (thunk (exp-alt complex1 complex2)))
