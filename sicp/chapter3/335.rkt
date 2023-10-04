#lang racket

#|
  Упражнение 3.35

  Бен Битобор объясняет Хьюго, что один из способов избежать неприятностей в упражнении 3.34 — определить
  квадратор как новое элементарное ограничение. Заполните недостающие части в Беновой схеме процедуры,
  реализующей такое ограничение:

    (define (squarer a b)
      (define (process-new-value)
        (if (has-value? b)
            (if (< (get-value b) 0)
                (error "square less than 0 -- SQUARER" (get-value b))
                <alternative1>)
            <alternative2>))
      (define (process-forget-value) <body1>)
      (define (me request) <body2>)
      <rest of definition>
      me)
|#

(#%require rackunit)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))

  (loop list))

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))

    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))

    (define (connect new-constraint)
      (when (not (memq new-constraint constraints))
            (set! constraints
                  (cons new-constraint constraints)))
      (when (has-value? me)
            (inform-about-value new-constraint))
      'done)

    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))

  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))

  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))

  (connect connector me)
  (set-value! connector value me)
  me)

(define (squarer x square-x)
  (define (process-new-value)
    (if (has-value? square-x)
        (if (< (get-value square-x) 0)
            (error "square less than 0 -- SQUARER" (get-value square-x))
            (set-value! x
                        (sqrt (get-value square-x))
                        me))
        (when (has-value? x)
              (set-value! square-x
                          (* (get-value x) (get-value x))
                          me))))

  (define (process-forget-value)
    (forget-value! square-x me)
    (forget-value! x me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQAURER" request))))

  (connect x me)
  (connect square-x me)
  me)

(define a (make-connector))
(define square-a (make-connector))

(squarer a square-a)

(set-value! a 1 'test)

(check-equal? (get-value a) 1)
(check-equal? (get-value square-a) 1)

(forget-value! a 'test)
(set-value! a 3 'test)

(check-equal? (get-value a) 3)
(check-equal? (get-value square-a) 9)

(forget-value! a 'test)
(set-value! square-a 81 'test)

(check-equal? (get-value a) 9)
(check-equal? (get-value square-a) 81)

(define b (make-connector))
(define square-b (make-connector))

(squarer b square-b)

(set-value! square-b 100 'test)

(check-equal? (get-value square-b) 100)
(check-equal? (get-value b) 10)
