#lang racket

#|
  Упражнение 3.31

  Внутренняя процедура accept-action-procedure!, определенная в make-wire, требует, чтобы в момент,
  когда процедура-действие добавляется к проводу, она немедленно исполнялась. Объясните, зачем требуется
  такая инициализация. В частности, проследите работу процедуры half-adder из этого текста и скажите,
  как отличалась бы реакция системы, если бы accept-action-procedure! была определена как

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))
|#

#|
  Такая инициализация требуется затем, что в противном случае action procedure отработают (а для этого
  события должны быть добавлены в agenda) только при изменении части входящих и промежуточных сигналов
  на противоположные, а это может изменить порядок срабатывания преобразователей сигнала и привести к
  неверным результатам.

  Посмотрим что произойдёт с half-adder, если убрать инициализацию при добавлении action procedure.

  Код процедуры half-adder:

    (define (half-adder a b s c)
      (let ((d (make-wire))
            (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s))
      'ok)

  Если сигнал в проводах a и b равен изначальному, то не сработают вообще все преобразователи.

  Если изменить сигнал a или b на 1 после инициализации, то сработает (or-gate a b d), (and-gate a b c)
  и (and-gate d e s), но не (inverter c e) (т.к. значение сигнала в проводе c останется равно 0).

  И т.д.
|#

(#%require rackunit)

(define (make-queue) (mcons '() '()))

(define (front-ptr queue) (mcar queue))

(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item) (set-mcar! queue item))

(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))
      ;(proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))

    dispatch))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (mcons 0 '()))

(define (current-time agenda) (mcar agenda))

(define (set-current-time! agenda time)
  (set-mcar! agenda time))

(define (segments agenda) (mcdr agenda))

(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))

(define (first-segment agenda) (mcar (segments agenda)))

(define (rest-segments agenda) (mcdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define the-agenda (make-agenda))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (when (empty-queue? q)
          (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr! segments
                         (mcons (make-new-time-segment time action)
                                (mcdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (mcons (make-new-time-segment time action)
                              segments))
        (add-to-segments! segments))))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define inverter-delay 5)
(define or-gate-delay 5)
(define and-gate-delay 5)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Wrong signal -- LOGICAL-NOT" s))))

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Wrong signal -- LOGICAL-AND" s1 s2))))

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Wrong signal -- LOGICAL-OR" s1 s2))))

(define (inverter input output)
  (define (invert-input)
    (display 'inverter-action-procedure)
    (newline)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (display 'inverter-action-procedure-work)
                     (newline)
                     (set-signal! output new-value)))))

  (add-action! input invert-input)
  'ok)

(define (and-gate w1 w2 output)
  (define (and-action-procedure)
    (display 'and-action-procedure)
    (newline)
    (let ((new-value (logical-and (get-signal w1)
                                  (get-signal w2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (display 'and-action-procedure-work)
                     (newline)
                     (set-signal! output new-value)))))

  (add-action! w1 and-action-procedure)
  (add-action! w2 and-action-procedure)
  'ok)

(define (or-gate w1 w2 output)
  (define (or-action-procedure)
    (display 'or-action-procedure)
    (newline)
    (let ((new-value (logical-or (get-signal w1)
                                 (get-signal w2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (display 'or-action-procedure-work)
                     (newline)
                     (set-signal! output new-value)))))

  (add-action! w1 or-action-procedure)
  (add-action! w2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s))
  'ok)

(define a1 (make-wire))
(define b1 (make-wire))
(define s1 (make-wire))
(define c1 (make-wire))

(half-adder a1 b1 s1 c1)
(propagate)

(check-equal? (get-signal a1) 0)
(check-equal? (get-signal b1) 0)
(check-equal? (get-signal s1) 0)
(check-equal? (get-signal c1) 0)

(set-signal! a1 1)
(propagate)

(check-equal? (get-signal a1) 1)
(check-equal? (get-signal b1) 0)
(check-not-equal? (get-signal s1) 1) ; !
(check-equal? (get-signal c1) 0)

(define a2 (make-wire))
(define b2 (make-wire))
(define s2 (make-wire))
(define c2 (make-wire))

(half-adder a2 b2 s2 c2)
(set-signal! a2 1)
(set-signal! b2 1)
(propagate)

(check-equal? (get-signal a2) 1)
(check-equal? (get-signal b2) 1)
(check-equal? (get-signal s2) 0)
(check-equal? (get-signal c2) 1)
