#lang racket

#|
  Упражнение 3.32

  Процедуры, предназначенные к выполнению в каждом временном отрезке, хранятся в виде очереди. Таким
  образом, процедуры для каждого отрезка вызываются в том же порядке, в котором они были добавлены к
  плану (первый пришел, первый ушел). Объясните, почему требуется использовать именно такой порядок.
  В частности, проследите поведение И-элемента, входы которого меняются с 0 на 1 и с 1 на 0 одновременно
  и скажите, как отличалось бы поведение, если бы мы хранили процедуры отрезка в обыкновенном списке,
  добавляя и убирая их только с головы (последний пришел, первый ушел).
|#

#|
  Потому что порядок выполнения команд важен, не говоря уже о существовании побочных эффектов.

  Если мы будем использовать стек, то получим обратный порядок действий, что приведёт, в ряде случаев,
  к неверному результату.

  При использовании стека И-элемент, входы которого одновременно меняются с 0 на 1, преобразует сигнал
  к 0 (а должен к 1). Если входы одновременно меняются с 1 на 0, то И-элемент преобразует сигнал к 1 (a
  должен к 0).

  Для случая, когда входы одноврменно меняются с 0 на 1 получаем такую последовательность вызовов:

    (logical-and 1 1)
    (logical-and 1 0)
    (logical-and 0 0)
    (logical-and 0 0)

  Для случая, когда входы одновременно меняются с 1 на 0 получаем такую последовательность вызовов:

    (logical-and 0 0)
    (logical-and 0 1)
    (logical-and 1 1)
    (logical-and 1 1)

  Вызовы при инициализации попадают в один и тот же временной сегмет с вызовами при изменениях сигнала.
  И обрабатываются, из-за того, что используется стек, последними. Даже если использовать (propagate)
  после инициализации и очистить agenda, то в первом случае мы получим такую последовательность
  вызовов:

    (logical-and 1 1)
    (logical-and 1 0)

  и, как следствие, неверный результат 0.
|#

(#%require rackunit
           compatibility/mlist)

(define (make-stack) (mlist '()))

(define (empty-stack? stack) (null? (mcar stack)))

(define (front-stack stack)
  (if (empty-stack? stack)
      (error "FRONT called with an empty stack" stack)
      (mcar (mcar stack))))

(define (delete-stack! stack)
  (cond ((empty-stack? stack)
         (error "DELETE! called with an empty stack" stack))
        (else
         (set-mcar! stack (mcdr (mcar stack)))
         stack)))

(define (insert-stack! stack item)
  (set-mcar! stack (mcons item (mcar stack)))
  stack)

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
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))

    dispatch))

(define (make-time-segment time stack)
  (cons time stack))

(define (segment-time s) (car s))

(define (segment-stack s) (cdr s))

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
  (let ((s (segment-stack (first-segment agenda))))
    (delete-stack! s)
    (when (empty-stack? s)
          (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-stack (segment-stack first-seg)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))

  (define (make-new-time-segment time action)
    (let ((s (make-stack)))
      (insert-stack! s action)
      (make-time-segment time s)))

  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-stack! (segment-stack (mcar segments))
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

(define and-gate-delay 5)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Wrong signal -- LOGICAL-AND" s1 s2))))

(define (and-gate w1 w2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal w1)
                                  (get-signal w2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (add-action! w1 and-action-procedure)
  (add-action! w2 and-action-procedure)
  'ok)

(define a (make-wire))
(define b (make-wire))
(define s (make-wire))

(and-gate a b s)
(propagate)

(set-signal! a 1)
(set-signal! b 1)
(propagate)

(check-equal? (get-signal a) 1)
(check-equal? (get-signal b) 1)
(check-equal? (get-signal s) 0) ; !

(define a1 (make-wire))
(define b1 (make-wire))
(define s1 (make-wire))

(set-signal! a1 1)
(set-signal! b1 1)

(check-equal? (get-signal a1) 1)
(check-equal? (get-signal b1) 1)

(and-gate a1 b1 s1)

(set-signal! a1 0)
(set-signal! b1 0)
(propagate)

(check-equal? (get-signal a1) 0)
(check-equal? (get-signal b1) 0)
(check-equal? (get-signal s1) 1) ; !
