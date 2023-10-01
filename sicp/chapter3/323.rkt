#lang racket

#|
  Упражнение 3.23

  Дек (deque, double-ended deque, «двусторонняя очередь») представляет собой последовательность,
  элементы в которой могут добавляться и уничтожаться как с головы, так и с хвоста. На деках определены
  такие операции: конструктор make-deque, предикат empty-deque?, селекторы front-deque и rear-deque, и
  мутаторы front-insert-deque!, rear-insert-deque!, front-delete-deque! и rear-delete-deque!. Покажите,
  как представить дек при помощи пар, и напишите реализацию операций. Все операции должны выполняться
  за Θ(1) шагов.
|#

(#%require rackunit)

(define (make-deque) (mcons '() '()))

(define (front-ptr deque) (mcar deque))

(define (rear-ptr deque) (mcdr deque))

(define (set-front-ptr! deque item)
  (set-mcar! deque item))

(define (set-rear-ptr! deque item)
  (set-mcdr! deque item))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (item-value (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (item-value (rear-ptr deque))))

(define (item-value item)
  (mcar (mcar item)))

(define (item-next item)
  (mcdr (mcar item)))

(define (item-prev item)
  (mcdr item))

(define (set-item-next! item next)
  (set-mcdr! (mcar item) next))

(define (set-item-prev! item prev)
  (set-mcdr! item prev))

(define (make-item value next prev)
  (mcons (mcons value next) prev))

(define (rear-insert-deque! deque value)
  (let ((new-item (make-item value '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else
           (set-item-prev! (rear-ptr deque) new-item)
           (set-item-next! new-item (rear-ptr deque))
           (set-rear-ptr! deque new-item)
           deque))))

(define (front-insert-deque! deque value)
  (let ((new-item (make-item value '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else
           (set-item-prev! new-item (front-ptr deque))
           (set-item-next! (front-ptr deque) new-item)
           (set-front-ptr! deque new-item)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((prev (item-prev (front-ptr deque))))
           (set-front-ptr! deque prev)
           (if (null? prev)
               (set-rear-ptr! deque '())
               (set-item-next! prev '()))
         deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((next (item-next (rear-ptr deque))))
           (set-rear-ptr! deque next)
           (if (null? next)
               (set-front-ptr! deque '())
               (set-item-prev! next '()))
         deque))))

(define dq1 (make-deque))

(check-true (empty-deque? dq1))

(front-insert-deque! dq1 'a)

(check-false (empty-deque? dq1))

(check-equal? (front-deque dq1) 'a)
(check-equal? (rear-deque dq1) 'a)

(rear-insert-deque! dq1 'b)

(check-equal? (front-deque dq1) 'a)
(check-equal? (rear-deque dq1) 'b)

(front-delete-deque! dq1)

(check-equal? (front-deque dq1) 'b)
(check-equal? (rear-deque dq1) 'b)

(front-delete-deque! dq1)

(check-true (empty-deque? dq1))

(define dq2 (make-deque))

(front-insert-deque! dq2 3)
(front-insert-deque! dq2 2)
(front-insert-deque! dq2 1)

(check-equal? (front-deque dq2) 1)
(check-equal? (rear-deque dq2) 3)

(rear-delete-deque! dq2)

(check-equal? (front-deque dq2) 1)
(check-equal? (rear-deque dq2) 2)

(rear-delete-deque! dq2)

(check-equal? (front-deque dq2) 1)
(check-equal? (rear-deque dq2) 1)

(rear-delete-deque! dq2)

(check-true (empty-deque? dq2))
