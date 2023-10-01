#lang racket

#|
  Упражнение 3.22

  Вместо того, чтобы представлять очередь как пару указателей, можно построить ее в виде процедуры с
  внутренним состоянием. Это состояние будет включать указатели на начало и конец обыкновенного списка.
  Таким образом, make-queue будет иметь вид

  (define (make-queue)
    (let ((front-ptr ...)
          (rear-ptr ...))
      <definitions of internal procedures>
      (define (dispatch m) ...)
      dispatch))

  Закончите определение make-queue и реализуйте операции над очередями с помощью этого представления.
|#

(#%require rackunit)

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-mcdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (mcdr front-ptr)))))

    (define (dispatch operation)
      (cond ((eq? operation 'insert-queue!) insert-queue!)
            ((eq? operation 'delete-queue!) (delete-queue!))
            ((eq? operation 'front-queue) (front-queue))
            (else (error "Unknown operation -- QUEUE" operation))))

    dispatch))

(define q (make-queue))

((q 'insert-queue!) 'a)
(check-equal? (q 'front-queue) 'a)

((q 'insert-queue!) 'b)
(check-equal? (q 'front-queue) 'a)

(q 'delete-queue!)
(check-equal? (q 'front-queue) 'b)
