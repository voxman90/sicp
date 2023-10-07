#lang racket

#|
  Упражнение 3.47

  Семафор (размера n) представляет собой обобщение мьютекса. Подобно мьютексу, семафор поддерживает
  операции захвата и освобождения, но захватить его одновременно могут до n процессов. Прочие процессы,
  которые попытаются захватить семафор, должны будут ждать освобождения. Дайте реализацию семафоров

  а. в терминах мьютексов.

  б. в терминах атомарных операций test-and-set!.
|#

(#%require rackunit
           compatibility/mlist)

(define (clear! cell)
  (set-mcar! cell #f))

(define (test-and-set! cell)
  (if (mcar cell)
      #t
      (begin (set-mcar! cell #t)
             #f)))

(define (make-mutex)
  (let ((cell (mlist #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (when (test-and-set! cell)
               (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))

  the-mutex))

; a

(define (make-semaphore capacity)
  (let ((free-slots capacity)
        (the-mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (when (the-mutex 'acquire)
               (cond ((> free-slots 0)
                      (set! free-slots (- free-slots 1))
                      (the-mutex 'release))
                     (else
                      (the-mutex 'release)
                      (the-semaphore 'acquire)))))
            ((eq? m 'release)
             (when (the-mutex 'acquire)
               (set! free-slots (+ free-slots 1))
               (the-mutex 'release)))
            ((eq? m 'count) free-slots)))

  the-semaphore))

; b

(define (make-semaphore-b capacity)
  (let ((free-slots capacity)
        (cell (mlist #f)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (let ((cell-busy? (test-and-set! cell)))
               (if cell-busy?
                   (the-semaphore 'acquire)
                   (cond ((> free-slots 0)
                          (set! free-slots (- free-slots 1))
                          (clear! cell))
                         (else
                          (clear! cell)
                          (the-semaphore 'acquire))))))
            ((eq? m 'release)
             (let ((cell-busy? (test-and-set! cell)))
               (cond ((not cell-busy?)
                      (set! free-slots (+ free-slots 1))
                      (clear! cell))
                     (else
                      (the-semaphore 'release)))))
            ((eq? m 'count) free-slots)))

  the-semaphore))

(define semaphore-mutex (make-semaphore 2))
; 'count also included to get semaphore state
(check-equal? (semaphore-mutex 'count) 2)
(semaphore-mutex 'acquire)
(check-equal? (semaphore-mutex 'count) 1)
(semaphore-mutex 'release)
(check-equal? (semaphore-mutex 'count) 2)
(semaphore-mutex 'acquire)
(semaphore-mutex 'acquire)
(check-equal? (semaphore-mutex 'count) 0)

(define semaphore-cell (make-semaphore-b 2))
(check-equal? (semaphore-cell 'count) 2)
(semaphore-cell 'acquire)
(check-equal? (semaphore-cell 'count) 1)
(semaphore-cell 'release)
(check-equal? (semaphore-cell 'count) 2)
(semaphore-cell 'acquire)
(semaphore-cell 'acquire)
(check-equal? (semaphore-cell 'count) 0)
