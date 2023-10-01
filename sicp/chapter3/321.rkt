#lang racket

#|
  Упражнение 3.21

  Бен Битобор решает протестировать вышеописанную реализацию. Он вводит процедуры в интерпретаторе Лиспа
  и тестирует их:

    (define q1 (make-queue))

    (insert-queue! q1 'a)
    ((a) a)

    (insert-queue! q1 'b)
    ((a b) b)

    (delete-queue! q1)
    ((b) b)

    (delete-queue! q1)
    (() b)

  «Ничего не работает! — жалуется он. — Ответ интерпретатора показывает, что последний элемент попадает
  в очередь два раза. А когда я оба элемента уничтожаю, второе b по-прежнему там сидит, так что очередь
  не становится пустой, хотя должна бы». Ева Лу Атор говорит, что Бен просто не понимает, что происходит.
  «Дело не в том, что элементы два раза оказываются в очереди, — объясняет она. — Дело в том, что
  стандартная лисповская печаталка не знает, как устроено представление очереди. Если ты хочешь, чтобы
  очередь правильно печаталась, придется написать специальную процедуру распечатки очередей». Объясните,
  что имеет в виду Ева Лу. В частности, объясните, почему в примерах Бена на печать выдается именно такой
  результат. Определите процедуру print-queue, которая берет на входе очередь и выводит на печать
  последовательность ее элементов.
|#

(#%require rackunit)

(define (make-queue) (mcons '() '()))

(define (front-ptr queue) (mcar queue))

(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item)
  (set-mcar! queue item))

(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

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

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

#|
  Так происходит, потому что Scheme воспринимает очередь как пару из двух элементов и, соответственно,
  выводит эти элементы целиком. Если это указатель на голову списка, то и выводится список.

  Удаление элементов из очереди не убирает ссылку на последний элемент, поэтому в нашей реализации у
  пустой очереди может быть непустая ссылка на последний элемент, что и наблюдается в примере выше.

  Печаталка должна выводить пустой список, если указатель на голову списка пуст.
|#

(define (print-queue queue op)
  (define back (rear-ptr queue))
  (define (bypass-queue front)
    (if (eq? front back)
        (mcons (mcar front) '())
        (mcons (mcar front) (bypass-queue (mcdr front)))))

  (display
    (if (empty-queue? queue)
       '()
        (bypass-queue (front-ptr queue)))
    op))

(define q1 (make-queue))

(define op1 (open-output-string))
(print-queue q1 op1)
(define display-result (get-output-string op1))
(check-equal? display-result "()")
(close-output-port op1)

(define op2 (open-output-string))
(print-queue (insert-queue! q1 'a) op2)
(define display-result-2 (get-output-string op2))
(check-equal? display-result-2 "{a}")
(close-output-port op2)

(define op3 (open-output-string))
(print-queue (insert-queue! q1 'b) op3)
(define display-result-3 (get-output-string op3))
(check-equal? display-result-3 "{a b}")
(close-output-port op3)

(define op4 (open-output-string))
(print-queue (delete-queue! q1) op4)
(define display-result-4 (get-output-string op4))
(check-equal? display-result-4 "{b}")
(close-output-port op4)

(define op5 (open-output-string))
(print-queue (delete-queue! q1) op5)
(define display-result-5 (get-output-string op5))
(check-equal? display-result-5 "()")
(close-output-port op5)
