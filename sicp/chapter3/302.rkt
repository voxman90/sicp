#lang sicp

#|
  Упражнение 3.2

  При тестировании программ удобно иметь возможность подсчитывать, сколько раз за время вычислений была
  вызвана та или иная процедура. Напишите процедуру make-monitored, принимающую в качестве параметра
  процедуру f, которая сама по себе принимает один входной параметр. Результат, возвращаемый make-monitored
  — третья процедура, назовем ее mf, которая подсчитывает, сколько раз она была вызвана, при помощи
  внутреннего счетчика. Если на входе mf получает специальный символ how-many-calls?, она возвращает
  значение счетчика. Если же на вход подается специальный символ reset-count, mf обнуляет счетчик. Для
  любого другого параметра mf возвращает результат вызова f с этим параметром и увеличивает счетчик.
  Например, можно было бы сделать отслеживаемую версию процедуры sqrt:

    (define s (make-monitored sqrt))
    (s 100)
    10
    (s 'how-many-calls?)
    1
|#

(#%require rackunit)

(define (make-monitored proc)
  (let ((call-times 0))
    (define (mf arg)
      (cond ((eq? arg 'how-many-calls?) call-times)
            ((eq? arg 'reset-count) (set! call-times 0) call-times)
            (else
              (set! call-times (+ call-times 1))
              (proc arg))))

    mf))

(define monitored-identity (make-monitored identity))
(define monitored-identity-alt (make-monitored identity))

(check-equal? (monitored-identity 'how-many-calls?) 0)
(check-equal? (monitored-identity-alt 'symbol) 'symbol)
(check-equal? (monitored-identity-alt 'symbol) 'symbol)
(check-equal? (monitored-identity-alt 'symbol) 'symbol)
(check-equal? (monitored-identity-alt 'symbol) 'symbol)
(check-equal? (monitored-identity-alt 'symbol) 'symbol)
(check-equal? (monitored-identity 'how-many-calls?) 0)
(check-equal? (monitored-identity-alt 'how-many-calls?) 5)
(check-equal? (monitored-identity-alt 'reset-count) 0)
(check-equal? (monitored-identity-alt 'how-many-calls?) 0)
