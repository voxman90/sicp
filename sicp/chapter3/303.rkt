#lang racket

#|
  Упражнение 3.3

  Измените процедуру make-account так, чтобы она создавала счета, защищенные паролем. А именно, make-account
  должна в качестве дополнительного аргумента принимать символ, например:

    (define acc (make-account 100 'secret-password))

  Получившийся объект-счет должен обрабатывать запросы, только если они сопровождаются паролем, с которым
  счет был создан, а в противном случае он должен жаловаться:

    ((acc 'secret-password 'withdraw) 40)
    60
    ((acc 'some-other-password 'deposit) 50)
    "Incorrect password"
|#

(#%require rackunit)

(define (make-accumulator balance)
  (define (accumulator op amount)
    (let ((abs-amount (abs amount)))
      (cond ((eq? op 'add) (set! balance (+ balance abs-amount))
                           balance)
            ((eq? op 'sub) (if (> abs-amount balance)
                               "Insufficient funds"
                               (begin
                                 (set! balance (- balance abs-amount))
                                 balance)))
            (else balance))))

  accumulator)

(define (make-account balance password)
  (let ((accumulator (make-accumulator balance)))
    (define (account entered-password op)
      (if (eq? password entered-password)
          (cond ((eq? 'withdraw op)
                 (lambda (outcome) (accumulator 'sub outcome)))
                ((eq? 'deposit op)
                 (lambda (income) (accumulator 'add income)))
                (else (accumulator 'balance 0)))
          (error "Incorrect password")))

  account))

(define account1 (make-account 100 'qwerty))

(check-equal? (account1 'qwerty 'balance) 100)
(check-equal? ((account1 'qwerty 'deposit) 10) 110)
(check-equal? ((account1 'qwerty 'withdraw) 90) 20)
(check-equal? ((account1 'qwerty 'withdraw) 90) "Insufficient funds")
(check-exn exn:fail? (thunk ((account1 'Qwerty 'deposit) 100)) "Incorrect password")
