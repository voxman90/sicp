#lang racket

#|
  Упражнение 3.4

  Модифицируйте процедуру make-account из упражнения 3.3 добавив еще одну локальную переменную, так,
  чтобы, если происходит более семи попыток доступа подряд с неверным паролем, вызывалась процедура
  call-the-cops (вызвать полицию).
|#

(#%require rackunit)

(define (make-accumulator balance)
  (define (accumulator op amount)
    (let ((abs-amount (abs amount)))
      (cond ((eq? op 'add) (set! balance (+ balance abs-amount))
                           balance)
            ((eq? op 'sub) (if (> abs-amount balance)
                               (error "Insufficient funds" abs-amount)
                               (begin
                                 (set! balance (- balance abs-amount))
                                 balance)))
            (else balance))))

  accumulator)

(define (make-account balance password)
  (let ((accumulator (make-accumulator balance))
        (failed-login-attempts 0))

    (define (call-the-cops) (error "Cops called"))

    (define (account entered-password op)
      (cond ((>= failed-login-attempts 7) (call-the-cops))
            ((eq? password entered-password)
             (set! failed-login-attempts 0)
             (cond ((eq? 'withdraw op)
                    (lambda (outcome) (accumulator 'sub outcome)))
                   ((eq? 'deposit op)
                    (lambda (income) (accumulator 'add income)))
                   (else (accumulator 'balance 0))))
            (else (set! failed-login-attempts (+ failed-login-attempts 1))
                  (error "Wrong password"))))

  account))

(define account1 (make-account 100 'qwerty))

(check-exn exn:fail? (thunk (account1 '1234567 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '1234576 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '1234567 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '1234657 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '1234756 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '1234765 'balance)) "Wrong password")
(check-equal? (account1 'qwerty 'balance) 100)
(check-exn exn:fail? (thunk (account1 '1237456 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '1237465 'balance)) "Wrong password")
(check-equal? (account1 'qwerty 'balance) 100)
(check-exn exn:fail? (thunk (account1 '1 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '12 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '123 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '1234 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '12345 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '123456 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '1234567 'balance)) "Wrong password")
(check-exn exn:fail? (thunk (account1 '12345678 'balance)) "Cops called")
(check-exn exn:fail? (thunk (account1 'qwerty 'balance)) "Cops called")
