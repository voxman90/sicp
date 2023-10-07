#lang sicp

#|
  Упражнение 3.45

  Хьюго Дум полагает, что теперь, когда операции снятия денег со счета и занесения их на счет перестали
  сериализовываться автоматически, система банковских счетов стала неоправданно сложной и работать с ней
  правильным образом чересчур трудно. Он предлагает сделать так, чтобы make-account-and-serializer
  экспортировал сериализатор (для использования в процедурах вроде serialized-exchange), и вдобавок сам
  использовал его для сериализации простых операций со счетом, как это делал make-account. Он предлагает
  переопределить объект-счет так:

    (define (make-account-and-serializer balance)
      (define (withdraw amount)
        (if (>= balance amount)
            (begin
              (set! balance (- balance amount))
              balance)
            "Insufficient funds"))
      (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
      (let ((balance-serializer (make-serializer)))
        (define (dispatch m)
          (cond ((eq? m 'withdraw) (balance-serializer withdraw))
                ((eq? m 'deposit) (balance-serializer deposit))
                ((eq? m 'balance) balance)
                ((eq? m 'serializer) balance-serializer)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        dispatch))

  Затем снятия обрабатываются так же, как с оригинальным make-account:

    (define (deposit account amount)
      ((account 'deposit) amount))

  Объясните, в чем Хьюго ошибается. В частности, рассмотрите, что происходит при вызове serialized-exchange.
|#

#|
  Вспомним, как выглядят процедуры exchange и serialized-exchange:

    (define (exchange account1 account2)
      (let ((difference (- (account1 'balance)
                           (account2 'balance))))
        ((account1 'withdraw) difference)
        ((account2 'deposit) difference)))

    (define (serialized-exchange account1 account2)
      (let ((serializer1 (account1 'serializer))
            (serializer2 (account2 'serializer)))
        ((serializer1 (serializer2 exchange))
         account1
         account2)))

  Процедура exchange запущенная через serialized-exchange не сможет получить доступ к операциям снятия
  и занесения средств на счёт и не завершится, т.к. эти процедуры сериализованы теми же сериализаторами,
  что захватила (acquire) она сама.

  Версия Хьюго сводит на нет нашу попытку разрешить проблему обмена, т.к. отсекает у обёрнутых в
  сериализаторы процедур возможность использовать withdraw и deposit.
|#
