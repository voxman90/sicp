#lang racket

#|
  Упражнение 3.7

  Рассмотрим объекты-банковские счета, создаваемые процедурой make-account, и снабженные паролями, как
  это описано в упражнении 3.3. Предположим, что наша банковская система требует от нас умения порождать
  совместные счета. Напишите процедуру make-joint, которая это делает. Make-joint должна принимать три
  аргумента. Первый из них — защищенный паролем счет. Второй обязан совпадать с паролем, с которым этот
  счет был создан, иначе make-joint откажется работать. Третий аргумент — новый пароль. Make-joint открывает
  дополнительный доступ к счету, с использованием нового пароля. Например, если банковский счет peter-acc
  был создан с паролем open-sesame, то

    (define paul-acc
      (make-joint peter-acc 'open-sesame 'rosebud))

  позволит нам проводить операции с peter-acc, используя имя paul-acc и пароль rosebud. Вам может
  потребоваться переработать решение упражнения 3.3, чтобы добавить эту новую возможность.
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
          (error "Wrong password")))

  account))

(define (make-joint account account-password user-password)
  (let ((password user-password))
    (lambda (entered-password op)
      (if (eq? password entered-password)
          (account account-password op)
          (error "Wrong password")))))

(define peter-acc (make-account 100 'qwerty))
(define john-acc (make-joint peter-acc 'qwertY '1234567))

(check-equal? (peter-acc 'qwerty 'balance) 100)
(check-exn exn:fail? (thunk (john-acc '1234567 'balance)) "Wrong password")

(define paul-acc (make-joint peter-acc 'qwerty 'asdfgh))

(check-equal? ((paul-acc 'asdfgh 'deposit) 14) 114)
(check-equal? (peter-acc 'qwerty 'balance) 114)
(check-equal? ((peter-acc 'qwerty 'withdraw) 114) 0)
(check-equal? (paul-acc 'asdfgh 'balance) 0)
