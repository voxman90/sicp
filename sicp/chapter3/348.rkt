#lang racket

#|
  Упражнение 3.48

  Подробно объясните, почему метод избежания тупиков, описанный выше (т. е. счета нумеруются, и каждый
  процесс сначала пытается захватить счет с меньшим номером), в самом деле позволяет избежать тупика в
  задаче обмена балансов. Перепишите serialized-exchange с использованием этой идеи. (Придется также
  изменить make-account, так, чтобы каждый счет создавался вместе с номером, и чтобы этот номер можно
  было считать, послав соответствующее сообщение.)
|#

#|
  Пусть первый процесс обменивает баланс счёта A и B, а второй процесс обменивает баланс счёта B и A.
  Каким образом возникает тупик, если не используется метод избегания тупиков? После того, как первый
  процесс захватывает сериализатор счёта А, а второй процесс захватывает сериализатор счёта B, и первый
  и второй процесс не отпускают сериализатор счёта А и B соответственно, до тех пор, пока не захватят
  сериализатор счёта B и A соответственно. В случае перевода с одного счёта на другой проблемы не возникло
  бы, потому что сериализатор после снятия или занесения на счёт отпускался бы, но в случае обмена требуется
  получить доступ и к одному счёту, и к другому, и только после завершения всех манипуляций отпустить их.

  Если добавить метод избежания тупиков, где счета нумеруются и каждый процесс сначала пытается захватить
  счёт с меньшим номером, то последовательность захвата для всех процессов, которые пытаются получить
  доступ к тем же разделённым ресурсам будет одинаковой. Стало быть, если какой-либо процесс на начальном
  этапе осуществит захват разделяемого ресурса, то он сможет продвигаться дальше (без тупика, но, возможно,
  с паузами, т.к. ресурсы с большим нормером могут быть временно заняты другим процессом).
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

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (proc)
      (define (serialized-proc . args)
        (mutex 'acquire)
        (let ((val (apply proc args)))
          (mutex 'release)
          val))

      serialized-proc)))

(define (make-account balance account-id)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      (error "Unsufficient funds -- MAKE-ACCOUNT")))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((id account-id)
        (balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'number) id)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unsufficient funds -- MAKE-ACCOUNT"
                  m))))

    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((if (> (account1 'number) (account2 'number))
         (serializer2 (serializer1 exchange))
         (serializer1 (serializer2 exchange)))
     account1
     account2)))

(define peter-acc (make-account 100 1))

(check-equal? (peter-acc 'number) 1)
(check-equal? (peter-acc 'balance) 100)
(check-equal? ((peter-acc 'withdraw) 20) 80)
(check-equal? ((peter-acc 'deposit) 70) 150)

(define paul-acc (make-account 50 2))

(serialized-exchange peter-acc paul-acc)
(check-equal? (peter-acc 'balance) 50)
(check-equal? (paul-acc 'balance) 150)
