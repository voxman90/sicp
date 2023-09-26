#lang sicp

#|
  Упражнение 3.11

  В разделе 3.2.3 мы видели, как модель с окружениями описывает поведение процедур, обладающих внутренним
  состоянием. Теперь мы рассмотрели, как работают локальные определения. Типичная процедура с передачей
  сообщений пользуется и тем, и другим. Рассмотрим процедуру моделирования банковского счета из раздела
  3.1.1:

    (define (make-account balance)
      (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
              balance)
            "Insufficient funds"))
      (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))

      dispatch)

  Покажите, какая структура окружений создается последовательностью действий

    (define acc (make-account 50))

    ((acc 'deposit) 40)
    90

    ((acc 'withdraw) 60)
    30

  Где хранится внутреннее состояние acc? Предположим, что мы определяем еще один счет

    (define acc2 (make-account 100))

  Каким образом удается не смешивать внутренние состояния двух счетов? Какие части структуры окружений
  общие у acc и acc2?
|#

#|
  Процедурные объекты в глобальном кадре окружения для (define acc (make-account 50)):

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━┓
     Глобальное ┃ ...                     ┃
    ───────────►┃ acc ─────────┐          ┃
      окружение┌╂ make-account │          ┃
               │┗━━━━━━━━━━━━━━┿━━━━━━━━━━┛
               ▼     ▲         │    ▲ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
           ┏━━━┳━━━┓ │         │    │ ┃ balance: 50                   ┃◄─ E1
           ┃ • ┃ • ╂─┘         │    │ ┃ withdraw ──────────────────┐  ┃
           ┗━┿━┻━━━┛           │    └─┨ deposit  ─────────┐        │  ┃
             ▼                 │      ┃ dispatch ┐        │        │  ┃
    параметры: balance         │      ┗━━━━━━━━━━┿━━━━━━━━┿━━━━━━━━┿━━┛
    тело:                      │       ▲         │ ▲      │ ▲      │ ▲
    ((define withdraw ...)     ▼       │         ▼ │      ▼ │      ▼ │
     (define deposit ...)    ┏━━━┳━━━┓ │     ┏━━━┳━┿━┓┏━━━┳━┿━┓┏━━━┳━┿━┓
     (define dispatch ...)   ┃ • ┃ • ╂─┘     ┃ • ┃ • ┃┃ • ┃ • ┃┃ • ┃ • ┃
     dispatch)               ┗━┿━┻━━━┛       ┗━┿━┻━━━┛┗━┿━┻━━━┛┗━┿━┻━━━┛
                               │ ┌─────────────┘        │        │
                               ▼ ▼                      ▼        ▼
                             параметры: m     параметры: amount  параметры: amount
                             тело:            тело: ...          тело: ...
                             (cond
                               ((eq? m 'withdraw) withdraw)
                               ((eq? m 'deposit) deposit)
                               (else (error "Unknown request -- MAKE-ACCOUNT"
                                            m)))

  Окружения, созданные при вычислении ((acc 'deposit) 40):

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━┓
     Глобальное ┃ ...                     ┃
    ───────────►┃ acc ─────────┐          ┃
      окружение┌╂ make-account │          ┃
               │┗━━━━━━━━━━━━━━┿━━━━━━━━━━┛
               ▼     ▲         │    ▲ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
           ┏━━━┳━━━┓ │         │    │ ┃ balance: 50                   ┃◄─ E1
           ┃ • ┃ • ╂─┘         │    │ ┃ withdraw ──────────────────┐  ┃
           ┗━┿━┻━━━┛           │    └─┨ deposit  ─────────┐        │  ┃
             ▼             ┌───┼─────►┃ dispatch ┐        │        │  ┃
    параметры: balance     │   │      ┗━━━━━━━━━━┿━━━━━━━━┿━━━━━━━━┿━━┛
    тело:                  │   │       ▲   ▲     │ ▲      │ ▲      │ ▲
    ((define withdraw ...) │   ▼       │   │     ▼ │      ▼ │      ▼ │
     (define deposit ...)  │ ┏━━━┳━━━┓ │   │ ┏━━━┳━┿━┓┏━━━┳━┿━┓┏━━━┳━┿━┓
     (define dispatch ...) │ ┃ • ┃ • ╂─┘   │ ┃ • ┃ • ┃┃ • ┃ • ┃┃ • ┃ • ┃
     dispatch)             │ ┗━┿━┻━━━┛     │ ┗━┿━┻━━━┛┗━┿━┻━━━┛┗━┿━┻━━━┛
                ┌──────────┘   │ ┌─────────┼───┘        │        │
    ┏━━━━━━━━━━━┷━┓            ▼ ▼         │            ▼        ▼
    ┃ m: 'deposit ┃◄─ E2      параметры: m │   параметры: amount  параметры: amount
    ┗━━━━━━━━━━━━━┛           тело: ...    │   тело: ...          тело: ...
                                           │
                                    ┏━━━━━━┷━━━━━━┓
                                    ┃ amount: 40  ┃◄─ E3
                                    ┗━━━━━━━━━━━━━┛

  Окружения, созданные при вычислении ((acc 'withdraw) 60):

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━┓
     Глобальное ┃ ...                     ┃
    ───────────►┃ acc ─────────┐          ┃
      окружение┌╂ make-account │          ┃
               │┗━━━━━━━━━━━━━━┿━━━━━━━━━━┛
               ▼     ▲         │    ▲ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
           ┏━━━┳━━━┓ │         │    │ ┃ balance: 90                   ┃◄─ E1
           ┃ • ┃ • ╂─┘         │    │ ┃ withdraw ──────────────────┐  ┃
           ┗━┿━┻━━━┛           │    └─┨ deposit  ─────────┐        │  ┃
             ▼             ┌───┼─────►┃ dispatch ┐        │        │  ┃
    параметры: balance     │   │      ┗━━━━━━━━━━┿━━━━━━━━┿━━━━━━━━┿━━┛
    тело:                  │   │       ▲   ▲     │ ▲      │ ▲      │ ▲
    ((define withdraw ...) │   ▼       │   │     ▼ │      ▼ │      ▼ │
     (define deposit ...)  │ ┏━━━┳━━━┓ │   │ ┏━━━┳━┿━┓┏━━━┳━┿━┓┏━━━┳━┿━┓
     (define dispatch ...) │ ┃ • ┃ • ╂─┘   │ ┃ • ┃ • ┃┃ • ┃ • ┃┃ • ┃ • ┃
     dispatch)             │ ┗━┿━┻━━━┛     │ ┗━┿━┻━━━┛┗━┿━┻━━━┛┗━┿━┻━━━┛
                ┌──────────┘   │ ┌─────────┼───┘        │        │
    ┏━━━━━━━━━━━┷━━┓           ▼ ▼         │            ▼        ▼
    ┃ m: 'withdraw ┃◄─ E2     параметры: m │   параметры: amount  параметры: amount
    ┗━━━━━━━━━━━━━━┛          тело: ...    │   тело: ...          тело: ...
                                           │
                                    ┏━━━━━━┷━━━━━━┓
                                    ┃ amount: 60  ┃◄─ E3
                                    ┗━━━━━━━━━━━━━┛

  Внутреннее состояние acc хранится в окружении E1. Если создать второй счёт, то внутренние состояния
  счёта acc и acc2 не будут смешиваться, т.к. balance каждого из них хранится в разных окружениях.
  Общим будет глобальное окружение, процедуры глобального окружения и тела процедур из соответствующих
  окружений (но не balance и не кадры, которые будут создаваться при вызове той или иной процедуры):

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓  ┌───────────┐
                ┃ ...                                          ┃◄─┘ ┏━━━┳━━━┓ │
     Глобальное ┃ make-account ────────────────────────────────╂───►┃ • ┃ • ╂─┘
    ───────────►┃ acc ────────────────┐                        ┃    ┗━┿━┻━━━┛
      окружение ┃ acc2 ─────────────┐ │                        ┃      ▼
                ┗━━━━━━━━━━━━━━━━━━━┿━┿━━━━━━━━━━━━━━━━━━━━━━━━┛     параметры: balance
                      ▲             │ │                ▲             тело: ((define withdraw ...)
         ┏━━━━━━━━━━━━┷━━━━━━━━━━┓  │ │  ┏━━━━━━━━━━━━━┷━━━━━━━━━━┓         (define deposit ...)
    E2 ─►┃ balance: 100          ┃  │ │  ┃ balance: 30            ┃◄─ E1    (define dispatch ...)
         ┃ withdraw ──────────┐  ┃  │ │  ┃ withdraw ───────────┐  ┃         dispatch)
         ┃ deposit  ┐         │  ┃  │ │  ┃ deposit  ───┐       │  ┃
         ┃ dispatch │         │  ┃◄┐│ │┌►┃ dispatch    │       │  ┃
         ┗━━┿━━━━━━━┿━━━━━━━━━┿━━┛ ││ ││ ┗━━┿━━━━━━━━━━┿━━━━━━━┿━━┛
            │  ▲    │    ▲    │ ▲  ││ ││    │     ▲    │    ▲  │ ▲
            ▼  │    ▼    │    ▼ │  ││ ││    ▼     │    ▼    │  ▼ └─┐
      ┏━━━┳━━━┓│┏━━━┳━━━┓│┏━━━┳━┿━┓││ ││ ┏━━━┳━━━┓│┏━━━┳━━━┓│┏━━━┳━┿━┓
      ┃ • ┃ • ╂┘┃ • ┃ • ╂┘┃ • ┃ • ┃││ ││ ┃ • ┃ • ╂┘┃ • ┃ • ╂┘┃ • ┃ • ┃
      ┗━┿━┻━━━┛ ┗━┿━┻━━━┛ ┗━┿━┻━━━┛││ ││ ┗━┿━┻━━━┛ ┗━┿━┻━━━┛ ┗━┿━┻━━━┛
        │         │         │      ││ ││   │         │         │
        │         │     ┌───┘      ││ ││   └──────┐  │  ┌──────┘
        │         │     │       ┌──┘│ │└──────┐   │  │  │
        │         │     │ ┏━━━┳━┿━┓ │ │ ┏━━━┳━┿━┓ │  │  │
        │         │     │ ┃ • ┃ • ┃◄┘ └►┃ • ┃ • ┃ │  │  │
        │         │     │ ┗━┿━┻━━━┛     ┗━┿━┻━━━┛ │  │  │
        │         │     │   │ ┌───────────┘       │  │  │
        │         │     │   ▼ ▼                   │  │  │
        └─────────│─────│─► параметры: m ◄────────┘  │  │
                  │     │   тело: ...                │  │
                  ▼     │                            │  │
                 параметры: amount ◄─────────────────┘  │
                 тело: ...                              │
                        ▼                               │
                       параметры: amount ◄──────────────┘
                       тело: ...
|# 