#lang sicp

#|
  Упражнение 3.10

  В процедуре make-withdraw локальная переменная balance создается в виде параметра make-withdraw. Можно
  было бы создать локальную переменную и явно, используя let, а именно:

    (define (make-withdraw initial-amount)
      (let ((balance initial-amount))
        (lambda (amount)
          (if (>= balance amount)
              (begin (set! balance (- balance amount))
                balance)
              "Insufficient funds"))))

  Напомним, что в разделе 1.3.2 говорится, что let всего лишь синтаксический сахар для вызова процедуры:

    (let ((<var> <exp>)) <body>)

  интерпретируется как альтернативный синтаксис для

    ((lambda (<var>) <body>) <exp>)

  С помощью модели с окружениями проанализируйте альтернативную версию make-withdraw. Нарисуйте картинки,
  подобные приведенным в этом разделе, для выражений

    (define W1 (make-withdraw 100))

    (W1 50)

    (define W2 (make-withdraw 100))

  Покажите, что две версии make-withdraw создают объекты с одинаковым поведением. Как различаются\
  структуры окружений в двух версиях?
|#

#|
  Процедурные объекты в глобальном кадре окружения для (define W1 (make-withdraw 100)):

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
     Глобальное ┃ make-withdraw ────────────────────┐ ┃
    ───────────►┃ ...                               │ ┃◄────┐
      окружение ┃ W1                                │ ┃     │
                ┗━━┿━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┿━┛     │
                   │            ▲                   ▼       │
                   │ ┏━━━━━━━━━━┷━━━━━━━━━━┓      ┏━━━┳━━━┓ │
                   │ ┃ initial-amount: 100 ┃◄─ E1 ┃ • ┃ • ╂─┘
                   │ ┗━━━━━━━━━━━━━━━━━━━━━┛      ┗━┿━┻━━━┛
                   │            ▲                   ▼
                   │ ┏━━━━━━━━━━┷━━━━━━━━━━┓       параметры: initial-amount
                   │ ┃ balance: 100        ┃◄─ E2  тело: (let ((balance initial-amount))
                   │ ┗━━━━━━━━━━━━━━━━━━━━━┛               (lambda (amount)
                   ▼       ▲                                 (if (>= balance amount)
                 ┏━━━┳━━━┓ │                                   (begin (set! balance (- balance amount))
                 ┃ • ┃ • ╂─┘                                     balance)
                 ┗━┿━┻━━━┛                                     "Insufficient funds"))))
                   ▼
          параметры: amount
          тело: (if (>= balance amount)
                    (begin (set! balance (- balance amount))
                      balance)
                    "Insufficient funds"))))

  Окружения, созданные при вычислении (W1 50):

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
     Глобальное ┃ make-withdraw ────────────────────┐ ┃
    ───────────►┃ ...                               │ ┃◄────┐
      окружение ┃ W1                                │ ┃     │
                ┗━━┿━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┿━┛     │
                   │            ▲                   ▼       │
                   │ ┏━━━━━━━━━━┷━━━━━━━━━━┓      ┏━━━┳━━━┓ │
                   │ ┃ initial-amount: 100 ┃◄─ E1 ┃ • ┃ • ╂─┘
                   │ ┗━━━━━━━━━━━━━━━━━━━━━┛      ┗━┿━┻━━━┛
                   │            ▲                   ▼
                   │ ┏━━━━━━━━━━┷━━━━━━━━━━┓       параметры: initial-amount
                   │ ┃ balance: 100        ┃◄─ E2  тело: (let ((balance initial-amount))
                   │ ┗━━━━━━━━━━━━━━━━━━━━━┛               (lambda (amount)
                   ▼       ▲        ▲                        (if (>= balance amount)
                 ┏━━━┳━━━┓ │   ┏━━━━┷━━━━━━━━━━━━━━━━┓             (begin (set! balance (- balance amount))
                 ┃ • ┃ • ╂─┘   ┃ amount: 50          ┃◄─ E3      balance)
                 ┗━┿━┻━━━┛     ┗━━━━━━━━━━━━━━━━━━━━━┛         "Insufficient funds"))))
                   ▼
          параметры: amount
          тело: (if (>= balance amount)
                    (begin (set! balance (- balance amount))
                      balance)
                    "Insufficient funds"))))

  Процедурные объекты в глобальном кадре окружения для (define W2 (make-withdraw 100)):

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓  ┌──────────┐
     Глобальное ┃ ...                                   ┃◄─┘ ┏━━━┳━━━┓│
    ───────────►┃ make-withdraw ────────────────────────╂───►┃ • ┃ • ╂┘
      окружение ┃ W2 ───────────────────────────────────╂───┐┗━┿━┻━━━┛
            ┌───╂ W1                                    ┃   │  │
            │   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛   │  ▼
            │           ▲                       ▲           │ параметры: initial-amount
            │┏━━━━━━━━━━┷━━━━━━━━━━┓ ┏━━━━━━━━━━┷━━━━━━━━━━┓│ тело: (let ((balance initial-amount))
        E1 ─►┃ initial-amount: 100 ┃ ┃ initial-amount: 100 ┃◄─ E3     (lambda (amount)
            │┗━━━━━━━━━━━━━━━━━━━━━┛ ┗━━━━━━━━━━━━━━━━━━━━━┛│           (if (>= balance amount)
            │           ▲                       ▲           │               (begin (set! balance (- balance amount))
            │┏━━━━━━━━━━┷━━━━━━━━━━┓ ┏━━━━━━━━━━┷━━━━━━━━━━┓│                 balance)
        E2 ─►┃ balance: 50         ┃ ┃ balance: 100        ┃◄─ E4           "Insufficient funds"))))
            │┗━━━━━━━━━━━━━━━━━━━━━┛ ┗━━━━━━━━━━━━━━━━━━━━━┛│
            ▼       ▲                               ▲       ▼
          ┏━━━┳━━━┓ │                               │   ┏━━━┳━━━┓
          ┃ • ┃ • ╂─┘                               │   ┃ • ┃ • ╂┐
          ┗━┿━┻━━━┛                                 │   ┗━┿━┻━━━┛│
            │ ┌───┐                                 └─────┼──────┘
            ▼ ▼   └───────────────────────────────────────┘
          параметры: amount
          тело: (if (>= balance amount)
                    (begin (set! balance (- balance amount))
                      balance)
                    "Insufficient funds"))))
|#
