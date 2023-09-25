#lang sicp

#|
  Упражнение 3.9

  В разделе 1.2.1 мы с помощью подстановочной модели анализировали две процедуры вычисления факториала,
  рекурсивную

    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))

  и итеративную

    (define (factorial n)
      (fact-iter 1 1 n))

    (define (fact-iter product counter max-count)
      (if (> counter max-count)
          product
          (fact-iter (* counter product)
                     (+ counter 1)
                     max-count)))

  Продемонстрируйте, какие структуры окружений возникнут при вычислении (factorial 6) с каждой из версий
  процедуры factorial.
|#

#|
  Процедурные объекты в глобальном кадре окружения:

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━┓
     Глобальное ┃ ...                     ┃
    ───────────►┃ ...                     ┃
      окружение ┃ factorial               ┃
                ┗━━┿━━━━━━━━━━━━━━━━━━━━━━┛
                   ▼       ▲
                ┏━━━┳━━━┓  │
                ┃ • ┃ • ╂──┘
                ┗━┿━┻━━━┛
                  │
                  ▼
          параметры: n
          тело: (if (= n 1)
                    1
                    (* n (factorial (- n 1))))

  Окружения, созданные при вычислении (factorial 6):

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━┓
     Глобальное ┃ ...                     ┃
    ───────────►┃ ...                     ┃
      окружение ┃ ...                     ┃
                ┗━━━━━━━━━━━━━━━━━━━━━━━━━┛
                   ▲      ▲  ▲  ▲  ▲  ▲
              ┏━━━━┷━━━━┓ │  │  │  │  │
     E1 ─────►┃ n: 6    ┃ │  │  │  │  │ (if (= n 1) 1 (* n (factorial (- n 1))))
              ┗━━━━━━━━━┛ │  │  │  │  │
              ┏━━━━━━━━━┓ │  │  │  │  │
     E2 ─────►┃ n: 5    ╂─┘  │  │  │  │ (if (= n 1) 1 (* n (factorial (- n 1))))
              ┗━━━━━━━━━┛    │  │  │  │
              ┏━━━━━━━━━┓    │  │  │  │
     E3 ─────►┃ n: 4    ╂────┘  │  │  │ (if (= n 1) 1 (* n (factorial (- n 1))))
              ┗━━━━━━━━━┛       │  │  │
              ┏━━━━━━━━━┓       │  │  │
     E4 ─────►┃ n: 3    ╂───────┘  │  │ (if (= n 1) 1 (* n (factorial (- n 1))))
              ┗━━━━━━━━━┛          │  │
              ┏━━━━━━━━━┓          │  │
     E5 ─────►┃ n: 2    ╂──────────┘  │ (if (= n 1) 1 (* n (factorial (- n 1))))
              ┗━━━━━━━━━┛             │
              ┏━━━━━━━━━┓             │
     E6 ─────►┃ n: 1    ╂─────────────┘ (if (= n 1) 1 (* n (factorial (- n 1))))
              ┗━━━━━━━━━┛

  Процедурные объекты в глобальном кадре окружения:

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
     Глобальное ┃ ...                                ┃
    ───────────►┃ fact-iter ──────────────────┐      ┃
      окружение ┃ factorial                   │      ┃
                ┗━━┿━━━━━━━━━━━━━━━━━━━━━━━━━━┿━━━━━━┛
                   ▼       ▲                  ▼    ▲
                ┏━━━┳━━━┓  │            ┏━━━┳━━━┓  │
                ┃ • ┃ • ╂──┘            ┃ • ┃ • ╂──┘
                ┗━┿━┻━━━┛               ┗━┿━┻━━━┛
                  │                       │
                  ▼                       ▼
          параметры: n                  параметры: product counter max-count
          тело:                         тело:
          (fact-iter 1 1 n)             (if (> counter max-count)
                                            product
                                            (fact-iter (* counter product)
                                                       (+ counter 1)
                                                       max-count)))

  Окружения, созданные при вычислении (factorial 6):

                ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
     Глобальное ┃ ...                             ┃
    ───────────►┃ ...                             ┃
      окружение ┃ ...                             ┃
                ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                   ▲           ▲  ▲  ▲  ▲  ▲  ▲  ▲
              ┏━━━━┷━━━━┓      │  │  │  │  │  │  │
     E1 ─────►┃ n: 6    ┃      │  │  │  │  │  │  │ (fact-iter 1 1 n)
              ┗━━━━━━━━━┛      │  │  │  │  │  │  │
              ┏━━━━━━━━━━━━━━┓ │  │  │  │  │  │  │ (if (> counter max-count)
              ┃ product: 1   ┃ │  │  │  │  │  │  │     product
     E2 ─────►┃ counter: 1   ╂─┘  │  │  │  │  │  │     (fact-iter (* counter product)
              ┃ max-count: 6 ┃    │  │  │  │  │  │                (+ counter 1)
              ┗━━━━━━━━━━━━━━┛    │  │  │  │  │  │               max-count)))
              ┏━━━━━━━━━━━━━━┓    │  │  │  │  │  │ (if (> counter max-count)
              ┃ product: 1   ┃    │  │  │  │  │  │     product
     E3 ─────►┃ counter: 2   ╂────┘  │  │  │  │  │     (fact-iter (* counter product)
              ┃ max-count: 6 ┃       │  │  │  │  │                (+ counter 1)
              ┗━━━━━━━━━━━━━━┛       │  │  │  │  │                max-count)))
              ┏━━━━━━━━━━━━━━┓       │  │  │  │  │ (if (> counter max-count)
              ┃ product: 2   ┃       │  │  │  │  │     product
     E4 ─────►┃ counter: 3   ╂───────┘  │  │  │  │     (fact-iter (* counter product)
              ┃ max-count: 6 ┃          │  │  │  │                (+ counter 1)
              ┗━━━━━━━━━━━━━━┛          │  │  │  │                max-count)))
              ┏━━━━━━━━━━━━━━┓          │  │  │  │ (if (> counter max-count)
              ┃ product: 6   ┃          │  │  │  │     product
     E5 ─────►┃ counter: 4   ╂──────────┘  │  │  │     (fact-iter (* counter product)
              ┃ max-count: 6 ┃             │  │  │                (+ counter 1)
              ┗━━━━━━━━━━━━━━┛             │  │  │                max-count)))
              ┏━━━━━━━━━━━━━━┓             │  │  │ (if (> counter max-count)
              ┃ product: 24  ┃             │  │  │     product
     E6 ─────►┃ counter: 5   ╂─────────────┘  │  │     (fact-iter (* counter product)
              ┃ max-count: 6 ┃                │  │                (+ counter 1)
              ┗━━━━━━━━━━━━━━┛                │  │                max-count)))
              ┏━━━━━━━━━━━━━━┓                │  │ (if (> counter max-count)
              ┃ product: 120 ┃                │  │     product
     E7 ─────►┃ counter: 6   ╂────────────────┘  │     (fact-iter (* counter product)
              ┃ max-count: 6 ┃                   │                (+ counter 1)
              ┗━━━━━━━━━━━━━━┛                   │                max-count)))
              ┏━━━━━━━━━━━━━━┓                   │ (if (> counter max-count)
              ┃ product: 720 ┃                   │     product
     E8 ─────►┃ counter: 7   ╂───────────────────┘     (fact-iter (* counter product)
              ┃ max-count: 6 ┃                                    (+ counter 1)
              ┗━━━━━━━━━━━━━━┛                                    max-count)))
|#
