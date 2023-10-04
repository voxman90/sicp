#lang racket

#|
  Упражнение 3.36

  Допустим, что мы выполняем следующую последовательность действий в глобальном окружении:

    (define a (make-connector))

    (define b (make-connector))

    (set-value! a 10 'user)

  В какой-то момент при вычислении set-value! будет выполнено следующее выражение из внутренней процедуры
  соединителя:

    (for-each-except setter inform-about-value constraints)

  Нарисуйте диаграмму, изображающую окружение, в котором выполняется указанное выражение.
|#

#|
                ┌► параметры:                                ┌► параметры:
                │  тело:                                     │  exception procedure list
                │  (let ((value #f)                          │  тело:
                │        (informant #f)                      │  (define (loop items)
                │        (constraints '()))                  │    (cond ((null? items) 'done)
                │  (define (set-my-value newval setter) ...) │          ((eq? (car items) exception)
                │  (define (forget-my-value retractor) ...)  │           (loop (cdr items)))
                │  (define (connect new-constraint) ...)     │          (else (procedure (car items))
                │  (define (me request) ...)                 │                (loop (cdr items)))))
                │  me                                        │  (loop list)
                └─────────────────┐                  ┌───────┘           ┌────────────► параметры:
                                ┏━┿━┳━━━┓          ┏━┿━┳━━━┓           ┏━┿━┳━━━┓◄───┐   constraint
                                ┃ • ┃ • ╂─┐        ┃ • ┃ • ╂─┐         ┃ • ┃ • ╂┐   │   тело:
                                ┗━━━┻━━━┛ │        ┗━━━┻━━━┛ │         ┗━━━┻━━━┛│   │   (constraint 'I-have-a-value)
                                    ▲     ▼            ▲     ▼             ▲    ▼   │
                ┏━━━━━━━━━━━━━━━━━━━┿━━━━━━━━━━━━━━━━━━┿━━━━━━━━━━━━━━━━━━━┿━━━━━┓  │
     Глобальное ┃ make-connector ───┘  for-each-except ┘inform-about-value ┘     ┃  │
    ───────────►┃ ...                                                            ┃  │
      окружение ┃   a                                                        b ──╂─┐│
              ┌►┗━━━┿━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ ││
              │     │                                                        ▲     ││
              │   ┌─│─► параметры: request                                   │     ││
              │   │ │   тело:                                                │     ││
              │   │ │   (cond ((eq? request 'has-value?)                     │     ││
              │   │ │          (if informant #t #f))                         │     ││
              │   │ │         ((eq? request 'value) value)                   │     ││
              │   │ │         ((eq? request 'set-value!) set-my-value)       │     ││
              │   │ │         ((eq? request 'forget) forget-my-value)        │     ││
              │   │ │         ((eq? request 'connect) connect)               │     ││
              │   │ │         (else (error "Unknown operation -- CONNECTOR"  │     ││
              ├◄──│─│────────┐                                            ┌──┤┌────│┘
              │   ├─│────────│────────────────────────────────────────────│──││──┐ │
              │   │ │      ┏━┷━━━━━━━━━━━━━━━━━━┓      ┏━━━━━━━━━━━━━━━━━━┷━┓││  │ │
              │   │ │      ┃ value: #f          ┃      ┃ value: #f          ┃││  │ │
              │   │ │ E1 ─►┃ informant: #f      ┃ E3 ─►┃ informant: #f      ┃││  │ │
              │   │ │      ┃ constrains: '()    ┃      ┃ constrains: '()    ┃││  │ │
              │   │ │      ┗━━━━━━━━━━━━━━━━━━━━┛      ┗━━━━━━━━━━━━━━━━━━━━┛││  │ │
              │   │ │                 ▲                          ▲           ││  │ │
              │   │ ▼ E2 ─►┏━━━━━━━━━━┷━━━━━━━━━━━┓  ┏━━━━━━━━━━━┷━━━━━━━━━━┓◄─ E4 ▼
              │ ┏━┿━┳━━━┓  ┃ set-my-value: ...    ┃  ┃ set-my-value: ...    ┃││┏━┿━┳━━━┓
              │ ┃ • ┃ • ╂─►┃ forget-my-value: ... ┃  ┃ forget-my-value: ... ┃││┃ • ┃ • ╂┐
              │ ┗━━━┻━━━┛  ┃ connect: ...         ┃  ┃ connect: ...         ┃││┗━━━┻━━━┛│
              │     ▲      ┃ me                   ┃  ┃ me                   ┃◄│────▲────┘
              │     │      ┗━┿━━━━━━━━━━━━━━━━━━━━┛  ┗━┿━━━━━━━━━━━━━━━━━━━━┛││    │
              │     ├────────┘     ▲         ▲         └─────────────────────││────┘
        ┏━━━━━┷━━━━━┿━━━━━━━━━━━┓  │   ┏━━━━━┷━━━━━━━━━━━━━━━━━┓             ││
        ┃ connector ┘           ┃  │   ┃ request: 'set-value!  ┃             ││
   E5 ─►┃ new-value: 10         ┃ E6 ─►┃                       ┃             ││
        ┃ informant: 'user      ┃  │   ┃                       ┃             ││
        ┗━━━━━━━━━━━━━━━━━━━━━━━┛  │   ┗━━━━━━━━━━━━━━━━━━━━━━━┛             ││
         (set-value! a 10 'user)   │    (connector 'set-value!)              ││
                               ┌───┴────┐                                    ││
        ┏━━━━━━━━━━━━━━━━━━━━━━┷┓      ┏┷━━━━━━━━━━━━━━━━━━━━━━┓             ││
        ┃ newval: 10            ┃      ┃ request: 'has-value?  ┃             ││
   E7 ─►┃ setter: 'user         ┃ E8 ─►┃                       ┃             ││
        ┃                       ┃      ┃                       ┃             ││
        ┗━━━━━━━━━━━━━━━━━━━━━━━┛      ┗━━━━━━━━━━━━━━━━━━━━━━━┛             ││
         (set-my-value 10 'user)        (connector 'has-value?)              ││
                               ┌─────────────────────────────────────────────┘│
        ┏━━━━━━━━━━━━━━━━━━━━━━┷┓                                             │
        ┃ exception: 'user      ┃                                             │
   E9 ─►┃ procedure ────────────╂─────────────────────────────────────────────┘
        ┃ list: '()             ┃◄─────────┐
        ┃ loop ┐                ┃◄┐        │
        ┗━━━━━━┿━━━━━━━━━━━━━━━━┛ │        │
         (for-each-except 'user inform-about-value '())
               │                  │        │
               ▼                  │       ┏┷━━━━━━━━━━━━━━━━━━┓
           ┏━━━┳━━━┓              │ E10 ─►┃ items: 'user      ┃
           ┃ • ┃ • ╂──────────────┘       ┗━━━━━━━━━━━━━━━━━━━┛
           ┗━┿━┻━━━┛                           (loop '())
             │
             └► параметры: items
                тело: (cond ((null? items) 'done)
                            ((eq? (car items) exception)
                             (loop (cdr items)))
                            (else (procedure (car items))
                                  (loop (cdr items))))
|#
