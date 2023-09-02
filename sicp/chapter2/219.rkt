#lang sicp

#|
  Рассмотрим программу подсчета способов размена из раздела 1.2.2. Было бы приятно иметь возможность
  легко изменять валюту, которую эта программа использует, так, чтобы можно было, например, вычислить,
  сколькими способами можно разменять британский фунт. Эта программа написана так, что знание о валюте
  распределено между процедурами first-denomination и count-change (которая знает, что существует пять
  видов американских монет). Приятнее было бы иметь возможность просто задавать список монет, которые
  можно использовать при размене.

  Мы хотим переписать процедуру cc так, чтобы ее вторым аргументом был список монет, а не целое число,
  которое указывает, какие монеты использовать. Тогда у нас могли бы быть списки, определяющие типы
  валют:

    (define us-coins (list 50 25 10 5 1))

    (define uk-coins (list 100 50 20 10 5 2 1 0.5))

  Можно было бы вызывать cc следующим образом:

    (cc 100 us-coins)
    292

  Это потребует некоторых изменений в программе cc. Ее форма останется прежней, но со вторым аргументом
  она будет работать иначе, вот так:

    (define (cc amount coin-values)
      (cond ((= amount 0) 1)
            ((or (< amount 0) (no-more? coin-values)) 0)
            (else
              (+ (cc amount
                 (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

  Определите процедуры first-denomination, except-first-denomination и no-more? в терминах элементарных
  операций над списковыми структурами. Влияет ли порядок списка coin-values на результат, получаемый
  cc? Почему?
|#

(#%require rackunit)

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(check-equal? (cc 100 us-coins) 292)
(check-equal? (cc 100 uk-coins) 104561)
