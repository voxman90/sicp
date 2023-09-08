#lang sicp

#|
  Упражнение 2.55

  Ева Лу Атор вводит при работе с интерпретатором выражение

    (car ''abracadabra)

  К ее удивлению, интерпретатор печатает quote. Объясните.
|#

(#%require rackunit)

(check-equal? (car ''abracadabra) 'quote)

#|
  Потому что 'exp есть синтаксический сахар для (quote exp). И после первого quote последующие
  воспринимаются как набор символов quote (а скобки воспринимаются как список).
|#

(check-equal? ''abracadabra (quote (quote abracadabra)))
(check-equal? ''abracadabra '(quote abracadabra))
