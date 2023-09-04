#lang racket

#|
  Упражнение 2.23

  Процедура for-each похожа на map. В качестве аргументов она принимает процедуру и список элементов.
  Однако вместо того, чтобы формировать список результатов, for-each просто применяет процедуру по
  очереди ко всем элементам слева направо. Результаты применения процедуры к аргументам не используются
  вообще — for-each применяют к процедурам, которые осуществляют какое-либо действие вроде печати.
  Например,

    (for-each (lambda (x) (newline) (display x))
              (list 57 321 88))
    57
    321
    88

  Значение, возвращаемое вызовом for-each (оно в листинге не показано) может быть каким угодно,
  например истина. Напишите реализацию my-for-each.
|#

(#%require rackunit)

(define (my-for-each callback l)
  (when (not (null? l))
      (begin
        (callback (car l))
        (my-for-each callback (cdr l)))))

(define (example-print example-list out)
  (my-for-each (lambda (x) (newline) (display x out)) example-list))

(define op1 (open-output-string))
(example-print (list 1 2 3) op1)
(define display-result (get-output-string op1))
(check-equal? display-result "123")
(close-output-port op1)

(define op2 (open-output-string))
(example-print (list 57 321 88) op2)
(define display-result2 (get-output-string op2))
(check-equal? display-result2 "5732188")
(close-output-port op2)
