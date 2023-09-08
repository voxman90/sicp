#lang sicp

#|
  Упражнение 2.60

  Мы указали, что множество представляется как список без повторяющихся элементов. Допустим теперь,
  что мы разрешаем повторяющиеся элементы. Например, множество {1, 2, 3} могло бы быть представлено как
  список (2 3 2 1 3 2 2). Разработайте процедуры element-of-set?, adjoin-set, union-set и intersection-set,
  которые бы работали с таким представлением. Как соотносится эффективность этих операций с эффективностью
  соответствующих процедур для представления без повторений? Существуют ли приложения, в которых Вы бы
  использовали скорее это представление, чем представление без повторений?
|#

(#%require rackunit)

#|
  Было и осталось O(n).
|#

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

#|
  Было O(n), стало O(1).
|#

(define (adjoin-set x set)
  (cons x set))

#|
  Было и осталось O(n²).
|#

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

#|
  Стало O(n), было O(n²).
|#

(define (union-set set1 set2)
  (define (iter acc set)
    (if (null? set)
        acc
        (iter (cons (car set) acc) (cdr set))))

  (iter set1 set2))

#|
  Я бы предпочёл использовать объекты со специальными сеттерами для подсчёта количества повторений или
  массивы с индексами для работы с последовательностями значений, которые могут повторяться.
|#

(define first '(1 2 3 5 3))
(define second '(3 4 5 6 5))

(check-equal? (union-set first second) '(5 6 5 4 3 1 2 3 5 3))
(check-equal? (intersection-set first second) '(3 5 3))
(check-equal? (intersection-set second first) '(3 5 5))
