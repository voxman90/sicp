#lang sicp

#|
  Упражнение 2.42

  В «задаче о восьми ферзях» спрашивается, как расставить на шахматной доске восемь ферзей так, чтобы
  ни один из них не бил другого (то есть никакие два ферзя не должны находиться на одной вертикали,
  горизонтали или диагонали). Одно из возможных решений показано на рисунке 2.8. Один из способов решать
  эту задачу состоит в том, чтобы идти поперек доски, устанавливая по ферзю в каждой вертикали. После
  того, как k − 1 ферзя мы уже разместили, нужно разместить k-го в таком месте, где он не бьет ни одного
  из тех, которые уже находятся на доске. Этот подход можно сформулировать рекурсивно: предположим, что
  мы уже породили последовательность из всех возможных способов разместить k − 1 ферзей на первых k − 1
  вертикалях доски. Для каждого из этих способов мы порождаем расширенный набор позиций, добавляя ферзя
  на каждую горизонталь k-й вертикали. Затем эти позиции нужно отфильтровать, оставляя только те, где
  ферзь на k-й вертикали не бьется ни одним из остальных. Продолжая этот процесс, мы породим не просто
  одно решение, а все решения этой задачи.

  Это решение мы реализуем в процедуре queens, которая возвращает последовательность решений задачи
  размещения n ферзей на доске n × n. В процедуре queens есть внутренняя процедура queen-cols, которая
  возвращает последовательность всех способов разместить ферзей на первых k вертикалях доски.

    (define (queens board-size)
      (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
              (lambda (positions) (safe? k positions))
                (flatmap
                  (lambda (rest-of-queens)
                    (map (lambda (new-row)
                           (adjoin-position new-row k rest-of-queens))
                         (enumerate-interval 1 board-size)))
                  (queen-cols (- k 1))))))
              (queen-cols board-size))

  В этой процедуре rest-of-queens есть способ размещения k − 1 ферзя на первых k − 1 вертикалях, а
  new-row — это горизонталь, на которую предлагается поместить ферзя с k-й вертикали. Завершите эту
  программу,  реализовав  представление  множеств  позиций  ферзей  на  доске,  включая  процедуру
  adjoin-position, которая добавляет нового ферзя на определенных горизонтали и вертикали к заданному
  множеству позиций, и empty-board, которая представляет пустое множество позиций. Еще нужно написать
  процедуру safe?, которая для множества позиций определяет, находится ли ферзь с k-й вертикали в
  безопасности  от  остальных. (Заметим, что  нам  требуется  проверять  только  то, находится  ли  в
  безопасности новый ферзь — для остальных ферзей безопасность друг от друга уже гарантирована.)
|#

(#%require rackunit)

(define (accumulate prop initial sequence)
  (define (iter acc sub-sequence)
    (if (null? sub-sequence)
        acc
        (iter (prop acc (car sub-sequence)) (cdr sub-sequence))))

  (iter initial sequence))

(define (filter predicate? sequence)
  (define (rec sub-sequence)
    (cond ((null? sub-sequence) '())
          ((predicate? (car sub-sequence))
           (cons (car sub-sequence) (rec (cdr sub-sequence))))
          (else (rec (cdr sub-sequence)))))

  (rec sequence))

(define (flatmap callback sequence)
  (accumulate append '() (map callback sequence)))

(define (enumerate-interval start end)
  (if (< end start)
     '()
      (cons start (enumerate-interval (+ start 1) end))))

(define empty-board '())

(define (row figure-position)
  (car figure-position))

(define (column figure-position)
  (cdr figure-position))

(define (on-same-row? position-1 position-2)
  (= (row position-1) (row position-2)))

(define (on-same-diagonal? position-1 position-2)
  (= (abs (- (row position-1) (row position-2)))
     (abs (- (column position-1) (column position-2)))))

(define (map-queen-positions last-column positions)
  (map
    (lambda (row column) (cons row column))
    positions
    (map abs (enumerate-interval (- last-column) -1))))

(define (safe? last-column queens-positions)
  (let ((last-queen-position (cons (car queens-positions) last-column)))
    (= 1
       (length
         (filter
           (lambda (queen-position)
             (or (on-same-row? queen-position last-queen-position)
                 (on-same-diagonal? queen-position last-queen-position)))
           (map-queen-positions last-column queens-positions))))))

(define (adjoin-position new-row _ rest-of-queens)
  (cons new-row rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))

  (queen-cols board-size))

(check-equal? (queens 1) '((1)))
(check-equal? (queens 2) '())
(check-equal? (queens 3) '())

(define queens-4x4 (queens 4))

(check-true (or (equal? queens-4x4 '((3 1 4 2) (2 4 1 3)))
                (equal? queens-4x4 '((2 4 1 3) (3 1 4 2)))))
