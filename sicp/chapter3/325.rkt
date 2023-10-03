#lang racket

#|
  Упражнение 3.25

  Обобщая случаи одно- и двумерных таблиц, покажите, как можно реализовать таблицу, в которой элементы
  хранятся с произвольным количеством ключей и различные значения могут храниться с различным количеством
  ключей. Процедуры lookup и insert! должны принимать на входе список ключей, с которыми требуется
  обратиться к таблице.
|#

(#%require rackunit
           compatibility/mlist)

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((equal? key (mcar (mcar (mcar records)))) (mcar records))
            (else (assoc key (mcdr records)))))

    (define (lookup keys table)
      (cond ((or (null? keys) (null? table)) #f)
            ((null? (mcdr keys))
             (let ((record (assoc (mcar keys) (mcdr table))))
               (if record
                   (mcdr (mcar record))
                   #f)))
            (else
             (let ((subtable (assoc (mcar keys) (mcdr table))))
               (if subtable
                   (lookup (mcdr keys) subtable)
                   #f)))))

    (define (insert! keys value table)
      (define (iter! keys table)
        (cond ((null? (mcdr keys))
               (let ((record (assoc (mcar keys) (mcdr table))))
                 (if record
                     (set-mcdr! (mcar record) value)
                     (set-mcdr! table
                                (mcons (mlist (mcons (mcar keys) value))
                                       (mcdr table))))))
              (else
               (let ((subtable (assoc (mcar keys) (mcdr table))))
                 (if subtable
                     (iter! (mcdr keys) subtable)
                     (let ((empty-subtable (mlist (mlist (mcar keys)))))
                       (set-mcdr! table (mcons empty-subtable
                                               (mcdr table)))
                       (iter! (mcdr keys) empty-subtable)))))))

      (if (null? keys) #f
          (iter! keys table))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc)  (lambda (keys)
                                     (lookup keys local-table)))
            ((eq? m 'insert-proc!) (lambda (keys value)
                                     (insert! keys value local-table)))
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

(define (lookup table keys)
  ((table 'lookup-proc) keys))

(define (insert! table keys value)
  ((table 'insert-proc!) keys value))

(define table1 (make-table))

(check-false (lookup table1 (mlist 1)))

(insert! table1 (mlist 1) 111)
(insert! table1 (mlist 1 2) 222)
(insert! table1 (mlist 1 3) 333)
(insert! table1 (mlist 1 3 4) 444)

(check-equal? (lookup table1 (mlist 1)) 111)
(check-equal? (lookup table1 (mlist 1 2)) 222)
(check-equal? (lookup table1 (mlist 1 3)) 333)
(check-equal? (lookup table1 (mlist 1 3 4)) 444)
(check-false (lookup table1 (mlist 2)))

(insert! table1 (mlist 1 3) 3333)
(insert! table1 (mlist 1 3 4) 4444)

(check-equal? (lookup table1 (mlist 1 3)) 3333)
(check-equal? (lookup table1 (mlist 1 3 4)) 4444)
