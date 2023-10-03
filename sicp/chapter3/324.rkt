#lang racket

#|
  Упражнение 3.24

  В реализациях таблиц в этом разделе ключи всегда проверяются на равенство с помощью equal? (который,
  в свою очередь, зовется из assoc). Это не всегда то, что нужно. Например, можно представить себе таблицу
  с числовыми ключами, где не требуется точного совпадения с числом, которое мы ищем, а нужно только
  совпадение с определенной допустимой ошибкой. Постройте конструктор таблиц make-table, который в
  качестве аргумента принимает процедуру same-key? для проверки равенства ключей. Make-table должна
  возвращать процедуру dispatch, через которую можно добраться до процедур lookup и insert! локальной
  таблицы.
|#

(#%require rackunit
           compatibility/mlist)

(define (make-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                     (mcons key-2 value))
                              (mcdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

(define table-eq (make-table =))
(define get-eq (table-eq 'lookup-proc))
(define put-eq (table-eq 'insert-proc!))

(put-eq 1 2 333)
(check-equal? (get-eq 1 2) 333)
(check-false (get-eq 1 2.0000001))

(define (same-key-epsilon? epsilon)
  (lambda (x y)
    (if (< (abs (- x y)) epsilon) #t
        #f)))

(define table-epsilon (make-table (same-key-epsilon? 0.1)))
(define get-epsilon (table-epsilon 'lookup-proc))
(define put-epsilon (table-epsilon 'insert-proc!))

(put-epsilon 1 2 42)
(check-equal? (get-epsilon 1 2) 42)
(check-equal? (get-epsilon 1 2.001) 42)
(check-equal? (get-epsilon 1.01 2.01001) 42)
(check-false (get-epsilon 1.2 2.1))
