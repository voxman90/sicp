#lang racket

#|
  Упражнение 3.26

  При поиске в таблице, как она реализована выше, приходится просматривать список записей. В сущности,
  это представление с неупорядоченным списком из раздела 2.3.3. Для больших таблиц может оказаться
  эффективнее организовать таблицу иначе. Опишите реализацию таблицы, в которой записи (ключ, значение)
  организованы в виде бинарного дерева, в предположении, что ключи можно каким-то образом упорядочить
  (например, численно или по алфавиту). (Ср. с упражнением 2.66 из главы 2.)
|#

(#%require rackunit
           compatibility/mlist)

(define (order-proc value1 value2)
  (cond ((= value1 value2) 0)
        ((> value1 value2) 1)
        (else -1)))

(define (make-table order-proc)
  (define (make-entry key value)
    (mlist (mlist (mcons key value)) '() '()))

  (define (entry-key entry) (mcar (mcar (mcar entry))))

  (define (entry-value entry) (mcdr (mcar (mcar entry))))

  (define (entry-subtree entry) (mcdr (mcar entry)))

  (define (left-branch entry) (mcar (mcdr entry)))

  (define (right-branch entry) (mcar (mcdr (mcdr entry))))

  (define (set-entry-value! entry value) (set-mcdr! (mcar (mcar entry)) value))

  (define (set-entry-subtree! entry subtree) (set-mcdr! (mcar entry) subtree))

  (define (set-left-branch! entry value) (set-mcar! (mcdr entry) value))

  (define (set-right-branch! entry value) (set-mcar! (mcdr (mcdr entry)) value))

  (let ((local-table (make-entry '*table* '())))
    (define (assoc key entry)
      (define (rec checked-entry parent-entry)
        (if (null? checked-entry)
            (mcons #f parent-entry)
            (let ((checked-entry-key (entry-key checked-entry)))
              (let ((order (order-proc key checked-entry-key)))
                (cond ((= order 0)
                       (mcons #t checked-entry))
                      ((= order 1)
                       (rec (right-branch checked-entry) checked-entry))
                      (else
                       (rec (left-branch checked-entry) checked-entry)))))))

      (rec entry entry))

    (define (lookup keys entry)
      (define (rec keys checked-entry)
        (let ((key (mcar keys)))
          (let ((search-result (assoc key (entry-subtree checked-entry))))
            (let ((search-successful? (mcar search-result))
                  (search-breakpoint (mcdr search-result)))
              (cond ((null? (mcdr keys))
                     (if search-successful?
                         (entry-value search-breakpoint)
                         #f))
                    (else
                     (if search-successful?
                         (rec (mcdr keys) search-breakpoint)
                         #f)))))))

      (if (or (null? entry) (null? keys))
          #f
          (rec keys entry)))

    (define (adjust-entry! entry recipient recipient-parent)
      (if (null? recipient)
          (set-entry-subtree! recipient-parent entry)
          (let ((key (entry-key entry))
                (recipient-key (entry-key recipient)))
            (let ((order (order-proc key recipient-key)))
              (cond ((= order 1)
                     (set-right-branch! recipient entry))
                    (else
                     (set-left-branch! recipient entry)))))))

    (define (insert! keys value table)
      (define (iter! keys entry)
        (let ((key (mcar keys)))
          (let ((search-result (assoc key (entry-subtree entry))))
            (let ((search-successful? (mcar search-result))
                  (search-breakpoint (mcdr search-result)))
              (cond ((null? (mcdr keys))
                     (if search-successful?
                         (set-entry-value! search-breakpoint value)
                         (adjust-entry! (make-entry key value) search-breakpoint entry)))
                    (else
                     (if search-successful?
                         (iter! (mcdr keys) search-breakpoint)
                         (let ((new-entry (make-entry key '())))
                           (adjust-entry! new-entry search-breakpoint entry)
                           (iter! (mcdr keys) new-entry)))))))))

      (if (null? keys)
          (error "Empty keys -- INSERT!")
          (iter! keys table))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc)
             (lambda (keys) (lookup keys local-table)))
            ((eq? m 'insert-proc!)
             (lambda (keys value) (insert! keys value local-table)))
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

(define (lookup table keys)
  ((table 'lookup-proc) keys))

(define (insert! table keys value)
  ((table 'insert-proc!) keys value))

(define table1 (make-table order-proc))

(check-false (lookup table1 (mlist 1)))

(insert! table1 (mlist 1) 111)
(check-equal? (lookup table1 (mlist 1)) 111)
(insert! table1 (mlist 1 5) 555)
(insert! table1 (mlist 1 3) 333)
(insert! table1 (mlist 1 3 4) 444)

(check-equal? (lookup table1 (mlist 1)) 111)
(check-equal? (lookup table1 (mlist 1 5)) 555)
(check-equal? (lookup table1 (mlist 1 3)) 333)
(check-equal? (lookup table1 (mlist 1 3 4)) 444)
(check-false (lookup table1 (mlist 2)))

(insert! table1 (mlist 1 3) 3333)
(insert! table1 (mlist 1 3 4) 4444)

(check-equal? (lookup table1 (mlist 1 3)) 3333)
(check-equal? (lookup table1 (mlist 1 3 4)) 4444)
