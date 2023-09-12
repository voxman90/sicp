#lang racket

#|
  Упражнение 2.74

  Insatiable Enterprises, Inc. — децентрализованная компания-конгломерат, которая состоит из большого
  количества независимых подразделений, раскиданных по всему миру. Недавно вычислительные мощности
  компании были связаны умной вычислительной сетью, создающей для пользователя иллюзию, что он работает
  с единым компьютером Президент компании, когда она в первый раз пытается воспользоваться способностью
  системы осуществлять доступ к файлам подразделений, с изумлением и ужасом обнаруживает, что, несмотря
  на то, что все эти файлы реализованы в виде структур данных на Scheme, конкретная структура данных
  отличается от подразделения к подразделению. Спешно созывается совещание менеджеров подразделений,
  чтобы найти стратегию, которая позволила бы собрать файлы в единую систему для удовлетворения нужд
  главного офиса, и одновременно сохранить существующую автономию подразделений.

  Покажите, как такую стратегию можно реализовать при помощи программирования, управляемого данными.
  К примеру, предположим, что сведения о персонале каждого подразделения устроены в виде единого файла,
  который содержит набор записей, проиндексированных по имени служащего. Структура набора данных от
  подразделения к подразделению различается. Более того, каждая запись сама по себе — набор сведений
  (в разных подразделениях устроенный по-разному), в котором информация индексируется метками вроде
  address (адрес) или salary (зарплата). В частности:

  а. Для главного офиса реализуйте процедуру get-record, которая получает запись, относящуюся к
  указанному служащему, из указанного файла персонала. Процедура должна быть применима к файлу любого
  подразделения. Объясните, как должны быть структурированы файлы отдельных подразделений. В частности,
  какую информацию о типах нужно хранить?

  б. Для главного офиса реализуйте процедуру get-salary, которая возвращает зарплату указанного служащего
  из файла любого подразделения. Как должна быть устроена запись, чтобы могла работать эта процедура?

  в. Для главного офиса напишите процедуру find-employee-record. Она должна искать в файлах всех
  подразделений запись указанного служащего и возвращать эту запись. Предположим, что в качестве
  аргументов эта процедура принимает имя служащего и список файлов всех подразделений.

  г. Какие изменения требуется внести в систему, чтобы внести в центральную систему информацию о новых
  служащих, когда Insatiable поглощает новую компанию?
|#

(#%require rackunit
           compatibility/mlist)

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
          (let ((record (massoc key-2 (mcdr subtable))))
            (if record
                (mcdr record)
                #f))
          #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
          (let ((record (massoc key-2 (mcdr subtable))))
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

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define (get-division-tag file)
  (car file))

(define (get-division-records file)
  (cdr file))

(define (get-record-tag record)
  (car record))

(define (get-record-content record)
  (cdr record))

#|
  Интерфейсы для подразделений
|#

(define (instal-interface-ins1)
  (define (get-content item)
    (cdr item))

  (define (get-tag item)
    (car item))

  (define (get-field-value field-name items)
    (if (null? items)
        #f
        (let ((item (car items)))
          (if (eq? (get-tag item) field-name)
              (get-content item)
              (get-field-value field-name (cdr items))))))

  (define (get-name record)
    (get-field-value 'name record))

  (define (get-salary record)
    (get-field-value 'salary record))

  (put 'get-name 'ins1-record get-name)
  (put 'get-salary 'ins1-record get-salary))

(define (instal-interface-ins2)
  (define (get-name record)
    (car record))

  (define (get-salary record)
    (caddr (cadr record)))

  (put 'get-name 'ins2-record get-name)
  (put 'get-salary 'ins2-record get-salary))

(instal-interface-ins1)
(instal-interface-ins2)

#|
  а. Для главного офиса реализуйте процедуру get-record, которая получает запись, относящуюся к
  указанному служащему, из указанного файла персонала. Процедура должна быть применима к файлу любого
  подразделения. Объясните, как должны быть структурированы файлы отдельных подразделений. В частности,
  какую информацию о типах нужно хранить?
|#

#|
  Файл должен хранить записи подразделения и файлу должен быть сопоставлен тег\идентификатор по которому
  можно обратиться к стандартизированному интерфейсу для работы с записями подразленеия.
|#

(define (get-record file name)
  (let ((division-tag     (get-division-tag file))
        (division-records (get-division-records file)))
    (find-record
      (lambda (record) (eq? ((get 'get-name division-tag) record)
                            name))
      (lambda (record) (attach-tag division-tag record))
      division-records)))

#|
  в. Для главного офиса напишите процедуру find-employee-record. Она должна искать в файлах всех
  подразделений запись указанного служащего и возвращать эту запись. Предположим, что в качестве
  аргументов эта процедура принимает имя служащего и список файлов всех подразделений.
|#

(define (find-record pred? callback records)
  (define (bypass records)
    (cond ((null? records) '())
          ((pair? records)
           (let ((record (car records)))
             (if (pred? record)
                 (callback record)
                 (bypass (cdr records)))))
          (else
           (error "wrong records type -- FIND-RECORD"))))

  (bypass records))

(define (find-employee-record files name)
  (define (bypass file)
    (cond ((null? file) '())
          ((pair? file)
           (let ((division-records (get-division-records file))
                 (division-tag     (get-division-tag file)))
             (find-record
               (lambda (record) (eq? ((get 'get-name division-tag) record)
                                     name))
               (lambda (record) record)
               division-records)))
          (else
           (error "wrong division file type -- FIND-EMPLOYEE-RECORD"))))

  (if (null? files)
     '()
      (let ((bypass-result (bypass (car files))))
        (if (null? bypass-result)
            (find-employee-record (cdr files) name)
            bypass-result))))

#|
  б. Для главного офиса реализуйте процедуру get-salary, которая возвращает зарплату указанного служащего
  из файла любого подразделения. Как должна быть устроена запись, чтобы могла работать эта процедура?
|#

#|
  Запись должна содержать тег подразделения, чтобы можно было использовать соответствующие методы для
  извлечения нужной информации. Соответственно, ссылки на методы должны быть занесены в таблицу, а их
  названия стандартизированы.
|#

(define (get-salary record)
  ((get 'get-salary (get-record-tag record)) (get-record-content record)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define Ben (list (cons 'name "Bitdiddle Ben") (cons 'address "Slumerville")
                  (cons 'position "computer wizard") (cons 'salary "60000")))

(define Alyssa (list (cons 'name "Hacker Alyssa P") (cons 'address "Cambridge")
                     (cons 'position "computer programmer") (cons 'salary "40000")))

(define ins-div1-records (list Ben Alyssa))

(define Fect (list "Fect Cy D" (list "computer programmer" "Cambridge"  35000)))

(define Tweakit (list "Tweakit Lem E" (list "computer technician" "Boston" 25000)))

(define ins-div2-records (list Fect Tweakit))

(define typed-ins1
  (attach-tag 'ins1-record ins-div1-records))

(define typed-ins2
  (attach-tag 'ins2-record ins-div2-records))

(check-equal? (find-employee-record (list typed-ins1 typed-ins2) "Bitdiddle Ben") Ben)
(check-equal? (find-employee-record (list typed-ins1 typed-ins2) "Tweakit Lem E") Tweakit)
(check-equal? (cdr (get-record typed-ins1 "Hacker Alyssa P")) Alyssa)
(check-equal? (get-record typed-ins2 "Hacker Alyssa P") '())
(check-equal? (cdr (get-record typed-ins2 "Fect Cy D")) Fect)
(check-equal? (get-record typed-ins1 "Fect Cy D") '())
(check-equal? (get-salary (get-record typed-ins1 "Bitdiddle Ben")) "60000")
(check-equal? (get-salary (get-record typed-ins2 "Fect Cy D")) 35000)

#|
  г. Какие изменения требуется внести в систему, чтобы внести в центральную систему информацию о новых
  служащих, когда Insatiable поглощает новую компанию?
|#

#|
  Если рассуждать в рамках задания, то требуется реализовать стандартный интерфейс для нового
  подразделения, установить его и добавить файл с записями о сотрудниках.
|#
