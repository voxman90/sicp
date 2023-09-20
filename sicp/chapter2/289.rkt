#lang racket

#|
  Упражнение 2.89

  Определите процедуры, которые реализуют представление в виде списка термов, описанное выше как подходящее
  для плотных многочленов.
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

; Процедуры для присоединения тега, извлечения тега и извлечения контента данных

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) datum)
        (else (error "Incorrect tagged data -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Incorrect tagged data -- CONTENTS" datum))))

(define (apply-generic op . args)
  (define type-tags (map type-tag args))

  (define (send-error)
    (error "There is no method for this types -- APPLY-GENERIC"
           (list op type-tags)))

  (let ((proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (send-error))))

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))

  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p)
    (car p))

  (define (term-list p)
    (cdr p))

  (define (variable? x)
    (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  (define (trim-term-list L)
    (cond ((null? L) L)
          ((= 0 (coeff (first-term L))) ; zero?
           (trim-term-list (rest-terms L)))
          (else L)))

  (define (equ-poly? p1 p2)
    (define (iter L1 L2)
      (if (empty-termlist? L1)
          #t
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
            (if (= (coeff t1) (coeff t2)) ; equ?
                (iter (rest-terms L1) (rest-terms L2))
                #f))))

    (let ((L1 (trim-term-list (term-list p1)))
          (L2 (trim-term-list (term-list p2))))
      (if (not (= (length L1) (length L2)))
          #f
          (iter L1 L2))))

  (define (zero-poly? p)
    (null? (trim-term-list (term-list p))))

  (define (apply-op-poly op error-msg p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (op (term-list p1)
                       (term-list p2)))
        (error error-msg
               (list p1 p2))))

  (define (add-poly p1 p2)
    (apply-op-poly add-terms
                   "Polynomials of different variables -- ADD-POLY"
                   p1 p2))

  (define (sub-poly p1 p2)
    (apply-op-poly sub-terms
                   "Polynomials of different variables -- SUB-POLY"
                   p1 p2))

  (define (mul-poly p1 p2)
    (apply-op-poly mul-terms
                   "Polynomials of different variables -- MUL-POLY"
                   p1 p2))

  (define (adjoin-term term term-list)
    (define term-order (order term))

    (define (iter L head-order item)
      (cond ((< head-order term-order)
             (cons item (iter L
                              (+ head-order 1)
                              0)))
            ((= head-order term-order)
             (cons (+ item ; add
                      (coeff (first-term L)))
                   (rest-terms L)))
            (else
             (cons (first-term L)
                   (iter (rest-terms L)
                         (- head-order 1)
                         item)))))

    (cond ((= 0 (coeff term)) term-list) ; zero?
          ((null? term-list) term)
          (else
           (let ((head (first-term term-list)))
                (iter head
                      (order head)
                      (coeff term))))))

  (define (the-empty-termlist) '())

  (define (first-term term-list)
    term-list)

  (define (rest-terms term-list)
    (cdr term-list))

  (define (empty-termlist? term-list)
    (null? term-list))

  (define (zero-termlist? term-list)
    (if (null? term-list)
        #t
        (let ((term (first-term term-list)))
          (if (= (coeff term) 0)
              (zero-termlist? (rest-terms term-list))
              #f))))

  (define (make-term order coeff)
    (define (iter length acc)
      (cond ((= length 0) acc)
            ((= length 1) (cons coeff acc))
            (else (iter (- length 1)
                        (cons 0 acc)))))

    (iter (+ order 1) '()))

  (define (order term)
    (- (length term) 1))

  (define (coeff term)
    (car term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (+ (coeff t1) (coeff t2))) ; add
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L1) (reverse-terms-sign L2))
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     (make-term (order t2)
                                (reverse-sign (coeff t2)))
                     (sub-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (- (coeff t1) (coeff t2))) ; sub
                     (sub-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (reverse-terms-sign terms)
    (if (null? terms)
       '()
        (let ((term (first-term terms)))
          (adjoin-term (make-term (order term)
                                  (* -1 (coeff term))) ; reverse-sign
                       (reverse-terms-sign (rest-terms terms))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (* (coeff t1) (coeff t2))) ; mul
         (mul-term-by-all-terms t1 (rest-terms L))))))

  (put 'make 'polynomial
    (lambda (var terms) (tag (make-poly var terms))))
  (put 'equ? '(polynomial polynomial) equ-poly?)
  (put 'zero? '(polynomial) zero-poly?)
  (put 'reverse-sign '(polynomial)
    (lambda (x) (tag (make-poly (variable x) 
                                (reverse-terms-sign (term-list x))))))
  (put 'add '(polynomial polynomial)
    (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
    (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
    (lambda (p1 p2) (tag (mul-poly p1 p2))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (add x y)
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (equ? datum1 datum2)
  (apply-generic 'equ? datum1 datum2))

(define (=zero? datum)
  (apply-generic 'zero? datum))

(define (reverse-sign datum)
  (apply-generic 'reverse-sign datum))

(install-polynomial-package)

(define p1 (make-polynomial 'x '(5 5 5 5 5)))
(define p2 (make-polynomial 'x '(1 0 1 0 1)))
(define p3 (make-polynomial 'x '(0 1 0 1 0)))

(check-equal? (=zero? p1) #f)
(check-equal? (=zero? p2) #f)
(check-equal? (=zero? p3) #f)
(check-equal? (=zero? (make-polynomial 'x '())) #t)
(check-equal? (=zero? (make-polynomial 'x '(0 0 0 0 0))) #t)

(check-equal? (equ? p1 p2) #f)
(check-equal? (equ? p2 p3) #f)
(check-equal? (equ? p3 p1) #f)
(check-equal? (equ? p3 p3) #t)
(check-equal? (equ? p1 (make-polynomial 'x '(0 0 5 5 5 5 5))) #t)

(check-equal? (equ? (reverse-sign p1) (make-polynomial 'x '(-5 -5 -5 -5 -5))) #t)
(check-equal? (equ? (reverse-sign p2) (make-polynomial 'x '(-1  0 -1  0 -1))) #t)
(check-equal? (equ? (reverse-sign p3) (make-polynomial 'x '( 0 -1  0 -1  0))) #t)
(check-equal? (equ? (reverse-sign (reverse-sign p3)) p3) #t)

(check-equal? (equ? (add p2 p3) (make-polynomial 'x '(1 1 1 1 1))) #t)
(check-equal? (equ? (add p1 p3) (make-polynomial 'x '(5 6 5 6 5))) #t)
(check-equal? (equ? (add p1 p2) (make-polynomial 'x '(6 5 6 5 6))) #t)
(check-equal? (equ? (add p1 (make-polynomial 'x '())) p1) #t)
(check-equal? (equ? (add (make-polynomial 'x '()) p1) p1) #t)

(check-equal? (equ? (sub p2 p3) (make-polynomial 'x '(1 -1  1 -1  1))) #t)
(check-equal? (equ? (sub p1 p3) (make-polynomial 'x '(5  4  5  4  5))) #t)
(check-equal? (equ? (sub p1 p2) (make-polynomial 'x '(4  5  4  5  4))) #t)
(check-equal? (equ? (sub p1 (make-polynomial 'x '())) p1) #t)
(check-equal? (equ? (sub (make-polynomial 'x '()) p1) (reverse-sign p1)) #t)

(check-equal? (equ? (mul p1 (make-polynomial 'x '(1)))
                    (make-polynomial 'x '(5 5 5 5 5))) #t)
(check-equal? (equ? (mul p2 (make-polynomial 'x '(2)))
                    (make-polynomial 'x '(2 0 2 0 2))) #t)
(check-equal? (equ? (mul (make-polynomial 'x '(1  1))
                         (make-polynomial 'x '(1 -1)))
                    (make-polynomial 'x '(1 0 -1))) #t)
(check-equal? (equ? (mul p1 (make-polynomial 'x '())) (make-polynomial 'x '())) #t)
