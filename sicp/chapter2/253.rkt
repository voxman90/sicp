#lang sicp

#|
  Упражнение 2.53

  Что напечатает интерпретатор в ответ на каждое из следующих выражений?

    (list 'a 'b 'c)

    (list (list 'george))

    (cdr '((x1 x2) (y1 y2)))

    (cadr '((x1 x2) (y1 y2)))

    (pair? (car '(a short list)))

    (memq 'red '((red shoes) (blue socks)))

    (memq 'red '(red shoes blue socks))
|#

(#%require rackunit)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(check-equal? (list 'a 'b 'c) '(a b c))               ; (a b c)
(check-equal? (list (list 'george)) '((george)))      ; ((george))
(check-equal? (cdr '((x1 x2) (y1 y2))) '((y1 y2)))    ; ((y1 y2))
(check-equal? (cadr '((x1 x2) (y1 y2))) '(y1 y2))     ; (y1 y2)
(check-equal? (pair? (car '(a short list))) #f)       ; #f
(check-equal? (memq 'red '((red shoes) (blue socks))) ; #f
                          #f)
(check-equal? (memq 'red '(red shoes blue socks))     ; (red shoes blue socks)
                         '(red shoes blue socks))
