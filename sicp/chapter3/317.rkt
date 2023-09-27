#lang sicp

#|
  Упражнение 3.17

  Напишите правильную версию процедуры count-pairs из упражнения 3.16 которая возвращает число различных
  пар в любой структуре. (Подсказка: просматривайте структуру, поддерживая при этом вспомогательную
  структуру, следящую за тем, какие пары уже были посчитаны.)
|#

(#%require rackunit)

(define (count-pairs x)
  (let ((passed-cons '()))
    (define (rec x)
      (if (not (pair? x))
          0
          (if (memq x passed-cons)
              0
              (begin
                 (set! passed-cons (cons x passed-cons))
                 (+ (rec (car x))
                    (rec (cdr x))
                    1)))))

    (rec x)))

(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define C3 (cons x y))

(define z (cons 'c x))
(define C4 (cons z x))

(define w (cons x x))
(define C7 (cons w w))

(define inf (cons 'c x))
(define Cinf (cons x inf))
(set-cdr! inf Cinf)

(check-equal? (count-pairs '()) 0)
(check-equal? (count-pairs '(a . b)) 1)
(check-equal? (count-pairs C3) 3)
(check-equal? (count-pairs C4) 3)
(check-equal? (count-pairs C7) 3)
(check-equal? (count-pairs Cinf) 3)
