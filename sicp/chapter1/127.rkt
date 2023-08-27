#lang sicp

#|
  Упражнение 1.27

  Числа Кармайкла
  Покажите, что числа Кармайкла (несколько первых это: 561, 1105, 1729, 2465, 2821 и 6601)
  действительно «обманывают» тест Ферма: напишите процедуру carmichael-test, которая берет
  целое число n и проверяет, правда ли a в степени n равняется a по модулю n для всех a < n,
  и проверьте эту процедуру на этих числах Кармайкла.
|#

(#%require rackunit)

(define (square n)
  (* n n))

(define (for i pred? inc body)
  (if (pred? i)
      (begin
        (body i)
        (for (inc i) pred? inc body))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (prime? n div)
  (cond ((= div 1) true)
        ((= (expmod div n n) div) (prime? n (- div 1)))
        (else false)))

(define (carmichael-test n)
  (prime? n (- n 1)))

(define (print-carmichael-test-result n)
  (map display
    (list "(carmichael-test " n ") => " (carmichael-test n)))
  (display "\n"))

(define CARMICHAEL_NUMBERS '(561 1105 1729 2465 2821 6601))
(define PRIME_NUMBERS '(2 3 5 7 11 563 1103 1733 2467 2819 6599))
(define NON_PRIME_NUMBERS '(1 4 562 1104 1735 2469 2820 6598))

((lambda ()
  (display "Carmichael numbers:\n")
  (map print-carmichael-test-result CARMICHAEL_NUMBERS)
  (newline)))

((lambda ()
  (display "Prime numbers:\n")
  (map print-carmichael-test-result PRIME_NUMBERS)
  (newline)))

((lambda ()
  (display "Non prime numbers:\n")
  (map print-carmichael-test-result NON_PRIME_NUMBERS)
  (newline)))

(check-equal? (carmichael-test 4) #f)
(check-equal? (carmichael-test 3) #t)
(check-equal? (carmichael-test 561) #t)
(check-equal? (carmichael-test 1105) #t)
(check-equal? (carmichael-test 1729) #t)
(check-equal? (carmichael-test 2465) #t)
(check-equal? (carmichael-test 2821) #t)
(check-equal? (carmichael-test 6601) #t)
