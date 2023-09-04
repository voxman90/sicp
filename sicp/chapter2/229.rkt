#lang sicp

#|
  Упражнение 2.29

  Бинарный мобиль состоит из двух ветвей, левой и правой. Каждая ветвь представляет собой стержень
  определенной длины, с которого свисает либо гирька, либо еще один бинарный мобиль. Мы можем представить
  бинарный мобиль в виде составных данных, соединив две ветви (например, с помощью list):

    (define (make-mobile left right)
      (list left right))

  Ветвь составляется из длины length (которая должна быть числом) и структуры structure, которая может
  быть либо числом (представляющим простую гирьку), либо еще одним мобилем:

    (define (make-branch length structure)
      (list length structure))

  а. Напишите соответствующие селекторы left-branch и right-branch, которые возвращают левую и правую
  ветви мобиля, а также branch-length и branch-structure, которые возвращают компоненты ветви.

  б. С помощью этих селекторов напишите процедуру total-weight, которая возвращает общий вес мобиля.

  в. Говорят, что мобиль сбалансирован, если момент вращения, действующий на его левую ветвь, равен
  моменту вращения, действующему на правую ветвь (то есть длина левого стержня, умноженная на вес груза,
  свисающего с него, равна соответствующему произведению для правой стороны), и если все подмобили,
  свисающие с его ветвей, также сбалансированы. Напишите предикат, который проверяет мобили на
  сбалансированность.

  г. Допустим, мы изменили представление мобилей, так что конструкторы теперь приняли такой вид:

    (define (make-mobile left right)
      (cons left right))
    (define (make-branch length structure)
      (cons length structure))

  Как много Вам нужно изменить в программах, чтобы перейти на новое представление?
|#

(#%require rackunit)

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (cond ((pair? mobile)
         (let ((lb (left-branch mobile))
               (rb (right-branch mobile)))
           (+ (total-weight (branch-structure lb))
              (total-weight (branch-structure rb)))))
         ((null? mobile) 0)
         (else mobile)))

(define (mobile-balanced? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))

  (cond ((pair? mobile)
         (let ((lb (left-branch mobile))
               (rb (right-branch mobile)))
           (and (= (torque lb)
                   (torque rb))
                (mobile-balanced? (branch-structure lb))
                (mobile-balanced? (branch-structure rb)))))
        (else #t)))

(define nested-mobile (make-mobile (make-branch 4 5)
                                   (make-branch 6 7)))
(define mobile-a (make-mobile (make-branch 1 2)
                              (make-branch 3
                                           nested-mobile)))
(define mobile-b (make-mobile (make-branch 5 2)
                              (make-branch 1 10)))

(check-equal? (total-weight mobile-a) 14)
(check-equal? (total-weight mobile-b) 12)
(check-equal? (mobile-balanced? mobile-a) #f)
(check-equal? (mobile-balanced? mobile-b) #t)

(define (make-mobile-alt left right)
  (cons left right))

(define (make-branch-alt length structure)
  (cons length structure))

(define (right-branch-alt mobile)
  (cdr mobile))

(define (branch-structure-alt branch)
  (cdr branch))

(define (total-weight-alt mobile)
  (cond ((pair? mobile)
         (let ((lb (left-branch mobile))
               (rb (right-branch-alt mobile)))
           (+ (total-weight-alt (branch-structure-alt lb))
              (total-weight-alt (branch-structure-alt rb)))))
         ((null? mobile) 0)
         (else mobile)))

(define (mobile-balanced-alt? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (total-weight-alt (branch-structure-alt branch))))

  (cond ((pair? mobile)
         (let ((lb (left-branch mobile))
               (rb (right-branch-alt mobile)))
           (and (= (torque lb)
                   (torque rb))
                (mobile-balanced-alt? (branch-structure-alt lb))
                (mobile-balanced-alt? (branch-structure-alt rb)))))
        (else #t)))

(define nested-mobile-alt (make-mobile-alt (make-branch-alt 4 5)
                                           (make-branch-alt 6 7)))
(define mobile-a-alt (make-mobile-alt (make-branch-alt 1 2)
                                      (make-branch-alt 3
                                                       nested-mobile-alt)))
(define mobile-b-alt (make-mobile-alt (make-branch-alt 5 2)
                                      (make-branch-alt 1 10)))

(check-equal? (total-weight-alt mobile-a-alt) 14)
(check-equal? (total-weight-alt mobile-b-alt) 12)
(check-equal? (mobile-balanced-alt? mobile-a-alt) #f)
(check-equal? (mobile-balanced-alt? mobile-b-alt) #t)

#|
  На деле, понадобилось всего два изменения - заменить cadr на car в двух процедурах.
|#
