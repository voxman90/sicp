#lang racket

#|
  Упражнение 2.51

  Определите для рисовалок операцию below. Below принимает в качестве аргументов две рисовалки. Когда
  получившейся рисовалке передается рамка, она рисует в нижней ее половине при помощи первой рисовалки,
  а в верхней при помощи второй. Определите below двумя способами — один раз аналогично процедуре beside,
  как она приведена выше, а второй раз через beside и операции вращения (см. упражнение 2.50).
|#

(#%require sicp-pict
           racket/class)

(define cat (bitmap->painter "./sicp/chapter2/images/cat.png"))

(define (paint-to-png painter filename)
  (define snip (paint painter))
  (define bitmap (send snip get-bitmap))
  (send bitmap save-file filename 'png))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
                                         (make-vect 0.0 0.0)
                                         split-point
                                         (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2
                                          split-point
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate180 (rotate90 painter)))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up (transform-painter painter1
                                       split-point
                                       (make-vect 1.0 0.5)
                                       (make-vect 0.0 1.0)))
          (paint-down (transform-painter painter2
                                         (make-vect 0.0 0.0)
                                         (make-vect 1.0 0.0)
                                         split-point)))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

(define (below-alt painter1 painter2)
  (rotate90 (beside (rotate270 painter2) (rotate270 painter1))))

(paint-to-png (below cat einstein) "./sicp/chapter2/images/2.51-below.png")
(paint-to-png (below-alt cat einstein) "./sicp/chapter2/images/2.51-below-alt.png")
