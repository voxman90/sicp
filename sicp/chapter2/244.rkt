#lang racket

#|
  Упражнение 2.44

  Определите процедуру up-split, которую использует corner-split. Она подобна right-split, но только
  меняет местами роли below и beside.
|#

(require sicp-pict
         racket/class)

(define cat (bitmap->painter "./sicp/chapter2/images/cat.png"))

(define (paint-to-png painter filename)
  (define snip (paint painter))
  (define bitmap (send snip get-bitmap))
  (send bitmap save-file filename 'png))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint-to-png (corner-split cat 5) "./sicp/chapter2/images/2.44.png")
