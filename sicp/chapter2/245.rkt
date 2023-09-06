#lang racket

#|
  Упражнение 2.45

  Right-split и up-split можно выразить как разновидности общей операции разделения. Определите процедуру
  split с таким свойством, что вычисление

    (define right-split (split beside below))

    (define up-split (split below beside))

  порождает процедуры right-split и up-split с таким же поведением, как и определенные ранее.
|#

(require sicp-pict
         racket/class)

(define cat (bitmap->painter "./sicp/chapter2/images/cat.png"))

(define (paint-to-png painter filename)
  (define snip (paint painter))
  (define bitmap (send snip get-bitmap))
  (send bitmap save-file filename 'png))

(define (split before after)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (- n 1))))
          (before painter (after smaller smaller)))))

  (lambda (painter n)
    (rec painter n)))

(define right-split (split beside below))

(define up-split (split below beside))

(paint-to-png (right-split cat 5) "./sicp/chapter2/images/2.45-right-split.png")
(paint-to-png (up-split cat 5) "./sicp/chapter2/images/2.45-up-split.png")
