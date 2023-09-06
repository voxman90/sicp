#lang racket

#|
  Упражнение 2.49

  С помощью segments->painter определите следующие элементарные рисовалки:

  а. Рисовалку, которая обводит указанную рамку

  б. Рисовалку, которая рисует «Х», соединяя противоположные концы рамки.

  в. Рисовалку, которая рисует ромб, соединяя между собой середины сторон рамки.

  г. Рисовалку wave.
|#

(#%require sicp-pict
           racket/class)

(define (paint-to-png painter filename)
  (define snip (paint painter))
  (define bitmap (send snip get-bitmap))
  (send bitmap save-file filename 'png))

(define (make-border frame)
  (let ((origin (frame-origin frame))
        (edge1  (frame-edge1 frame))
        (edge2  (frame-edge2 frame)))
    (let ((diagonal (vector-add edge1 edge2)))
      (list
        (make-segment origin edge1)
        (make-segment origin edge2)
        (make-segment edge1 diagonal)
        (make-segment edge2 diagonal)))))

(define (make-cross frame)
  (let ((origin (frame-origin frame))
        (edge1  (frame-edge1 frame))
        (edge2  (frame-edge2 frame)))
    (let ((diagonal (vector-add edge1 edge2)))
      (list
        (make-segment edge1 edge2)
        (make-segment origin diagonal)))))

(define (make-rhomb frame)
  (define edge1 (frame-edge1 frame))
  (define edge2  (frame-edge2 frame))
  (define half-edge-1 (vector-scale 0.5 edge1))
  (define half-edge-2 (vector-scale 0.5 edge2))
  (define half-edge-3 (vector-add edge2 half-edge-1))
  (define half-edge-4 (vector-add edge1 half-edge-2))

  (list
    (make-segment half-edge-1 half-edge-2)
    (make-segment half-edge-2 half-edge-3)
    (make-segment half-edge-3 half-edge-4)
    (make-segment half-edge-4 half-edge-1)))

(define (make-wave frame)
  (define (percent-to-vector px py)
    (vector-add (vector-scale (/ px 100) (frame-edge1 frame))
                (vector-scale (/ py 100) (frame-edge2 frame))))

  (list
    (make-segment (percent-to-vector 25 0)
                  (percent-to-vector 35 50))
    (make-segment (percent-to-vector 35 50)
                  (percent-to-vector 30 60))
    (make-segment (percent-to-vector 30 60)
                  (percent-to-vector 15 40))
    (make-segment (percent-to-vector 15 40)
                  (percent-to-vector 0 65))

    (make-segment (percent-to-vector 0 85)
                  (percent-to-vector 15 60))
    (make-segment (percent-to-vector 15 60)
                  (percent-to-vector 30 65))
    (make-segment (percent-to-vector 30 65)
                  (percent-to-vector 40 65))
    (make-segment (percent-to-vector 40 65)
                  (percent-to-vector 35 85))
    (make-segment (percent-to-vector 35 85)
                  (percent-to-vector 40 100))

    (make-segment (percent-to-vector 60 100)
                  (percent-to-vector 65 85))
    (make-segment (percent-to-vector 65 85)
                  (percent-to-vector 60 65))
    (make-segment (percent-to-vector 60 65)
                  (percent-to-vector 75 65))
    (make-segment (percent-to-vector 75 65)
                  (percent-to-vector 100 35))

    (make-segment (percent-to-vector 100 15)
                  (percent-to-vector 65 40))
    (make-segment (percent-to-vector 65 40)
                  (percent-to-vector 75 0))

    (make-segment (percent-to-vector 60 0)
                  (percent-to-vector 50 30))
    (make-segment (percent-to-vector 50 30)
                  (percent-to-vector 40 0))))

(define frame1 (make-frame (make-vect 0. 0.)
                           (make-vect 1. 0.)
                           (make-vect 0. 1.)))

(paint-to-png (segments->painter (make-border frame1)) "./sicp/chapter2/images/2.49-border.png")
(paint-to-png (segments->painter (make-cross frame1)) "./sicp/chapter2/images/2.49-cross.png")
(paint-to-png (segments->painter (make-rhomb frame1)) "./sicp/chapter2/images/2.49-rhomb.png")
(paint-to-png (segments->painter (make-wave frame1)) "./sicp/chapter2/images/2.49-wave.png")
