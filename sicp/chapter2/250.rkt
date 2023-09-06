#lang racket

#|
  Упражнение 2.50

  Определите преобразование flip-horiz, которое обращает изображение вокруг горизонтальной оси, а также
  преобразования, которые вращают рисовалки против часовой стрелки на 180 и 270 градусов.
|#

(#%require sicp-pict
           racket/class)

(define cat (bitmap->painter "./sicp/chapter2/images/cat.png"))

(define (paint-to-png painter filename)
  (define snip (paint painter))
  (define bitmap (send snip get-bitmap))
  (send bitmap save-file filename 'png))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate180 (rotate90 painter)))

(define (rotate360 painter)
  (rotate180 (rotate180 painter)))

(paint-to-png (rotate360 cat) "./sicp/chapter2/images/2.50-rotate360.png")
(paint-to-png (rotate270 cat) "./sicp/chapter2/images/2.50-rotate270.png")
(paint-to-png (rotate180 cat) "./sicp/chapter2/images/2.50-rotate180.png")
(paint-to-png (rotate90 cat) "./sicp/chapter2/images/2.50-rotate90.png")
(paint-to-png (flip-horiz cat) "./sicp/chapter2/images/2.50-flip-hotiz.png")
