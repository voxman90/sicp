#lang racket

#|
  Упражнение 2.52

  Измените предел квадрата рисовалки wave, показанный на рисунке 2.9, работая на каждом из вышеописанных
  уровней. А именно:

  а. Добавьте новые отрезки к элементарной рисовалке wave из упражнения 2.49 (например, изобразив улыбку).

  б. Измените шаблон, который порождает corner-split (например, используя только одну копию образов
  up-split и right-split вместо двух).

  в. Измените версию square-limit, использующую square-of-four, так, чтобы углы компоновались как-нибудь
  по-другому. (Например, можно сделать так, чтобы большой мистер Роджерс выглядывал из каждого угла
  квадрата.)
|#

(#%require sicp-pict
           racket/class)

(define cat (bitmap->painter "./sicp/chapter2/images/cat.png"))

(define (paint-to-png painter filename)
  (define snip (paint painter))
  (define bitmap (send snip get-bitmap))
  (send bitmap save-file filename 'png))

#|
  a.
|#

(define (make-wave frame)
  (define edge1 (frame-edge1 frame))
  (define edge2 (frame-edge2 frame))

  (define (percent-to-vector px py)
    (vector-add (vector-scale (/ px 100) edge1)
                (vector-scale (/ py 100) edge2)))

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

(define (wave-with-smile frame)
  (define (percent-to-vector px py)
    (vector-add (vector-scale (/ px 100) (frame-edge1 frame))
                (vector-scale (/ py 100) (frame-edge2 frame))))

  (append
    (make-wave frame)
    (list
      (make-segment (percent-to-vector 42 77)
                    (percent-to-vector 50 75))
      (make-segment (percent-to-vector 50 75)
                    (percent-to-vector 58 77)))))

#|
  b.
|#

(define (up-split-alt painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split-alt painter (- n 1))))
        (below painter smaller))))

(define (right-split-alt painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split-alt painter (- n 1))))
        (beside painter smaller))))

(define (corner-split-alt painter n)
  (if (= n 0)
      painter
      (let ((up (up-split-alt painter (- n 1)))
            (right (right-split-alt painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split-alt painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

#|
  c.
|#

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

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

(define (square-limit-alt painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (rotate180 (corner-split painter n)))))

(define frame1 (make-frame (make-vect 0.0 0.0)
                           (make-vect 1.0 0.0)
                           (make-vect 0.0 1.0)))

(paint-to-png (segments->painter (wave-with-smile frame1)) "./sicp/chapter2/images/2.52-wave.png")
(paint-to-png (corner-split-alt cat 3) "./sicp/chapter2/images/2.52-corner-split-alt.png")
(paint-to-png (square-limit-alt cat 5) "./sicp/chapter2/images/2.52-square-limit-alt.png")
