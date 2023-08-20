#lang sicp

(define (+rec a b)
  (if (= a 0)
      b
      (inc (+rec (dec a) b))))

(define (+iter a b)
  (if (a = 0)
      b
      (+iter (dec a) (inc b))))

#| 
  Обе процецедуры рекурсивны, так как в их определении присутвует
  ссылка на саму себя.
  Первая процедура определяет рекурсивный процесс, а вторая
  итеративный.

  (+rec 2 1) =>
  (inc (+rec (dec 2) 1)) => (inc (+rec 1 1)) =>
  (inc (inc (+rec (dec 1) 1)) => (inc (inc (+rec 0 1))) =>
  (inc (inc 1)) => (inc 2) => 3

  (+iter 2 1) =>
  (+iter (dec 2) (inc 1)) => (+iter 1 2) =>
  (+iter (dec 1) (inc 2)) => (+iter 0 3) =>
  3
|#
