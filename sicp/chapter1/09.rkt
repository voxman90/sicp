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

  (+rec 4 5) =>
  (inc (+rec (dec 4) 5)) => (inc (+rec 3 5)) =>
  (inc (inc (+rec (dec 3) 5))) => ... =>
  (inc (inc (inc (+rec (dec 2) 5)))) => ... =>
  (inc (inc (inc (inc (+rec (dec 1) 5))))) =>
  (inc (inc (inc (inc (+rec 0 5))))) =>
  (inc (inc (inc (inc 5)))) =>
  (inc (inc (inc 6))) =>
  (inc (inc 7)) =>
  (inc 8) =>
  9

  (+iter 4 5) => (+iter (dec 4) (inc 5)) =>
  (+iter 3 6) => ... =>
  (+iter 2 7) => ... =>
  (+iter 1 8) => ... =>
  (+iter 0 9) =>
  9
|#
