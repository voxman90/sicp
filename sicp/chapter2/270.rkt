#lang sicp

#|
  Упражнение 2.70

  Нижеприведенный алфавит из восьми символов с соответствующими им относительными частотами был разработан,
  чтобы эффективно кодировать слова рок-песен 1950-х годов. (Обратите внимание, что «символы» «алфавита»
  не обязаны быть отдельными буквами.)

    A     2   NA   16
    BOOM  1   SHA  3
    GET   2   YIP  9
    JOB   2   WAH  1

  При помощи generate-huffman-tree (упр. 2.69) породите соответствующее дерево Хаффмана, и с помощью
  encode (упр. 2.68) закодируйте следующее сообщение:

    Get a job

    Sha na na na na na na na na

    Get a job

    Sha na na na na na na na na

    Wah yip yip yip yip yip yip yip yip yip

    Sha boom

  Сколько битов потребовалось для кодирования? Каково наименьшее число битов, которое потребовалось бы
  для кодирования этой песни, если использовать код с фиксированной длиной для алфавита из восьми
  символов?
|#

(#%require rackunit)

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
     '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (include-symbol? symbol branch)
  (define (include? symbols)
    (cond ((null? symbols) #f)
          ((eq? symbol (car symbols)) #t)
          (else (include? (cdr symbols)))))

  (include? (symbols branch)))

(define (encode-symbol symbol tree)
  (define (encode-1 current-branch)
    (cond ((leaf? current-branch)
          '())
          ((include-symbol? symbol (left-branch current-branch))
           (cons 0 (encode-1 (left-branch current-branch))))
          (else
           (cons 1 (encode-1 (right-branch current-branch))))))

  (if (not (include-symbol? symbol tree))
      (error "symbol not included -- ENCODE-SYMBOL")
      (encode-1 tree)))

(define (encode message tree)
  (if (null? message)
     '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (successive-merge ordered-leaf-set)
  (define (merge ordered-set)
    (if (null? (cdr ordered-set))
        (car ordered-set)
        (successive-merge
          (adjoin-set
            (make-code-tree (car ordered-set) (cadr ordered-set))
            (cddr ordered-set)))))

  (if (null? ordered-leaf-set)
     '()
      (merge ordered-leaf-set)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define rock-song
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))

(define rock-song-uppercase
  (map
    (lambda (str)
      (string->symbol
        (list->string
          (map
            (lambda (char) (char-upcase char))
            (string->list (symbol->string str))))))
    rock-song))

(define dictionary '((BOOM 1) (WAH 1) (A 2) (GET 2)
                     (JOB 2) (SHA 3) (YIP 9) (NA 16)))

(define huffman-tree (generate-huffman-tree dictionary))

(check-equal? (length (encode rock-song-uppercase huffman-tree)) 84)

#|
  Для кодирования рок-песни понадобилось 84 бита.

  Для кодирования восьми символов кодом с фиксированной длинной понадобилось бы по 3 бита на символ и,
  в сумме: (16 + 9 + 3 + 2 + 2 + 2 + 1 + 1)*3 = 108 бит.
|#

(define dictionary-alt '((BOOM 1) (WAH 1) (A 1) (GET 1)
                         (JOB 1) (SHA 1) (YIP 1) (NA 1)))

(define huffman-tree-alt (generate-huffman-tree dictionary-alt))

(check-equal? (length (encode rock-song-uppercase huffman-tree-alt)) 108)
