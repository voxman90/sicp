#lang sicp

#|
  Упражнение 2.69

  Следующая процедура берет в качестве аргумента список пар вида символ-частота (где ни один символ не
  встречается более, чем в одной паре) и порождает дерево кодирования по Хаффману в соответствии с
  алгоритмом Хаффмана.

    (define (generate-huffman-tree pairs)
      (successive-merge (make-leaf-set pairs)))

  Приведенная выше процедура make-leaf-set преобразует список пар в упорядоченное множество пар. Вам
  нужно написать процедуру successive-merge, которая при помощи make-code-tree сливает наиболее легкие
  элементы множества, пока не останется только один элемент, который и представляет собой требуемое
  дерево Хаффмана. (Эта процедура устроена немного хитро, но она не такая уж сложная. Если Вы видите,
  что строите сложную процедуру, значит, почти наверняка Вы делаете что-то не то. Можно извлечь немалое
  преимущество из того, что мы используем упорядоченное представление для множеств.)
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

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

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

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
       '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))

  (decode-1 bits tree))

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

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-decode-result '(A D A B B C A))

(define generated-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(check-equal? (decode sample-message sample-tree)
              (decode sample-message generated-tree))
(check-equal? (encode sample-decode-result sample-tree)
              (encode sample-decode-result generated-tree))
