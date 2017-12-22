
(load "../../lib/scm/unit.scm")

; Връща нов списък, съставен от квадратите на елементите в оригиналния.
(define (squares l)
  (map (lambda (x) (* x x))
       l))

(assert-equal '(1 4 9) (squares '(1 2 3)))

; Като горното, но работи със списък от списъци.
(define (squares-2 l)
  (map (lambda (sub-list) (map (lambda (x) (* x x))
                               sub-list))
       l))

(assert-equal '((1 4) (9 16) (25 36))
              (squares-2 '((1 2) (3 4) (5 6))))

; Реализация на filter. Връща елементите от списъка, за които даден предикат
; връща #t.
(define (filter p l)
  (define (helper current-l result)
    (cond
      ((null? current-l) result)
      ((p (car current-l)) (helper (cdr current-l)
                                   (append result (list (car current-l)))))
      (else (helper (cdr current-l) result))))
  (helper l '()))

(assert-equal '(1 3 5 7) (filter odd? '(1 2 3 4 5 6 7 8)))
(assert-equal '() (filter odd? '()))

(define (range start end)
  (define (helper i result)
    (if (< i start)
        result
        (helper (- i 1) (cons i result))))
  (helper end '()))

; accumulate за списъци. Вместо start, end и next работим със списък, който
; съдържа елементите, които трябва да обходим.
(define (accumulate l term comb initial)
  (define (helper current-l result)
    (if (null? current-l)
        result
        (helper (cdr current-l)
                (comb result (term (car current-l))))))
  (helper l initial))

(assert= 14 (accumulate '(1 2 3) (lambda (x) (* x x)) + 0))

(define (even-squares-sum l)
  (accumulate (filter even? l)
              (lambda (x) (* x x))
              +
              0))

(assert= 20 (even-squares-sum '(1 2 3 4)))

; Събира числата в даден списък. Използва това, че + приема произволен брой
; аргументи.
(define (sum l)
  (apply + l))

(assert-equal 6 (sum '(1 2 3)))

; Сума от произведения на подсписъци. Обърнете внимание, че * може да се приложи
; и на едно число. :)
(define (sum-of-products l)
  (apply + (map (lambda (sublist) (apply * sublist))
                l)))

(assert-equal 19 (sum-of-products '((1 2) (3 4) (5))))

; Още един пример за apply - най-малък елемент в матрица.
; Още един пример за apply - най-малък елемент в матрица.
(define (min-matrix m)
  (apply min (map (lambda (l) (apply min l))
                  m)))

(assert-equal 1 (min-matrix '((1 2 3) (4 5 6) (7 8 9))))

(define (get-nth-column m n)
  (map (lambda (row)
         (list-ref row n))
       m))

(assert-equal '(1 4 7) (get-nth-column '((1 2 3) (4 5 6) (7 8 9)) 0))

(define (filter-matrix p m)
  (map (lambda (row)
         (filter p row))
       m))

(assert-equal '((2) (4 6) (8)) (filter-matrix even? '((1 2 3) (4 5 6) (7 8 9))))

; Първи вариант - с функции от по-висок ред. Генерира индексите 0, 1, ..., length(m)-1 и за всеки
; индекс i връща елемента на позиция i, i в матрицата.
(define (diagonal m)
  (map (lambda (i) (list-ref (list-ref m i) i))
       (range 0 (- (length m) 1))))

; Втори вариант - работа изцяло с индекси, но се рекурсия. Отново вземаме i-тия елемент от i-тия
; ред.
(define (diagonal2 m)
  (define (helper i)
    (if (>= i (length m))
        '()
        (cons (list-ref (list-ref m i)
                        i)
              (helper (+ i 1)))))
  (helper 0))

; Трети вариант, без индекси - взема елементът най-горе и най-вляво в матрицата.
; Изпълнява се рекурсивно като от матрицата се махат първия ред и първата колона
; и пак вземаме елемента най-горе и най-вляво (т.е. следващият елемент в
; диагонала).
(define (diagonal3 m)
  (if (null? m)
      '()
      (cons (caar m)
            (diagonal3 (cdr (map cdr m))))))

; Вторичен диагонал с индекси, подобен на diagonal. Разликата е, че смятаме
; съответния индекс на колоната.
(define (second-diagonal m)
  (let ((matrix-length (length m)))
    (map (lambda (i) (list-ref (list-ref m i) (- matrix-length i 1)))
         (range 0 (- matrix-length 1)))))

; Отново вторичен диагонал с индекси, този път рекурсивно.
(define (second-diagonal2 m)
  (define (helper i)
    (if (>= i (length m))
        '()
        (cons (list-ref (list-ref m i)
                        (- (length m) i 1))
              (helper (+ i 1)))))
  (helper 0))

(define (last l)
  (if (null? (cdr l))
      (car l)
      (last (cdr l))))

(define (strip-last l)
  (if (null? (cdr l))
      '()
      (cons (car l)
            (strip-last (cdr l)))))

; Вариант без индекси на вторичния диагонал. Идеята отново е подобна на варианта
; без индекси на главния диагонал (diagonal2) - вземаме елемента най-горе и
; най-вдясно, след което на рекурсивното извикване махаме първия ред и
; последната колона.
; Разбира се, този вариант е по-бавен от горните.
(define (second-diagonal3 m)
  (if (null? m)
      '()
      (cons (last (car m))
            (second-diagonal3 (cdr (map strip-last m))))))

(assert-equal '(1 5 9) (diagonal '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal '(1) (diagonal '((1))))
(assert-equal '() (diagonal '()))
(assert-equal '(1 5 9) (diagonal2 '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal '(1) (diagonal2 '((1))))
(assert-equal '() (diagonal2 '()))
(assert-equal '(1 5 9) (diagonal3 '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal '(1) (diagonal3 '((1))))
(assert-equal '() (diagonal3 '()))
(assert-equal '(3 5 7) (second-diagonal '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal '(3 5 7) (second-diagonal2 '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal '(3 5 7) (second-diagonal3 '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal '(1 2) (strip-last '(1 2 3)))
(assert-equal '((1 2 3) (4 5 6)) (strip-last '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal '((1 2) (4 5) (7 8)) (map strip-last '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal 3 (last '(1 2 3)))
(assert-equal '(3 5 7) (second-diagonal2 '((1 2 3) (4 5 6) (7 8 9))))

; Този skip работи за r-тия ред в матрицата m. Всъщност, това е код който маха
; r-тия елемент в списъка m, а ползването му за матрици е частен случай.
(define (skip m r)
  (map (lambda (i) (list-ref m i))
       (filter (lambda (i) (not (= i r)))
               (range 0 (- (length m) 1)))))

; И вариант с рекурсия.
(define (skip2 m r)
  (define (helper i)
    (cond
      ((>= i (length m)) '())
      ((= i r) (helper (+ i 1)))
      (else (cons (list-ref m i)
                  (helper (+ i 1))))))
  (helper 0))


(assert-equal '((1 2 3) (7 8 9))
              (skip '((1 2 3) (4 5 6) (7 8 9)) 1))
(assert-equal '((1 2 3) (7 8 9))
              (skip2 '((1 2 3) (4 5 6) (7 8 9)) 1))

; Тук решението е подобно - махаме редове с helper-а, подобно на skip. Разликата
; е, че при добавянето на редовете викаме skip върху самите тях, за да махнем
; съответните елементи от c-тата колона.
(define (skip-row-column m r c)
  (map (lambda (row) (skip row c))
       (skip m r)))

(assert-equal '((1 3) (7 9))
              (skip-row-column '((1 2 3) (4 5 6) (7 8 9)) 1 1))