;`() - празен списък
;(car <list>) -> <element> - връща „главата“ на списък
;(cdr <list>) -> <list> - връща „опашката“ на списък
;(cons <element> <list>) -> <list>
;(append <list> <list>) -> <list> - „залепва“ два списъка
;(list <element> <element> ... <element>) -> <list> - създава списък от зададени елементи (може и с (<element> <element> ... <element>), ако елементите са атоми)
;(null? <list>) -> boolean - проверява дали нещо е празният списък
;(list? <list>) -> boolean - проверява дали нещо е списък
;(list-ref <list> <i>) - връща i-тия елемент от списък


; Проверява дали x е част от списъка l. Подобно на вградената процедура member.
(define (member? x l)
  (cond
    ((null? l) #f)
    ((= (car l) x) #t)
    (else (member? x (cdr l)))))

(assert-true (member? 1 '(1 2 3)))
(assert-true (member? 2 '(1 2 3)))
(assert-true (member? 3 '(1 2 3)))
(assert-false (member? 4 '(1 2 3)))

; Връща дължината на списък. Еквивалентно на вградената процедура length.
(define (my-length l)
  (define (helper current-l length)
    (if (null? current-l)
        length
        (helper (cdr current-l)
                (+ length 1))))
  (helper l 0))

(assert= 0 (my-length '()))
(assert= 1 (my-length '(1)))
(assert= 3 (my-length '(1 2 3)))
(assert= 4 (my-length '(1 2 7 8)))

; Да се върне n-тият елемент от списъка l. Това е еквивалентно на вградената
; процедура list-ref.
(define (nth l n)
  (define (nth-helper current-l i)
    (if (= i n)
        (car current-l)
        (nth-helper (cdr current-l)
                    (+ i 1))))
  (nth-helper l 0))

(assert= 1 (nth '(1 2 3) 0))
(assert= 3 (nth '(1 2 3) 2))

; Реализация на append.
(define (my-append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (my-append (cdr l1) l2))))

(assert-equal '(1 2 3 4 5 6) (my-append '(1 2 3) '(4 5 6)))
(assert-equal '(1 2 3) (my-append '(1 2 3) '()))
(assert-equal '(4 5 6) (my-append '() '(4 5 6)))
(assert-equal '() (my-append '() '()))

; Да се конструира списъкът с числата от start до end.
(define (range start end)
  (define (helper i result)
    (if (< i start)
        result
        (helper (- i 1) (cons i result))))
  (helper end '()))

(assert-equal '(1) (range 1 1))
(assert-equal '(1 2 3 4) (range 1 4))

; Целта на задачата е да се обърне списъкът l. За целта постепенно махаме
; елементите ОТПРЕД на списъка l и ги слагаме ОТПРЕД на резултата.
; Еквивалентно на вградената процедура reverse.
(define (my-reverse l)
  (define (helper current-l result)
    (if (null? current-l)
        result
        (helper (cdr current-l)
                (cons (car current-l) result))))
  (helper l '()))

; Обръща списъка два пъти. Естествено, крайният резултат е същият списък. Давам
; задачата за да можете да видите какво трябва да се промени за да може
; операцията да се случи в другата посока. В случая просто на всяка стъпка
; залепяме ОТЗАД на резултата елемените, взети ОТПРЕД на l.
; Обърнете внимание, че тук това е много по-бавна операция, поради линейната
; сложност на append.
(define (my-reverse-reverse l)
  (define (helper current-l result)
    (if (null? current-l)
        result
        (helper (cdr current-l)
                (append result (list (car current-l))))))
  (helper l '()))

(assert-equal '(3 2 1) (my-reverse '(1 2 3)))
(assert-equal '(1 2 3) (my-reverse-reverse '(1 2 3)))
(assert-equal '() (my-reverse '()))
(assert-equal '() (my-reverse-reverse '()))

; Обединение на двата списъка l1 и l2 (приемаме, че няма повтарящи се елементи).
(define (union l1 l2)
  (if (null? l1)
      l2
      (let ((l1-head (car l1)))
        (if (member? l1-head l2)
            (union (cdr l1) l2)
            (cons l1-head (union (cdr l1) l2))))))

(assert-equal '(5 4 1 3 2) (union '(2 3 5) '(4 1 3 2)))
(assert-equal '(1 2 3) (union '(1 2 3) '(1 2 3)))
(assert-equal '(2) (union '(2) '(2)))
(assert-equal '() (union '() '()))

; Сечение на двата списъка (приемаме, че няма повтарящи се елементи).
(define (intersection l1 l2)
  (if (null? l1)
      '()
      (let ((l1-head (car l1)))
        (if (member? l1-head l2)
            (cons l1-head (intersection (cdr l1) l2))
            (intersection (cdr l1) l2)))))

(assert-equal '(2 3) (intersection '(2 3 5) '(4 1 3 2)))
(assert-equal '(1 2 3) (intersection '(1 2 3) '(1 2 3)))
(assert-equal '() (intersection '(1 2 3) '(4 5 6)))
(assert-equal '(1) (intersection '(1) '(1)))

; Брои колко пъти се срещат всички елементи от списъка l2 (подредени по същия
; начин) в списъка l1.
(define (count-occurences l1 l2)
  (define (l1-starts-with-l2? current-l1 current-l2)
    (cond
      ((null? current-l2) #t)
      ((null? current-l1) #f)
      ((not (= (car current-l1) (car current-l2))) #f)
      (else (l1-starts-with-l2? (cdr current-l1) (cdr current-l2)))))
  (cond
    ((null? l1) 0)
    ((l1-starts-with-l2? l1 l2) (+ 1 (count-occurences (cdr l1) l2)))
    (else (count-occurences (cdr l1) l2))))

(assert= 2 (count-occurences '(1 2 3 4 1 5 1 2 7) '(1 2)))
(assert= 1 (count-occurences '(1 2 3 4 1 5 1 2 7) '(1 2 3)))
(assert= 1 (count-occurences '(1 2 3 4 1 5 1 2 7) '(7)))
(assert= 0 (count-occurences '(1 2 3 4 1 5 1 2 7) '(1 3)))
(assert= 0 (count-occurences '(1 2 3 4 1 5 1 2 7) '(3 7)))
(assert= 1 (count-occurences '(1 2) '(1 2)))
(assert= 1 (count-occurences '(1 2) '(1)))

; Създава вложени списъци чрез generate-list (вижте примера).
(define (construct-list n)
  (if (= n 0)
    '()
    (cons (reverse (range 1 n))
          (construct-list (- n 1)))))

(assert-equal '() (construct-list 0))
(assert-equal '((1)) (construct-list 1))
(assert-equal '((5 4 3 2 1) (4 3 2 1) (3 2 1) (2 1) (1)) (construct-list 5))
