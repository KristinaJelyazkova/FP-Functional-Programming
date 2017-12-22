; Връща n-тата колона в матрицата m. От миналия път.
(define (get-nth-column m n)
  (map (lambda (row) (list-ref row n)) m))

; Идеята е на всяка стъпка да вземем i-тата колона на матрицата m и да я добавим като i-ти ред в
; резултата.
(define (transpose m)
  (map (lambda (i) (get-nth-column m i))
       (range 0 (- (length m) 1))))

; Като горното, но рекурсивно.
(define (transpose2 m)
  (define (helper i)
    (if (= i (length m))
        '()
        (cons (get-nth-column m i)
              (helper (+ i 1)))))
  (helper 0))

; Вариант за любознателни :-) Ползва алтернативни форми на map и apply.
(define (transpose3 m)
  (apply map list m))

(assert-equal '((1 4 7) (2 5 8) (3 6 9)) (transpose '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal '((1 4 7) (2 5 8) (3 6 9)) (transpose2 '((1 2 3) (4 5 6) (7 8 9))))
(assert-equal '((1 4 7) (2 5 8) (3 6 9)) (transpose3 '((1 2 3) (4 5 6) (7 8 9))))

; Декартово произведение с процедури от по-висок ред. За всеки елемент от l1 обхожда целия l2 и
; образува двойки от съответните елементи.
; apply append се ползва за да премахне ниво на влагане в списъците.
(define (cartesian-product l1 l2)
  (apply append (map (lambda (x1) (map (lambda (x2) (list x1 x2))
                                       l2))
                     l1)))

; Рекурсивен вариант на декартовото произведение.
; Има помощна процедура за „умножение“ на елемент от единия списък с другия
; списък.
(define (cartesian-product2 l1 l2)
  (define (mul-l2 x l2)
    (if (null? l2)
        '()
        (cons (list x (car l2))
              (mul-l2 x (cdr l2)))))
  (if (null? l1)
      '()
      (append (mul-l2 (car l1) l2)
              (cartesian-product (cdr l1) l2))))

(assert-equal '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))
              (cartesian-product '(1 2 3) '(4 5 6)))
(assert-equal '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))
              (cartesian-product2 '(1 2 3) '(4 5 6)))

; flatten взима списък от вложени (може повече от един път) списъци и го
; „изглажда“ - връща всички елементи по вложените списъци, подредени в един
; списък без влагане (вижте примера).
; За целта на всяка стъпка разгеждаме какво има най-отпред на списъка и
; преценяваме какво да правим с него според това дали е списък или атом. Ако
; тази задача ви притеснява, разгледайте member-deep.
(define (flatten l)
  (cond
    ((null? l) l)
    ((list? (car l)) (append
                       (flatten (car l))
                       (flatten (cdr l))))
    (else (cons (car l)
                (flatten (cdr l))))))

(assert-equal '(1 2 3 4 5 6) (flatten '((() 1) 2 (3 4 5 ()) ((6)) ())))

; Добавя (k v) в асоциативния списък a, ползвайки set-cdr!.
(define (insert! k v a)
  (let ((existing (assoc k a)))
    (if existing
      (set-cdr! existing
        (cons v (cdr existing)))
      (set-cdr! a
        (cons (list k v) (cdr a))))))

; Създава граф по списък от върхове. Налага се да вземаме списъка от върхове
; тъй като не можем да създадем истински празен граф - ако той е празен списък,
; деструктивните операции няма да могат да го „напълнят“ със съдържание, тъй
; като insert! (респективно set-cdr!) няма как да добавя в празен списък.
(define (create-graph vertices)
  (map (lambda (v) (list v))
       vertices))

(define (add-edge! a b g)
  (let ((start-vertex (assoc a g)))
    (set-cdr! start-vertex
              (cons b (cdr start-vertex)))))

(define (add-vertex! v g)
  (set-cdr! g
            (cons (list v) (cdr g))))

(define (vertices g) (map car g))

(define (neighbours v g) (cdr (assoc v g)))

(define g1 (create-graph '(5 7)))
(add-vertex! 1 g1)
(add-vertex! 2 g1)
(add-vertex! 4 g1)
(assert-equal '((5) (4) (2) (1) (7)) g1)
(assert-equal '(5 4 2 1 7) (vertices g1))
(assert-equal '() (neighbours 2 g1))

(add-edge! 1 2 g1)
(add-edge! 1 4 g1)
(add-edge! 5 2 g1)
(assert-equal '((5 2) (4) (2) (1 4 2) (7)) g1)

(define g2 '((4 1 2) (5 1 2 3)))
(assert-equal '(1 2 3) (neighbours 5 g2))

; Полу-степен на изхода. Интересува ни само колко съседи има върхът.
(define (degree-out v g)
  (length (neighbours v g)))

; За полу-степен на входа трябва да намерим всички върхове, които имат дадения
; връх за съсед. След това връщаме дължината им.
(define (degree-in v g)
  (length (filter
            (lambda (vertex)
              (member v
                      (neighbours vertex g)))
            (vertices g))))

(define (degree v g)
  (+ (degree-out v g)
     (degree-in v g)))

(define g1 (create-graph '(1 2 3)))
(add-edge! 1 2 g1)
(add-edge! 1 3 g1)
(add-edge! 2 3 g1)
(add-edge! 2 1 g1)
(add-edge! 3 1 g1)

(assert= 2 (degree-out 1 g1))
(assert= 2 (degree-out 2 g1))
(assert= 1 (degree-out 3 g1))
(assert= 2 (degree-in 1 g1))
(assert= 1 (degree-in 2 g1))
(assert= 2 (degree-in 3 g1))
(assert= 4 (degree 1 g1))
(assert= 3 (degree 2 g1))
(assert= 3 (degree 3 g1))

; Връща списък от всички ребра в графа.
; Идеята на вземането на всички ребра:
; 1. Намираме всички върхове в графа.
; 2. За всеки от връх намираме всичките му съседи.
; 3. За всеки съсед от стъпка 2 създаваме двойката (връх съсед) и (с map) я
;    правим част от списък от двойки който има вида ((връх-i съсед-1)
;    (връх-i съсед-2) ...). Резултатът от тази операция ще са всичките ребра,
;    които имат за начало избрания на стъпка 2 връх.
; 4. Залепяме списъците, получени на стъпка 3.
(define (edges g)
  ; С apply append „залепяме“ списъците с ребра за всеки връх. Тоест, превръщаме
  ; списък от вида (за върховете 1, 2, 3):
  ; (((1 2) (1 3)) ((2 1) (2 4)) ((3 5)))
  ; В списък от вида:
  ; ((1 2) (1 3) (2 1) (2 4) (3 5))
  (apply
    append
    (map (lambda (vertex)
           ; Създаваме списък от всички ребра с начало vertex.
           (map (lambda (neighbour)
                  ; Добавяме ребро към резултата.
                  (list vertex neighbour))
                (neighbours vertex g)))
         (vertices g))))

; Може и с декартовото произведение :)
(define (edges2 g)
  (apply
    append
    (map (lambda (vertex)
           (cartesian-product (list vertex) (neighbours vertex g)))
         (vertices g))))

(define g1 (create-graph '(5 7)))
(add-vertex! 1 g1)
(add-vertex! 2 g1)
(add-vertex! 4 g1)
(add-vertex! 7 g1)
(add-edge! 1 2 g1)
(add-edge! 1 4 g1)
(add-edge! 5 2 g1)
(assert-equal '((5 2) (1 4) (1 2)) (edges g1))
(assert-equal '((5 2) (1 4) (1 2)) (edges2 g1))

; За да проверим дали е симетричен обхождаме всички ребра и проверяваме за всяко
; ребро (a b) дали съществува съответно ребро (b a). Ако не съществува -
; премахваме го с filter. Накрая проверяваме дали филтрирания списък е със
; същата дължина като списъка на ребрата - т.е. дали има отпаднали ребра, които
; не са симетрични.
(define (symmetric? g)
  (let ((g-edges (edges g)))
    (= (length g-edges)
       (length (filter (lambda (edge)
                         (member (list (cadr edge)
                                       (car edge))
                                 g-edges))
                       g-edges)))))

(define g1 (create-graph '(1 2 3 4)))
(add-edge! 1 2 g1)
(add-edge! 1 3 g1)
(add-edge! 3 1 g1)
(add-edge! 2 1 g1)

(assert-true (symmetric? g1))
(add-edge! 3 4 g1)
(assert-false (symmetric? g1))
(add-edge! 4 3 g1)
(assert-true (symmetric? g1))

; Създаваме празен граф със същите върхове като графа g. След това, обхождаме
; ребрата на g и за всяко ребро (a b) добавяме реброто (b a) в резултата.
(define (invert g)
  (let ((result (create-graph (vertices g))))
    (map
      (lambda (edge)
        (add-edge! (cadr edge) (car edge) result))
      (edges g))
    result))

(define g1 (create-graph '(1 2 3 4)))
(add-edge! 1 2 g1)
(add-edge! 2 3 g1)
(add-edge! 4 3 g1)

(define g2 (invert g1))
(assert-equal '((1) (2 1) (3 4 2) (4)) g2)