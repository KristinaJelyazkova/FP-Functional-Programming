; програма за работа със списъци

; представяне на списъци
; конструктори - cons и list са вградени - (list 1 2 3) -> (1 2 3)
; селектори - car връща първия елемент на списъка, cdr връща списъка без първия си елемент

; основни операции над списъци

; дефинираме процедура, която връща n - тия елемент на списъка
(define (nth n L)
  (if (= n 1)
      (car L)
      (nth (- n 1) (cdr L))))

; length - има си я вградена
(define (my-length L)
  (if (null? L)
      0
      (+ 1 (my-length (cdr L)))))

; сега итеративно
(define (my-length-iter L)
  (define (helper current result)
    (if (null? current)
        result
        (helper (cdr current)(+ 1 result))))
  (helper L 0))

; append - вградена - съединява произволен брой списъци
; ние ще си дефинираме такава, която съединява 2 списъка
(define (append2 L1 L2)
  (if (null? L1)
      L2
      (cons (car L1) (append2 (cdr L1) L2))))

; reverse - вградена процедура
; и ние ще си направим
(define (my-reverse L1)
  (if (null? L1)
      L1
      (append2 (my-reverse (cdr L1)) (list (car L1)))))

; итеративен вариант
(define (rev-iter L)
  (define (helper current result)
    (if (null? current)
        result
        (helper (cdr current) (cons (car current) result))))
  (helper L '()))


;------------------------------------------------------------
; работа със списъци, които съдържат списъци

; дефинираме процедура, която връща броя атоми в списъка като брои и празния списък
(define (atom? x)
  (not (pair? x)))

(define (count-atoms L)
  (cond ((null? L) 0)
        ((atom? (car L)) (+ 1 (count-atoms (cdr L))))
        (else (+ (count-atoms (car L)) (count-atoms (cdr L))))))

; дефинираме процедура, която връща броя атоми в списъка като НЕ брои празния списък
(define (count-atoms2 L)
  (cond ((null? L) 0)
        ((atom? L) 1)
        (else (+ (count-atoms2 (car L)) (count-atoms2 (cdr L))))))

;oбръщане реда на елементите на даден списък на всички нива на влагане
(define (deep-reverse L)
  (cond ((null? L) '())
        ((atom? L) L)
        (else (append2 (deep-reverse (cdr L))
                       (list (deep-reverse (car L)))))))

(define (deep-reverse2 L)
  (if  (atom? L)
          L
          (append (deep-reverse2 (cdr L))
                       (list (deep-reverse2 (car L))))))
        
;--------------------------------------------------------------------
; работа със списъци като множества от елементи (числа)
; дали число е елемент на множество (число - заради равното)
(define (member? x A)
  (cond ((null? A) #f)
        ((= x (car A)) #t)
        (else (member? x (cdr A)))))

; сеченеието на 2 множества
(define (intersection A B)
  (cond ((or (null? B)(null? A)) '())
        ((member? (car A) B)
         (cons (car A) (intersection (cdr A) B)))
        (else (intersection (cdr A) B))))

; разлика на 2 множества А\B
(define (diff A B)
  (cond ((null? A) '())
        ((null? B) A)
        ((member? (car A) B) (diff (cdr A) B))
        (else (cons (car A) (diff (cdr A) B)))))

; обединение на 2 множества А и B
(define (union A B)
  (append (diff A B) B))
; друг вариант
(define (union1 A B)
  (cond ((null? A) B)
        ((null? B) A)
        ((member? (car A) B) (union1 (cdr A) B))
        (else (cons (car A) (union1 (cdr A) B)))))

; изтриване на първо срещане на елемент от списък от числа
(define (del x L)
  (cond ((null? L) L)
        ((= x (car L)) (cdr L))
        (else (cons (car L) (del x (cdr L))))))

; изтриване на всички срещания на елемент от списък
(define (del-all x L)
  (cond ((null? L) '())
        ((= x (car L)) (del-all x (cdr L)))
        (else (cons (car L) (del-all x (cdr L))))))

; Намиране на последния елемент на непразен списък
(define (last L)
  (if (null? (cdr L))
      (car L)
      (last (cdr L))))

; Изтриване на последния елемент на непразен списък
(define (del-last L)
  (if (null? (cdr L))
      '()
      (cons (car L) (del-last (cdr L)))))

; друг интерсен вариант чрез reverse
(define (del-last1 L)
  (reverse (cdr (reverse L))))

; намиране на минимален елемент на непразен списък
(define (min-el L)
  (cond ((null? (cdr L)) (car L))
        ((< (car L) (min-el (cdr L))) (car L))
        (else (min-el (cdr L)))))
; друг вариант
(define (min-el1 L)
  (if (null? (cdr L))
       (car L)
       (min (car L) (min-el1 (cdr L)))))

;Сортиране на списък от числа по метода на пряката селекция
(define (sort1 L)
  (if (null? L)
      '()
      (let ((m (min-el L)))
        (cons m (sort1 (del m L))))))

; има вградена функция за сортиране sort
;(sort '(3 2 1 5 5 3 2 1) <)
;(sort '("Ivan" "Katia" "Anna" "Bilyana") string<?) - лексигографски
