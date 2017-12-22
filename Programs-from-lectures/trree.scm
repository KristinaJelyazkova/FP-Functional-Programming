; програма за работа с двоични дървета

; конструктор
(define (make-tree root lefttree righttree)
  (list root lefttree righttree))

; селектори
(define (entry tree)
  (car tree))

(define (lefttree tree)
  (cadr tree))

(define (righttree tree)
  (caddr tree))

; предикати
(define (empty-tree? tree)
  (null? tree))

(define (leaf? tree)
  (and (empty-tree? (lefttree tree))
       (empty-tree? (righttree tree))))

;--------------------------------------------------------------------------------------------------------------------

;Проверка дали дадена стойност x се съдържа във върховете на двоичното дърво t
(define (member-tree? x tree)
  (cond ((empty-tree? tree) #f)
        ((= x (entry tree)) #t)
        (else (or (member-tree? x (lefttree tree))
                  (member-tree? x (rightttree tree))))))

;Намиране на дълбочината на двоичното дърво t
(define (depth tree)
  (if (empty-tree? tree)
      0
      (+ 1
         (max (depth (lefttree tree))
              (depth (righttree tree))))))

;Проверка дали дадена стойност x се съдържа в листата на двоичното дърво t
(define (member-leaf? x tree)
  (cond ((empty-tree? tree) #f)
        ((leaf? tree) (= x (entry tree)))
        (else (or (member-leaf? x (lefttree tree))
                  (member-leaf? x (righttree tree))))))

;Проверка дали има път от върха a до върха b в двоичното дърво t - считаме, че b e наследник на a
(define (path? a b tree)
  (cond ((empty-tree? tree) #f)
        ((= a (entry tree))
         (or (member? b (lefttree tree))
             (member? b (lefttree tree))))
        (else (or (path? a b (lefttree tree))
                  (path? a b (righttree tree))))))


