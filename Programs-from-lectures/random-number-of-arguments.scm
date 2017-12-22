;произволен брой аргументи (чиито оценки са числа) и връща като резултат произведението от оценките на аргументите си
(define mult1
  (lambda x (apply * x))) ; x не е в скоби => произв. бр. арг., които стават на списък от оценките на факт. пар.

(define (mult2 . x) ; еквивалентна дефиниция
  (apply * x))

;произволен брой аргументи и връща конюнкцията от оценките на аргументите си
(define (new-and . x)
  (define (helper as)
    (cond ((null? as) #t)
          ((not (car as)) #f)
          (else (helper (cdr as)))))
  (helper x))

;предефинира примитивната процедура append,конкатенираща проиволен брой списъци
  ;позволяваща 0-ла на брой аргументи
(define (my-append . L)
  (define (append2 L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (append2 (cdr L1) L2))))
  (define (helper List)
    (if (null? List)
        '()
        (append2 (car List) (helper (cdr List)))))
  (helper L))

  ;с поне 1 аргумент
 (define (my-append L1 . L)
  (define (append2 L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (append2 (cdr L1) L2))))
  (define (helper L1 L)
    (if (null? L)
        L1
        (append2 L1 (helper (car L) (cdr L)))))
  (helper L1 L))