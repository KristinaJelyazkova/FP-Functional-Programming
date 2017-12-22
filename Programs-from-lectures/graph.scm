;Приложение на едномерните таблици за реализиране на граф

;конструктор
(define (make-graph)
  (list 'graph))

;предикати
;(vertex? a g) - проверява дали а е връх на графа g
(define (vertex? a g)
  (let ((L (assq a (cdr g))))
    (not (null? L))))

;(edge? a b g) - проверява дали има ребро от върха а до върха b на графа g
(define (edge? a b g)
  (let ((L (assq a (cdr g))))
    (list? (member b (cdr L)))))

;мутатори

;(insert-edge! a b g) - включва ориентирано ребро от върха а до върха b на графа g
(define (insert-edge! a b g)
  (let ((L (assq a (cdr g))))
    (begin (set-cdr! L
              (cons b (cdr L)))
           g)))

;(del-edge! a b g) - изключва ориентирано ребро от върха а до върха b на графа g
(define (del-edge! a b g)
  (let ((L (assq a (cdr g))))
    (begin (delete! b L)
           g)))

(define (delete! b L)
  (let ((prev (previous b L)))
    (begin (set-cdr! prev (cddr prev))
           L)))

(define (previous b L)
  (if (eq? b (cadr L))
      L
      (previous b (cdr L))))

;(insert-vertex! a g) - включва върха а в графа g
(define (insert-vertex! a g)
  (insert! a '() g))

;(insert! key value table) - включва двойката (key . value) в таблицата table.
(define (my-assq key a-list)
  (cond ((null? a-list) '())
        ((eq? key (caar a-list)) (car a-list))
        (else (my-assq key (cdr a-list)))))
(define (insert! key value table)
  (let ((record (my-assq key (cdr table))))
    (if (null? record)
        (let ((newElem (cons key value)))
          (begin (set-cdr! table
                           (cons newElem (cdr table)))
                 table))
        (begin (set-cdr! record
                         value)
                         ;(cons value (cdr record)))
               table))))

;(del-vertex! a g) - изключва върха а на графа g
(define (del-vertex! a g)
  ; изтрива всички ребра от върхове на g към върха b
  (define (del-vertex-help! b f)
    (cond ((null? f) '())
          ((edge? (caar f) b g)
           (begin (del-edge! (caar f) b g)
                  (del-vertex-help! b (cdr f))))
           (else (del-vertex-help! b (cdr f)))))
  (del-vertex-help! a (cdr g))
  ; изтрива върха a и всички ребра изходящи от върха a
  (remassq! a g)
  g)

;Да се дефинира процедура, която проверява дали има път от връх a до връх b на графа g.
(define solution #f)

(define (way1? x b g) ; x е списък от върхове на графа g
  (cond ((null? x) #f)
        ((way? (car x) b g) #t)
        (else (way1? (cdr x) b g))))

(define (way? a b g)
  (let* ((L (assq a (cdr g)))
        (x (cdr L)))
    (cond ((null? x) #f)
          ((edge? a b g)
           (begin (set! solution #t)
                  solution))
          (else (begin (set-cdr! L '())
                       (set! solution (way1? x b g))
                       (set-cdr! L x)
                       solution)))))
                
                          
           
           
        
              