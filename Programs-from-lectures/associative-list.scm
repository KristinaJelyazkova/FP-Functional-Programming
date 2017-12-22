;Реализация на асоциативен списък

;конструктор

(define (make-table)
  (list '*table*))

;селектори

;(assq key a-list) - намира първия елемент на [a-list], койтоима ключ, равен на [key] (в смисъл на eq?), или #f, ако
;такъв елемент не съществува.
(define (my-assq key a-list)
  (cond ((null? a-list) '())
        ((eq? key (caar a-list)) (car a-list))
        (else (my-assq key (cdr a-list)))))
;Процедурата assq има a-list за свой параметър, т.е. (cdr table), а не table.
;Ако в нея eq? се замени с eqv?, получената процедура се означава с assv, а ако пък eq? се
;замени с equal?, получената процедура се означава с assoc.

;(lookup key table) - намира асоциацията, свързана с ключ, идентичен с key в таблицата table. Ако key не е ключ в
;асоциативния списък на table, lookup връща nil (или '()).
(define (lookup key table)
  (let ((record (my-assq key (cdr table))))
    (if (null? record)
        '()
        (cdr record))))

;мутатори

;(insert! key value table) - включва двойката (key . value) в таблицата table.
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
                           
;(remassq! key table) - изключва двойка с ключ key от таблицата table. Предполагаме, че съществува двойка на
;table, която съдържа ключ, идентичен на key.
;(pred key table) - връща двойката, намираща се пред двойката, чийто car е двойка с ключ, идентичен на key.
(define (pred key table)
  (let ((record (my-assq key (cdr table))))
    (if (eq? (cadr table) record)
        table
        (pred key (cdr table)))))

(define (remassq! key table)
  (let ((record (pred key table)))
    (begin (set-cdr! record (cddr record))
           table)))
        