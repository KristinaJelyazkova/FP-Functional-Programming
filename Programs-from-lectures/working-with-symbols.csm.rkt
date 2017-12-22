;Работа със символи
;Проверка за принадлежност на елемент в списък
(define (memq? x L)
  (cond ((null? L) #f)
        ((eq? x (car L)) L)
        (else (memq? x (cdr L)))))

;дали символът item се съдържа в списъка L на произволно ниво
(define (deep-found? item L)
  (cond ((null? L) #f)
        ((atom? (car L))
             (if (eq? item (car L))
                 #t
                 (deep-found? item (cdr L))))
        (else (or (deep-found? item (car L))
                  (deep-found? item (cdr L))))))