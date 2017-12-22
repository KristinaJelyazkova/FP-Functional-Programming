;--------------------------------------------------------------------------------------------------------------------

;Процедури от по-висок ред за работа със списъци

;Събиране на елементите на даден списък от числа
(define (sum-list L)
  (if (null? L)
      0
      (+ (car L)
         (sum-list (cdr L)))))

;Умножаване на елементите на даден списък от числа
(define (product-list L)
  (if (null? L)
      1
      (* (car L)
         (sum-list (cdr L)))))

;Акумулиране на елементите на даден списък → accumulate
(define (accumulate op null-val L)
  (if (null? L)
      null-val
      (op (car L)
          (accumulate op null-val (cdr L)))))

;итеративен вариант на accumulate
(define (accumulate-iter op null-val L)
  (define (helper currentList result)
    (if (null? currentList)
        result
        (helper (cdr currentList) (op (car currentList) result))))
  (helper L null-val))

;намира сумата от елементите на [L] (работи като sum-list)
(define (sum-list-acc L)
  (accumulate-iter + 0 L))

;намира произведението на елементитена [L] (работи като product-list)
(define (product-list-acc L)
  (accumulate-iter * 1 L))

;копира елементите на [L] на другомясто в паметта
(define (copy-list L)
  (accumulate cons '() L))

;конкатенира елементите на [L]
(define (concat L)
  (accumulate append '() L))

;копира в ОБРАТЕН РЕД елементите на [L] на друго място в паметта;
(define (copy-reverse L)
  (accumulate-iter cons '() L))

;конкатенира в ОБРАТЕН РЕД елементите на [L];
(define (concat-reverse L)
  (accumulate-iter append '() L))

;Преобразуване на списък чрез прилагане на една и съща процедура към всеки от елементите на списъка map (вграденa e)
(define (my-map f L)
  (if (null? L)
      '()
      (cons (f (car L)) (my-map f (cdr L)))))

;връща списък, съдържащ само онези елементи на L, които са прости числа
(define (prime-list L)
  (cond ((null? L) '())
        ((prime? (car L))
         (cons (car L) (prime-list (cdr L))))
        (else (prime-list (cdr L)))))

;връща списък, съдържащ онези елементи на L, които са числа на Фибоначи
(define (fib? x)
  (define (helper current previous)
    (cond ((> current x) #f)
          ((= current x) #t)
          (else (helper (+ current previous) current))))
  (helper 1 1))

           
(define (fib-list L)
  (cond ((null? L) '())
        ((fib? (car L))
         (cons (car L) (fib-list (cdr L))))
        (else (fib-list (cdr L)))))

;Филтриране елементите на списък
(define (filter pred? L)
  (cond ((null? L) '())
        ((pred? (car L))
         (cons (car L)
               (filter pred? (cdr L))))
        (else (filter pred? (cdr L)))))

;Реализация на процедурата filter чрез apply
(define (filter-apply pred? L)
  (apply append
         (map (lambda (x)
                (if (pred? x)
                    (list x)
                    '() ))
              L) ))

;Намиране броя на елементите на даден списък, които удовлетворяват даден предикат.
(define (count pred? L)
  (apply +
         (map (lambda (x)
                (if (pred? x)
                    1
                    0))
              L) ))