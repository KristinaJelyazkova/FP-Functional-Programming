(define (new-cons x y)
  (lambda (m) (m x y)))
(define (new-car z)
  (z (lambda (p q) p)))
(define (new-cdr z)
  (z (lambda (p q) q)))

;(define (new-cons x y)
 ; (define (dispatch m)
 ;   (cond ((= m 0) x)
 ;         ((= m 1) y)
  ;        (else (error "Argument not 0 or 1" m))))
; ; dispatch)
;(define (new-car z) (z 0))
;(define (new-cdr z) (z 1))

;програма за работа с рационални числа - пример за абстракция с данни

;дефиниране на процедура, която намира рационалното число r = 1/1 + 1/2 + 1/3 + ... + 1/n
(define (sum-rat n)
  (define (helper current result)
    (if (> current n)
        result
        (helper (+ current 1) (+rat result (make-rat 1 current)))))
  (helper 1 (make-rat 0 1)))

;същата процедура, но с accumulate
(define (accumulate start next end term op nullval)
  (define (helper current result)
    (if (> current end)
        result
        (helper (next current)
                (op result (term current)))))
  (helper start nullval))

(define (sum-rat-acc n)
  (accumulate 1
              (lambda (x) (+ x 1))
              n
              (lambda (x) (make-rat 1 x))
              +rat
              (make-rat 0 1)))

;дефиниране на процедура, която намира рационалното число r = 1/1! + 1/2! + 1/3! + ... + 1/n!
(define (sum-rat-fact n)
  (accumulate 1
              (lambda (x) (+ x 1))
              n
              (letrec ((fact
                         (lambda (x)
                           (if (< x 2)
                               1
                               (* x (fact (- x 1)))))))
               (lambda (x) (make-rat 1 (fact x))))
              +rat
              (make-rat 0 1)))
;---------------------------------------------------------------------------

; дефиниране на процедури за работа с рационални числа
(define (+rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (/rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (=rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))
;-------------------------------------

;дефиниране на процедури, представящи рационалните числа

;конструктор
(define (make-rat x y)
  ;(cons x y)) ; вграден конструктор - създава точковата двойка ([x] . [y]) - [x] e оценката на x
  ; със съкращаване:
  (if (= x 0)
      (cons 0 1)
      (let ((g (gcd (abs x) (abs y))))
        (if (or (and (< x 0) (< y 0))
                (and (> x 0) (> y 0)))
            (cons (/ (abs x) g) (/ (abs y) g))
            (cons (- (/ (abs x) g)) (/ (abs y) g))))))
        

;селектори
(define (numer x) ; връща числителя на рационалното число
  (car x)) ; вграден селектор - връща първия елемент на точковата двойка

(define (denom x) ; връща знаменателя на рационалното число
  (cdr x)) ; вграден селектор - връща втория елемент на точковата двойка
