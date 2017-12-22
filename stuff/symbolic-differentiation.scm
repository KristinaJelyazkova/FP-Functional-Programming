;Символно диференциране
(define (derive expr var)
  (cond ((constant? expr) 0)
        ((variable? expr)
         (if (same-variables? expr var)
             1
             0))
        ((sum? expr)
         (make-sum (derive (ad1 expr) var)
                   (derive (ad2 expr) var)))
        ((product? expr)
         (make-sum (make-product (mult1 expr)
                                 (derive (mult2 expr) var))
                   (make-product (derive (mult1 expr) var)
                                 (mult2 expr))))))

;предикати
(define (constant? expr)
  (number? expr))

(define (variable? expr)
  (symbol? expr))

(define (same-variables? var1 var2)
  (and (variable? var1) (variable? var2) (eq? var1 var2)))

(define (sum? expr)
  (if (pair? expr)
      (eq? '+ (car expr))
      #f))

(define (product? expr)
  (if (pair? expr)
      (eq? '* (car expr))
      #f))

;селектори
(define (ad1 sum)
  (cadr sum))

(define (ad2 sum)
  (caddr sum))

(define (mult1 product)
  (cadr product))

(define (mult2 product)
  (caddr product))

;конструктори
(define (make-sum a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((number? a1)
         (if (= a1 0)
             a2
             (list '+ a1 a2)))
        ((number? a2)
         (if (= a2 0)
             a1
             (list '+ a1 a2)))
        (else (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (* a1 a2))
        ((number? a1)
         (cond ((= a1 0) 0)
               ((= a1 1) a2)
               (else (list '* a1 a2))))
        ((number? a2)
         (cond ((= a2 0) 0)
               ((= a2 1) a1)
               (else (list '* a1 a2))))
        (else (list '* a1 a2))))