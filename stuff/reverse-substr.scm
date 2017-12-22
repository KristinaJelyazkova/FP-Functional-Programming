;(define (my-reverse n)
 ; (define (helper n stepeni10 digits)
 ; (display n)
 ; (display " ")
 ; (display ( * (last-digit n) (expt 10 digits)))
 ; (display " ")
 ; (display (* (first-digit n) stepeni10))
 ; (newline)
 ;   (if (< n 10)
 ;       n
  ;      (+ (* (last-digit n) (expt 10 digits))
 ;          (* (first-digit n) stepeni10)
  ;         (helper (remove-first-and-last-digit n) (* stepeni10 10) (- digits 1)))))
  ;(helper n 1 (- (count-digits n) 1)))
  
  
(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))
  
  
(define (first-digit n)
  (quotient n (expt 10 (- (count-digits n) 1))))
  
(define (last-digit n)
  (remainder n 10))

(define (remove-first-and-last-digit n)
  (quotient (remainder n (expt 10 (- (count-digits n) 1))) 10))

(define (reverse-iter n)
  (define (helper n result)
    (if (= n 0)
        result
        (helper (quotient n 10)
                (+ (* result 10)
                   (remainder n 10)))))
  (helper n 0))


(define (count-digits n)
  (define (helper x result)
    (if (<= x 0)
        result
        (helper (quotient x 10) (+ result 1))))
  (helper n 0))


(define (subStringEnd? a b)
  (if (< a b)
      (subStringEnd? b a)
      (= b
         (remainder a (expt 10 (count-digits b))))))

(define (substr? n a)
  (cond ((< n a) #f)
        ((subStringEnd? n a) #t)
        (else (substr? (quotient n 10) a))))
