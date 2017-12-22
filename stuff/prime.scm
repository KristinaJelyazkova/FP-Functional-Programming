(define (primeRec y x)
  (if (> y (sqrt x))
      #t
      (if (= (remainder x y) 0)
          #f
          (primeRec (+ y 1) x))))

(define (prime? x)
  (if (< x 2)
      #f
      (primeRec 2 x)))


(define (prime-iter? n)
  (define (helper possible-divisor)
    (cond
      ((<= possible-divisor 1) #t)
      ((= (remainder n possible-divisor) 0) #f)
      (else (helper (- possible-divisor 1)))))
    (helper (- n 1)))