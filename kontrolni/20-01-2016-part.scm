(define (list-of-divisors n)
  (define (helper current result)
    (if (= current 0)
        result
        (if (= (remainder n current) 0)
            (helper (- current 1)
                (cons current result))
            (helper (- current 1)
                result))))
  (helper (- n 1) '()))

(define (perfect? n)
  (= n
     (apply + (list-of-divisors n))))