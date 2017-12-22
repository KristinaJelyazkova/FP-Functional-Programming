(define (sumDigits x)
  (if (< x 10)
      x
      (+ (remainder x 10) (sumDigits (quotient x 10)))))

(define (count-digits n)
  (if (< x 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (sum-digits-iter x)
  (define (helper number result)
    (if (< number 10)
        number
        (helper (quotient number 10) (+ (remainder number 10) result))))
  (helper x 0))

(define (count-digits-iter n)
  (define (helper current result)
    (if (= current 0)
        result
        (helper (quotient current 10) (+ result 1))))
  (helper n 0))

(define (sum-iter start end)
  (define (helper current result)
    (if (>= current end)
        (+ result end)
        (helper (+ current 1) (+ current result))))
  (helper start 0))
