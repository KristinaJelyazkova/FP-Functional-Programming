(define (bin-to-dec n)
  (define (helper n stepeni2)
    ;(display n)
    ;(display " ")
    ;(display stepeni2)
    ;(display " ")
    ;(display (* (remainder n 10) stepeni2))
    ;(newline)
    (if (<= n 0)
        0
        (+ (* (remainder n 10) stepeni2)
           (helper (quotient n 10) (* stepeni2 2)))))
  (helper n 1))

(define (dec-to-bin n)
    (define (helper n stepeni10)
      (if (= n 0)
          0
          (+ (* (remainder n 2) stepeni10)
             (helper (quotient n 2) (* stepeni10 10)))))
  (helper n 1))


(define (bin-to-dec-iter n)
  (define (helper n stepeni2 result)
    (if (<= n 0)
        result
        (helper (quotient n 10)
                (* stepeni2 2)
                (+ result (* (remainder n 10) stepeni2)))))
  (helper n 1 0))

(define (dec-to-bin-iter n)
  (define (helper n stepeni10 result)
    (if (<= n 0)
        result
        (helper (quotient n 2)
                (* stepeni10 10)
                (+ result (* (remainder n 2) stepeni10)))))
  (helper n 1 0))
