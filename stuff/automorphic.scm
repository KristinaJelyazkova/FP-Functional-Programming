(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (automorphic? n)
  (if (= (remainder (* n n) (expt 10 (count-digits n))) n)
      #t
      #f))