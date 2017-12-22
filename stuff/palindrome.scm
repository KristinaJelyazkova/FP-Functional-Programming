(define (count-digits-1 n)
  (if (< n 10)
      0
      (+ 1 (count-digits-1 (quotient n 10)))))

(define (last-digit n)
  (remainder n 10))

(define (first-digit n)
  (quotient n (expt 10 (count-digits-1 n))))


(define (remove-first-and-last-digit n)
  (quotient (remainder n
                       (expt 10
                             (count-digits-1 n)))
            10))

(define (palindrome? n)
  (if (< n 10)
      #t
      (if (not (= (last-digit n)
                  (first-digit n)))
          #f
          (palindrome? (remove-first-and-last-digit n)))))

