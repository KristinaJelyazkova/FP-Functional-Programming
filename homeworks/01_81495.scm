(define (f-to-c F)
  (* (/ 5 9)(- F 32)))

(define (help a b c)
  (if (and (< a 3) (>= b 3) (>= c 3))
       #t
       #f))

(define (not-a-grade x)
  (if (or (< x 2) (> x 6))
      #t
      #f))

(define (three-grades a b c)
  (if (or (not-a-grade a) (not-a-grade b) (not-a-grade c))
      #f
      (if (or (help a b c) (help b a c) (help c a b))
          #t
          #f)))