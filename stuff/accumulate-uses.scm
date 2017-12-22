(define (accumulate start next end term comb initial)
  (define (helper current result)
    (if (> current end)
        result
        (helper (next current)
                (comb result (term current)))))
  (helper start initial))

(define (sum-square start end)
  (accumulate start plus-1 end square + 0))

(define (plus-1 x)
  (+ x 1))

(define (square x)
  (* x x))

(define (id x)
  x)

(define (fact n)
  (accumulate 1 plus-1 n id * 1))

(define (pow x n)
  (define (ret-x _)
    x)
  (accumulate 1 plus-1 n ret-x * 1))

(define (variation k n)
  (/ (accumulate 1 plus-1 n id * 1)
     (accumulate 1 plus-1 (- n k) id * 1)));

(define (combination k n)
  (/ (accumulate 1 plus-1 n id * 1)
     (* (accumulate 1 plus-1 (- n k) id * 1)
        (accumulate 1 plus-1 k id * 1))))

(define (my-exp x)
  (define n 50)
  (define (term i)
    (/ (pow x i) (fact i)))
  (accumulate 0 plus-1 n term + 0))

(define (my-sin x)
  (define n 50)
  (define (f i)
    (+ (* 2 i) 1))
  (define (term i)
    (/ (* (expt -1 (remainder i 2))
          (pow x (f i)))))
  (accumulate 0 plus-1 n term + 0))

(define (my-sqrt x)
  (define n 10)
  (define (op res i)
    (/ (+ res (/ x res)) 2))
  (accumulate 1 plus-1 n id op 1))

(define (prime? n)
  (define (term i)
    (not (= (remainder n i) 0)))
  (define (andAnd x y)
    (and x y))
  (accumulate 2 plus-1 (quotient n 2) term andAnd 1))

(define (my-reverse n)
  (define (count-digits x)
    (define (helper current result)
    (if (<= current 0)
        result
        (helper (quotient current 10) (+ result 1))))
    (helper x 0))
  (define (op s t)
    (+ (* s 10) t))
  (define (term y)
    (remainder (quotient n (expt 10 y)) 10))
  (accumulate 0 plus-1 (- (count-digits n) 1) term op 0))