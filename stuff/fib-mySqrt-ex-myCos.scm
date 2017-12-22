(define (fib n)
  (define (helper a b i)
    (if (> i n)
        b
        (helper (+ a b) a (+ i 1))))
  (helper 1 0 0))

(define (my-sqrt x)
  (define (helper t)
    (if (< (abs (- (* t t) x)) 0.001)
        t
        (helper (/ (+ t 
                      (/ x t)) 
                   2))))
  (helper 1))

(define (ex x)
  (define n 50)
  (define (helper i res stepenix fact)
    (if (> i n)
        res
        (helper (+ i 1) (+ res (/ stepenix fact)) (* stepenix x) (* fact (+ i 1)))))
  (helper 0 0 1 1))

(define (my-cos x)
  (define n 50)
  (define (helper i res stepenix fact sgn)
    (if (> i n)
        res
        (helper (+ i 1) (+ res (* (/ stepenix fact) sgn)) (* stepenix (* x x)) (* fact (+ i 1) (+ i 2)) (* sgn -1))))
  (helper 0 0 1 1 1))