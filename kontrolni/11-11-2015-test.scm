(define (expr n)
  (define (helper current result)
    (if (< current 1)
        (/ 1 result)
        (helper (- current 1)
                (+ (- (* 2 current)
                      1)
                   (/ 1 result)))))
  (helper n (+ (* 2 n) 1)))


(define (row eps)
  (define (helper ai i)
    (if (< (abs ai) eps)
        ai
        (helper (* ai
                   (- 1
                      (/ 1
                         (+ i 1))))
                (+ i 1))))
  (helper 1/2 2))

(define (accumulate op null-val term a next b)
  (define (helper current result)
    (if (> current b)
        result
        (helper (next current)
                (op result (term current)))))
  (helper a null-val))

(define (filter-accumulate pred? op null-val term a next b)
  (define (helper current result)
    (if (> current b)
        result
        (if (pred? current)
            (helper (next current)
                (op result (term current)))
            (helper (next current)
                result))))
  (helper a null-val))
        
(define (g m n)
  (accumulate +
              0
              (lambda (i)
                (filter-accumulate (lambda (j)
                                     (= (remainder j i) 0))
                                   *
                                   1
                                   (lambda (x) x)
                                   1
                                   (lambda (x) (+ x 1))
                                   n))
              1
              (lambda (x) (+ x 1))
              m))

(define (h f g)
  (lambda (x)
    ((accumulate (lambda (f1 f2)
                  (lambda (y)
                    (f1 (f2 y))))
                (lambda (z) z)
                (lambda (_) g)
                1
                (lambda (x) (+ x 1))
                (f x))
    x)))
  
                  