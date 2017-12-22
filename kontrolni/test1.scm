(define (accumulate start next end term comb initial)
  (define (helper current result)
    (if (> current end)
        result
        (helper (next current)
                (comb result (term current)))))
  (helper start initial))

(define (left n m)
  (accumulate 1
              (lambda (x) (+ x 1))
              n
              (lambda (x)
                (accumulate x
                            (lambda (y) (+ y 1))
                            (+ x m)
                            (lambda (y) y)
                            *
                            1))
              +
              0))

(define (right n m)
  (* (/ 1
        (+ m 2))
     (accumulate n
                 (lambda (x) (+ x 1))
                 (+ m n 1)
                 (lambda (y) y)
                 *
                 1)))

(define (list-of-divisors n)
  (define (helper i result)
    (if (= i 0)
        result
        (if (= (remainder n i) 0)
            (helper (- i 1)
                    (cons i result))
            (helper (- i 1)
                    result))))
  (helper (- n 1) '()))

(define (friendly? a b)
  (and (= a
          (apply + (list-of-divisors b)))
       (= b
          (apply + (list-of-divisors a)))))

(define (allFriends)
  (apply append
         (map (lambda (x)
                (apply append
                       (map (lambda (y)
                              (if (friendly? x y)
                                  (list (cons x y))
                                  '()))
                            (range 1 1000))))
              (range 1 1000))))

(define (range start end)
  (define (helper i result)
    (if (< i start)
        result
        (helper (- i 1)
                (cons i result))))
  (helper end '()))

(define (construct-list m)
  (define (helper current result i)
    (if (null? (car current))
        result
        (if (= (remainder i 2) 0)
            (helper (map cdr current)
                    (append result
                            (map car current))
                    (+ i 1))
            (helper (map cdr current)
                    (append result
                            (reverse (map car current)))
                    (+ i 1)))))
  (if (null? m)
      '()
      (helper m '() 0)))

(define (filter p l)
  (define (helper current-l result)
    (cond
      ((null? current-l) result)
      ((p (car current-l)) (helper (cdr current-l)
                                   (append result (list (car current-l)))))
      (else (helper (cdr current-l) result))))
  (helper l '()))


(define (min-n L n)
  (define (remove-prev-layer l)
    (filter (lambda (x)
              (list? x))
            l))
  (define (remove-n-times i l)
    (if (= i n)
        l
        (remove-n-times (+ i 1)
                        (apply append (remove-prev-layer l)))))
  (apply min
         (filter (lambda (y)
                   (not (list? y)))
                 (remove-n-times 0 L))))
            
                
                   