(define (my-member? x L)
  (cond ((null? L) #f)
        ((= x (car L)) #t)
        (else (my-member? x (cdr L)))))
      
(define (my-length L)
  (if (null? L)
      0
      (+ 1 (my-length (cdr L)))))
    
(define (nth L n)
  (if (= n 1)
      (car L)
      (nth (cdr L) (- n 1))))
    
(define (my-append L1 L2)
  (if (null? L1) 
      L2
      (cons (car L1) (my-append (cdr L1) L2))))
    
(define (range start end)
  (if (> start end)
      '()
      (cons start (range (+ start 1) end))))
    
(define (my-reverse L)
  (if (null? L)
      '()
      (my-append (my-reverse (cdr L)) (list (car L)))))
    
(define (union L1 L2)
  (cond ((null? L1) L2)
        ((null? L2) L1)
        ((my-member? (car L1) L2) (union (cdr L1) L2))
        (else (cons (car L1) (union (cdr L1) L2)))))
      
(define (intersection L1 L2)
  (cond ((null? L1) '())
        ((null? L2) '())
        ((my-member? (car L1) L2) (cons (car L1) (intersection (cdr L1) L2)))
        (else (intersection (cdr L1) L2))))
      
(define (count-occurences L1 L2)
  (define (beginWith l1 l2)
    (cond ((null? l2) 1)
          ((null? l1) 0)
          ((= (car l1) (car l2))
               (beginWith (cdr l1) (cdr l2)))
          (else 0)))
  (if (null? L1)
      0
      (+ (beginWith L1 L2) (count-occurences (cdr L1) L2))))
    
(define (construct-list n)
  (define (range start end)
    (if (< start end)
        '()
        (cons start (range (- start 1) end))))
  (if (= n 0)
      '()
      (cons (range n 1) (construct-list (- n 1)))))
    
(define (deep-member? x L)
  (cond ((null? L) #f)
        ((not (pair? (car L))) (or (= x (car L))
                              (deep-member? x (cdr L))))
        (else (or (deep-member? x (car L))
                  (deep-member? x (cdr L))))))