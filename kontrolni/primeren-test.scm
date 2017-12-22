(define (meetOnce f g a b)
  (cond ((> a b) #f)
        ((= (f a) (g a)) a)
        (else (meetOnce f g (+ a 1) b))))

(define (meetTwice? f g a b)
  (let ((x (meetOnce f g a b)))
    (if (not x)
         #f
         (if (not (meetOnce f g (+ x 1) b))
             #f
             #t))))

(define (member? x li)
  (cond ((null? li) #f)
        ((= x (car li)) #t)
        (else (member? x (cdr li)))))
  

(define (duplicate? x li)
  (cond ((null? li) #f)
        ((= x (car li)) (member? x (cdr li)))
        (else (duplicate? x (cdr li)))))

(define (filter p l)
  (define (helper current-l result)
    (cond
      ((null? current-l) result)
      ((p (car current-l)) (helper (cdr current-l)
                                   (append result (list (car current-l)))))
      (else (helper (cdr current-l) result))))
  (helper l '()))

(define (maxDuplicate LL)
(define L1
 (apply append
        (map (lambda (l)
              (if (null? l)
                   '()
                   (list (apply max l))))
             (map (lambda (li)
                    (filter (lambda (x)
                              (duplicate? x li))
                            li))
                  LL))))
  (if (null? L1)
     #f
     (apply max L1)))

(define (checkMatrix? m k)
  (if (null? m)
      #t
      (if (= (length (filter (lambda (x)
                               (= (remainder x k) 0))
                             (car m)))
             0)
          #f
          (checkMatrix? (cdr m) k))))

(define (longestDescendingPrefix L)
  (define (helper current result last)
        (if (null? current)
            result
            (if (>= (car current) last)
                result
                (helper (cdr current)
                        (append result (list (car current)))
                        (car current)))))
  (if (null? L)
      '()
      (helper (cdr L) (list (car L)) (car L))))

(define (longestDescending L)
  (let* ((current (longestDescendingPrefix L))
         (len (length current)))
        (define (helper li)
          (if (null? li)
              current
              (let* ((next (longestDescendingPrefix li))
                     (nextLen (length next)))
                (if (> nextLen len)
                    (begin (set! current next)
                           (set! len nextLen)
                           (helper (cdr li)))
                    (helper (cdr li))))))
        (helper (cdr L))))

;-----------------------------------------------------------------------
(define (mixedHelp f g a b)
  (cond ((> a b) #f)
        ((< (f a) (g a)) #t)
        (else (mixedHelp f g (+ a 1) b))))

(define (mixed? f g a b)
  (and (mixedHelp f g a b) (mixedHelp g f a b)))

(define (maxUnique LL)
(define L1
 (apply append
        (map (lambda (l)
              (if (null? l)
                   '()
                   (list (apply max l))))
             (map (lambda (li)
                    (filter (lambda (x)
                              (not (duplicate? x li)))
                            li))
                  LL))))
  (if (null? L1)
     #f
     (apply max L1)))

(define (checkMatrix2? m k)
  (if (null? m)
      #t
      (if (= (length (filter (lambda (x)
                               (= (remainder k x) 0))
                             (car m)))
             (length (car m)))
          #f
          (checkMatrix2? (cdr m) k))))
