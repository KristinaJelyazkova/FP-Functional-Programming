(define (last x)
  (if (null? (cdr x))
      x
      (last (cdr x))))

(define (append2! L1 L2)
  (if (null? L1)
      L2
      (begin (set-cdr! (last L1) L2)
             L1)))

(define (accumulate op initial L)
  (define (helper current-list result)
    (if (null? current-list)
        result
        (helper (cdr current-list)
                (op result (car current-list)))))
  (helper L initial))

(define (my-append . L)
  (accumulate append2! '() L))

(define(delete! L n)
  (define (pointer current-list index)
    (if (= index 0)
        current-list
        (pointer (cdr current-list) (- index 1))))
    (if (= n 0)
      ;(begin (set! L (cdr L))
             ;L)
        (begin (set-car! L (cadr L))
               (set-cdr! L (cddr L))
               L)
      (begin (set-cdr! (pointer L (- n 1)) (cdr (pointer L n)))
         L)))
    
        
             