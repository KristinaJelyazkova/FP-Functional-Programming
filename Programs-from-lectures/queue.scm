;предикат
(define (empty-queue? queue)
  (null? (car queue)))


; конструктор
(define (make-queue)
  (cons '() '()))

;Намиране на елемента в началото на опашката
(define (front queue)
  (if (empty-queue? queue)
      (error "front is called for an empty queue" queue)
      (caar queue)))

;Включване на елемент в опашка
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
        (begin (set-car! queue new-pair)
               (set-cdr! queue new-pair)
               queue)
        (begin (set-cdr! (cdr queue) new-pair)
               (set-cdr! queue new-pair)
               queue))))

;Изключване на елемент от опашка
(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "deleting an element from an empty queue" queue)
      (begin (set-car! queue (cdr (car queue)))
             queue)))

;извежда елементите на опашка, но я разрушава
(define (print-queue! queue)
    (if (empty-queue? queue)
      (display " ")
      (begin
        (display (front queue))
        (display " ")
        (print-queue! (delete-queue! queue)))))

;извежда елементите на опашка без да я разрушава
(define (print-queue queue)
  (if (empty-queue? queue)
      (princ " ")
      (car queue)))

