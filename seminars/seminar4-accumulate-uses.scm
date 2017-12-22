(define (identity x) x)
(define (plus-1 x) (+ x 1))

(define (sum-iter start end)
  (define (helper current result)
    (if (> current end)
        result
        (helper (+ current 1)
                (+ result current))))
  (helper start 0))

; Абстрахираме изчисляването на членовете на сумата, добавяйки "term".
; (sum-iter 1 100) става (sum-iter 1 100 identity)
(define (sum-term start end term)
  (define (helper current result)
    (if (> current end)
        result
        (helper (+ current 1)
                (+ result (term current)))))
  (helper start 0))

; Абстрахираме движението между "start" и "end", чрез процедура "next", която ни дава следващ
; индекс.
; (sum-iter 1 100) става (sum-iter-next 1 plus-1 100 identity)
(define (sum-term-next start next end term)
  (define (helper current result)
    (if (> current end)
        result
        (helper (next current)
                (+ result (term current)))))
  (helper start 0))

; Абстрахираме комбинирането (напр. операцията събиране при сумирането по-горе). Трябва ни е
; първоначална стойност за комбинатора. Така получаваме accumulate.
; (sum-iter 1 100) става (accumulatet 1 plus-1 100 identity + 0)
(define (accumulate start next end term comb initial)
  (define (helper current result)
    (if (> current end)
        result
        (helper (next current)
                (comb result (term current)))))
  (helper start initial))

(define (_square x) (* x x))

(assert= 36 (accumulate 1 plus-1 3 _square * 1))

;--------------------------------------------------------------------------------------------------

(define (fact n)
  (accumulate 1 plus-1 n identity * 1))

(assert= 1 (fact 1))
(assert= 24 (fact 4))

;--------------------------------------------------------------------------------------------------

; Редицата ни е от вида "x * x * x * ...", като всеки от членовете е независим
; от текущия индекс. Затова подаваме за term процедура, която игнорира индекса
; и винаги връща стойността на x.
; (Името на параметъра "_" е конвенция за кръщаване на неизползвани параметри)
(define (pow x n)
  (accumulate 1 plus-1 n (lambda (_) x) * 1))

; Процедурата може да е и именована:
(define (pow2 x n)
  (define (term _) x)
  (accumulate 1 plus-1 n term * 1))

(assert= 27 (pow 3 3))
(assert= 1024 (pow 2 10))
(assert= 27 (pow2 3 3))
(assert= 1024 (pow2 2 10))

;--------------------------------------------------------------------------------------------------

(define (variation k n)
  (/ (accumulate 1 plus-1 n identity * 1)
     (accumulate 1 plus-1 (- n k) identity * 1)))

(assert= 210 (variation 3 7))
(assert= 11880 (variation 4 12))

;--------------------------------------------------------------------------------------------------

(define (combination k n)
  (/ (accumulate 1 plus-1 n identity * 1)
     (* (accumulate 1 plus-1 k identity * 1)
        (accumulate 1 plus-1 (- n k) identity * 1))))

(assert= 35 (combination 3 7))
(assert= 495 (combination 4 12))

;--------------------------------------------------------------------------------------------------

(define (ex x)
  (define (term n)
    (/ (expt x n)
       (fact n)))
  (accumulate 0 plus-1 50 term + 0))

(assert-approx 20.085 0.001 (ex 3))
(assert-approx 2980.957 0.001 (ex 8))

;--------------------------------------------------------------------------------------------------

(define (my-sin x)
  (define (term n)
    (* (expt -1 n)
       (/ (expt x (+ (* 2 n) 1))
          (fact (+ (* 2 n) 1)))))
  (accumulate 0 plus-1 50 term + 0))

(assert-approx 0 0.001 (my-sin 0))
(assert-approx 0.909 0.001 (my-sin 2))
(assert-approx 0 0.001 (my-sin 3.1415))

;--------------------------------------------------------------------------------------------------

; Тук на всеки индекс съпоставяме самия него чрез identity - членовете на
; редицата изобщо не зависят от индексите. Истинската работа се случва в
; комбиниращата процедура, която изчислява всеки нов член чрез този преди него.
; Затова и тази процедура игнорира аргумента си, който съдържа текущия индекс.
(define (my-sqrt x)
  (accumulate 1
              plus-1
              10
              identity
              (lambda (result _)  (/ (+ result (/ x result)) 2))
              1))

(assert-approx 4 0.01 (my-sqrt 16))
(assert-approx 2 0.01 (my-sqrt 4))
(assert-approx 10 0.01 (my-sqrt 100))
(assert-approx (expt 2 0.5) 0.01 (my-sqrt 2))
(assert-approx (expt 171 0.5) 0.01 (my-sqrt 171))
(assert-approx (expt 2237 0.5) 0.01 (my-sqrt 2237))