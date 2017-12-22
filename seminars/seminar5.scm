; Как действаме тук:
; 1. Минаваме през всички индекси в интервала от 2 до n-1.
; 2. На всяко число съпостяваме булева стойност, която казва дали НЕ Е делител
;    на n.
; 3. Прилагаме логическо И на стойностите от предната стъпка, като началната
;    стойност на операциите е #t.
; По този начин, ако има делител на n в дадения интервал, то term ще даде #f за
; него и крайният резултат от accumulate ще стане #f заради логическото И.
(define (prime-accumulate? n)
  (define (term candidate)
    (not (= 0 (remainder n candidate))))
  (accumulate 2
              plus-1
              (- n 1)
              term
              (lambda (x y) (and x y))
              #t))

(define (count-digits-iter n)
  (define (helper i result)
    (if (<= i 0)
        result
        (helper (quotient i 10)
                (+ result 1))))
  (helper n 0))

; Това е реализация на добре познатия ни алгоритъм за reverse.
; - term отделя x-тата цифра (от дясно наляво) от х на всяка стъпка.
; - comb добавя нова цифра на края на резултата.
(define (my-reverse-accumulate n)
  (let ((len-n (count-digits-iter n)))
    (define (term x)
      (remainder (quotient n (expt 10 (- x 1))) 10))
    (define (comb result new-digit)
      (+ (* result 10) new-digit))
    (accumulate 1
                plus-1
                len-n
                term
                comb
                0 )))

; Пример за проста употреба на let:
(define (testlet x)
  (let
    ((x-plus-3 (+ x 3))
     (x-plus-5 (+ x 5)))
    (* x-plus-3 x-plus-5)))

(assert= 35 (testlet 2))

; Същият пример, с lambda:
(define (withoutlet x)
  ((lambda (xplus3 xplus5)
           (* xplus3 xplus5))
   (+ x 3) (+ x 5)))

(assert= 35 (withoutlet 2))

; Долното няма да работи с let, тъй като x-plus-3 няма да е достъпно в средата,
; в която се изчислява стойността на x-plus-5.
(define (testlet* x)
  (let*
    ((x-plus-3 (+ x 3))
     (x-plus-5 (+ x-plus-3 2)))
    (* x-plus-3 x-plus-5)))

(assert= 35 (testlet* 2))

; letrec ни позволява дефинициите да са взаимно рекурсивни. Долното няма да
; работи с let*
(define (testletrec x)
  (letrec
    ((my-even? (lambda (x)
                 (if (= x 1)
                     #f
                     (my-odd? (- x 1)))))
     (my-odd? (lambda (x)
                (not (my-even? x)))))
    (my-odd? x)))

; Целта на задачата е да се напише процедура (derive f), която връща f'(x).

; derive взима процедура и връща нова процедура. В новополучената процедура
; фиксираме z да е близо до x. По този начин можем да вземем някакво
; приближение на стойността на производната.
(define (derive f)
  (lambda (x)
    (let ((z (+ x 0.001)))
      (/ (- (f x)
            (f z))
         (- x z)))))

; Това е ГРЕШНО решение на задачата. Тук стойността се изчислява директно, а не
; се връща функция (т.е. lambda). Това е груба грешка, която бива санкционирана
; на контролни и изпити.
(define (derive-INCORRECT f x)
  (let ((z (+ x 0.001)))
   (/ (- (f x)
         (f z))
      (- x z))))

; Тук и двете процедури правят почти същото като в derive - просто взимат за
; параметри и връщат като резултати двуаргументни процедури. За да намерим
; частната производна във всеки от случаите, ние диференцираме f само по един от
; параметрите му (x или y), като запазваме другия (съответно, y или x) без да го
; променяме и директно го подаваме на f.

(define (derive-x f)
  (lambda (x y)
    (let ((z (+ x 0.001)))
      (/ (- (f x y)
            (f z y))
         (- x z)))))

(define (derive-y f)
  (lambda (x y)
    (let ((z (+ y 0.001)))
      (/ (- (f x y)
            (f x z))
         (- y z)))))

(assert-approx 2
               0.01
               ((derive-x (lambda (x y) (+ (* 2 x) (* 3 y))))
                1337 1337))
(assert-approx 3
               0.01
               ((derive-y (lambda (x y) (+ (* 2 x) (* 3 y))))
                1337 1337))

; Композира две функции. Това означава да върне функция p(x), като p(x)=f(g(x)).
(define (compose f g)
  (lambda (x)
    (g (f x))))

; Обърнете внимание, че това е ГРЕШНО решение на задачата, тъй като не връща
; функция (т.е. lambda), а директно изчислява нейната стойност за дадено x.
; Това е груба грешка, която бива санкционирана на контролни и изпити.
(define (compose-INCORRECT f g x)
  (g (f x)))

(define (identity x) x)
(define (pow2 x) (* x x))
(define (plus1 x) (+ x 1))

(assert= 9 ((compose identity pow2) 3))
(assert= 9 ((compose pow2 identity) 3))
(assert= 3 ((compose identity identity) 3))
(assert= 81 ((compose pow2 pow2) 3))
(assert= 5 ((compose pow2 plus1) 2))
(assert= 9 ((compose plus1 pow2) 2))

; Целта на задачата е да се напише (repeat-n f n), която изчислява n-тото
; „влагане“ на функцията f от вида: f(f(f(...f(x)))), където n е „дълбочината“
; на вложените викания.
;
; Например, ако искаме да го приложим за (lambda (x) (+ x 1)), то
; (repeat-n (lambda (x) (+ x 1)) 5) ще върне процедура, която 5 пъти прибавя
; единица към аргумента си.
;
; Всичко това се прави рекурсивно, като на всяка стъпка прилагаме compose.
; Дъното на рекурсията е функцията идентитет, която просто връща x (т.е.
; най-вътрешното изчисление в условието със скобите по-горе).

; Рекурсивен вариант на repeat.
(define (repeat-n-recur f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeat-n-recur f (- n 1)))))

; Итеративен вариант.
(define (repeat-n f n)
  (define (helper result i)
    (if (= i 0)
        result
        (helper (compose f result) (- i 1))))
  (helper (lambda (x) x) n))

; И един вариант с accumulate. За комбинатор използва compose.
(define (repeat-accumulate f n)
  (accumulate 1
              plus1
              n
              (lambda (_) f)
              compose
              identity))

(define (plus1 x) (+ x 1))

(assert= 6 ((repeat-n-recur plus1 5) 1))
(assert= 6 ((repeat-n plus1 5) 1))
(assert= 6 ((repeat-accumulate plus1 5) 1))

; n-та производна на f. Изчислява се чрез прилагане n пъти на derive.
(define (derive-n f n)
  ((repeat-n derive n) f))

(assert-approx 12 0.1 ((derive-n (lambda (x) (* x x x)) 1) 2))
(assert-approx 12 0.1 ((derive-n (lambda (x) (* x x x)) 2) 2))
(assert-approx 6 0.1 ((derive-n (lambda (x) (* x x x)) 3) 2))
(assert-approx 0 0.1 ((derive-n (lambda (x) (* x x x)) 4) 2))
