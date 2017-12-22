; Обръща цифрите на число
; Идеята е на всяка стъпка да взимаме последната цифра на числото и да я слагаме
; най-отзад на резултата. На всяка стъпка махаме по една цифра от числото и
; добавяме нула в края на резултата (т.е. местим цифрите на числото надясно, а
; тези на резултата - наляво).
(define (my-reverse n)
  (define (helper current result)
    (if (<= current 0)
        result
        (helper (quotient current 10)
                (+ (* result 10)
                   (remainder current 10)))))
  (helper n 0))

; Проверката дали дадено число е палиндром използва за база идеята на reverse -
; с итеративен процес постепенно обръщаме числото. Разликата е, че тук на всяка
; стъпка проверяваме дали междинният резултат (обърнатото число до тук) е равен
; на оставащата част от числото, което обръщаме. Ако се затруднявате с това
; решение, опитайте се първо да разгледате reverse.
(define (palindrome? n)
  (define (helper current result)
    (cond
      ((< current result) #f)
      ((or (= current result) (= (quotient current 10) result)) #t)
      (else (helper (quotient current 10)
                    (+ (* result 10) (remainder current 10))))))
  (if (= 0 (remainder n 10)) ; aко числото завършва на 0, то не е палиндром, а ако го няма това, може да излезе, че е
      #f
      (helper n 0)))

; Малко по-лесен вариант :)
(define (palindrome2? n)
  (= n (my-reverse n)))

; Проверяваме дали числото a се съдържа в числото n като последователност от
; цифри. За целта отделяме „маска“ от вида 10^m, където m е дължината на a.
; След това на всяка стъпка гледаме остатъка на n при деление на тази маска, за
; да вадим последните му m цифри и да ги сравним с a. Ако a се съдържа в n, на
; някоя от стъпките този остатък ще е равен на a.
(define (substr? n a)
  ; Помощна функция, която проверява дали числото завършва на a.
  (define (ends-with-a? i)
    (= (remainder i (expt 10 (count-digits-iter a)))
       a))
  ; Тук на всяка стъпка махаме цифра и гледаме дали резултатът завършва на a.
  (define (helper i)
    (cond
       ((< i a) #f)
       ((ends-with-a? i) #t)
       (else (helper (quotient i 10)))))
  (helper n))

; Наивен вариант с дървовидна рекурсия. Много (експоненциално) бавен.
(define (fib n)
  (if (<= n 2)
    1
    (+ (fib (- n 1))
       (fib (- n 2)))))

; Итеративен вариант. Следете внимателно как се сменят current и previous. Това
; е основният подход за обръщане на дървовидна рекурсия в итеративен процес :)
(define (fib-iter n)
  (define (helper current previous count)
    (if (= count 1)
        current
        (helper (+ current previous) current (- count 1))))
  (helper 1 0 n))

; Изчислява приближение на корен квадратен итеративно.
;
; epsilon - определя колко близо трябва да е квадратът на текущия резултат до
; числото на което търсим корен квадратен
; good-enough? - проверка, която гледа дали е изпълнено условието за epsilon
; improve - по даден член на редицата намира следващия
; helper - вика improve докато good-enough? не се изпълни
;
; Забележете, че викаме helper с числото 1. Там може да сложите всяко
; положително число. Итеративният процес винаги ще е сходящ и членовете ще
; клонят към корен квадратен от x.
(define (sqrt-iter x)
  (define epsilon 0.01)
  (define (good-enough? tn)
    (< (abs (- (* tn tn) x))
       epsilon))
  (define (next tn)
    (/ (+ tn (/ x tn))
       2))
  (define (helper tn)
    (if (good-enough? tn)
        tn
        (helper (next tn))))
  (helper 1))

; Смята e^x с ред на Тейлър.
; Обърнете внимание на разбиването на помощни функции.
(define (ex x)
  (define iterations 50)
  (define (term n)
    (/ (expt x n)
       (fact n)))
  (define (helper n result)
    (if (> n iterations)
        result
        (helper (+ n 1)
                (+ result (term n)))))
  (helper 0 0))

; Смята cos с ред на Тейлър.
; Обърнете внимание, че разликата с ex е само във функцията term.
(define (cos-iter x)
  (define iterations 50)
  (define (term n)
    (/ (* (expt -1 n) (expt x (* 2 n)))
       (fact (* 2 n))))
  (define (helper n result)
    (if (> n iterations)
        result
        (helper (+ n 1)
                (+ result (term n)))))
  (helper 0 0))
