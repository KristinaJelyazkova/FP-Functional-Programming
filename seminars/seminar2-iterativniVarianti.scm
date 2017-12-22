; Проверка дали число е просто.
; Идеята на helper-а е да обходи възможните делители между n-1 и 1. Всъщност,
; можем да вървим от корен квадратен на n и да си спестим малко проверки, но
; ще се наложи да правим закръгляния. Също така, 1 излиза просто. Можете да
; поправите този проблем като упражнение вкъщи! :D
(define (prime? n)
  (define (helper possible-divisor)
    (cond
      ((<= possible-divisor 1) #t)
      ((= (remainder n possible-divisor) 0) #f)
      (else (helper (- possible-divisor 1)))))
  (helper (- n 1)))

; Брои цифрите на число.
; На всяка стъпка числото бива разделяно целочислено на 10 (т.е. махаме по една
; цифра от края му).
(define (count-digits n)
  (if (<= n 0)
      0
      (+ 1 (count-digits (quotient n 10)))))

; Сумиране на цифрите на число.
; Обърнете внимание, че това е вариация на броенето на цифри. Разликата е, че на
; всяка стъпка освен махане на последната цифра (чрез quotient), ние добавяме
; тази цифра в крайния резултат (частта с remainder).
(define (sum-digits n)
  (if (<= n 0)
      0
      (+ (remainder n 10) (sum-digits (quotient n 10)))))

; „Чисто“ рекурсивен вариант на bin-to-dec, без helper.
(define (bin-to-dec n)
  (if (= n 0)
      0
      (+ (remainder n 10)
         (* 2 (bin-to-dec (quotient n 10))))))

; Вариант с helper.
(define (bin-to-dec2 n)
  (define (helper current i)
    (if (= 0 current)
        0
        (+ (* (remainder current 10) (expt 2 i))
           (helper (quotient current 10) (+ i 1)))))
  (helper n 0))

; „Чисто“ рекурсивен вариант на dec-to-bin, без helper.
(define (dec-to-bin n)
  (if (= n 0)
      0
      (+ (remainder n 2)
         (* 10 (dec-to-bin (quotient n 2))))))

; Вариант с helper.
(define (dec-to-bin2 n)
  (define (helper current i)
    (if (= 0 current)
        0
        (+ (* (remainder current 2) (expt 10 i))
           (helper (quotient current 2) (+ i 1)))))
  (helper n 0))

; Проверява дали число е автоморфно - квадратът му завършва със самото него.
; Например, 25 е автоморфно, защото 25^2=625.
; Можете да го решите и рекурсивно, като направите процедура, която проверява
; дали число завършва на друго число.
(define (automorphic? n)
  (= n
     (remainder (expt n 2)
                (expt 10 (count-digits n)))))

; Рекурсивна сума на числа
; Ако пробвате с малък start и огромен (>10^8) end ще видите колко много памет
; се използва. Това е заради необходимостта някъде в стека да се запази
; информация до къде точно сме стигнали с извикванията. Можете да си го
; представите по следния начин: интерпретаторът трябва да стигне до дъното на
; рекурсията (т.е. да разкрие всички скоби при сумирането) за да започне изобщо
; да прави някакви сметки. През това време трябва да запише някъде в паметта
; всички междинни резултати.
(define (sum-rec start end)
  (if (>= start end)
    end
    (+ start (sum-rec (+ start 1) end))))

; Итеративен вариант на сумирането. Вътрешно се изпълнява подобно на цикъл от
; императивен език. Няма проблем с голяма (>10^8) разлика между start и end.
; Забележете, че сметката се прави при рекурсивното извикване. Макар и на пръв
; поглед разликата да изглежда малка, това е доста по-различно от по-горния
; вариант - интерпретаторът върши сметките в движение, докато „върви“ към дъното
; на рекурсията - не му се налага да помни контекст.
(define (sum-iter start end)
  (define (helper current result)
    (if (> current end)
      result
      (helper (+ current 1)
              (+ result current))))
  (helper start 0))

; Итеративен вариант на броенето на цифри. Тук резултатът е просто един брояч.
(define (count-digits-iter n)
  (define (helper current result)
    (if (<= current 0)
        result
        (helper (quotient current 10)
                (+ result 1))))
  (helper n 0))

; Итеративен вариант на сумирането на цифри. Единствената разлика с предишната
; задача (броене на цифри) е, че вместо единица се добавя последната цифра в
; резултата.
(define (sum-digits-iter n)
  (define (helper current result)
    (if (<= current 0)
        result
        (helper (quotient current 10)
                (+ result (remainder current 10)))))
  (helper n 0))