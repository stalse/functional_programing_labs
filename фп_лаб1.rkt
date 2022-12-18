(define r 0)
 
; глибина рекурсії
(define-syntax rec
  (syntax-rules () ; визначення правила
    ( (_ r) (begin (set! r (+ r 1)) r)))) ; інкремент числа
 

(define (square b)
  (* b b)) ; добуток числа
 

(define (mod-number? n)
  (= (remainder n 2) 0)) ; визначення умови ділення числа n на 2 із остачею 0 
 

(define (mod-powing b m)
  (display "b^p = ")(display b) (newline)
  (display "b^p mod m = ") (display (remainder b m)) ; відображення остачі від ділення
  (newline) (display "Recursion level: ") (display r)) ; відображення глибини рекурсії
 

(define (exp b n m result)
  (rec r) ; виклик процедури інкременту
  (cond ((= n 1)
        (mod-powing result m)) ;перевірка остачі від ділення шляхом виклику 
        ( (= n 0) (display "Result: ") 1) ; відображення результату
        ( (mod-number? n) (exp b (/ n 2) m (square result)) ) ; предикат із визначенням остачею 
        ( else (exp b (- n 2) m (* b (* b (sqrt (square result))))) ) ) ); альтернативне розкриття вказаної формули через корінь
        
  
;завдання 14.1
 
; запуск
(display "14,1") (newline)
 
(display "Enter b: ")
(define b (read)) (newline) ; отримання значення із консолі
 
(display "Enter p: ")
(define p (read)) (newline)
 
(display "Enter m: ")
(define m (read)) (newline)
 
(exp b p m b) ; Виклик процедури
 
;завдання 14.2
 
(define (sub n)
  (cond ((= n 1)
         (display n))
        ( (even? n) (sub (- n 1))) ; перевірка на парність числа 
        ( else (display n) (newline) (sub (- n 1))) ))
 
 
; запуск
(newline) (newline)
(display "14,2") (newline)
 
(sub 8)