;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))

;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

;method 1
;(+ 5 4)
;(inc (+ 4 4))
;(inc (inc (+ 3 4)))
;(inc (inc (inc (+ 2 4))))
;(inc (inc (inc (inc (+ 1 4)))))
;(inc (inc (inc (inc (inc (+ 0 4))))))
;(inc (inc (inc (inc (inc 4)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9

;method 2
;(+ 5 4)
;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9
;

; define a function f such that
; f(n) = n if n<3
;      = f(n-1) + 2*f(n-2) + 3*f(n-3)  ...otherwise
;
; recursive def
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	    (* 2 
	       (f (- n 2)))
	    (* 3
	       (f (- n 3))))))

(define (f-itr count a b c)
  (cond ((= 0 count) 0)
	((= 1 count) 1)
	((= 2 count) c)
	(else (f-itr (- count 1)
		     b
		     c
		     (+ c 
			(* 2 b)
			(* 3 a))))))

; 
; iterative defination
(define (newf n)
  (f-itr n 0 1 2))


;         1
;        1 1
;       1 2 1
;      1 3 3 1
;     1 4 6 4 1
; write recursive def to calculate elemets of pascal traingle
;
; pascal base i
; 	| base <=2 || i==1 || i==base = 1
; 	| otherwise (pascal base-1 i-1 + pascal base-1 i)

(define (pascal base i)
  (if (or (< base 3) (= base i) (= 1 i))
      1
      (+ (pascal (- base 1)
		 (- i 1))
	 (pascal (- base 1) i))))


; recursive defination for finding out number of ways to 
; have change

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(define (change-count amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
	  ((or (< amount 0) (= kinds-of-coins 0)) 0)
	  (else (+ (cc amount
		       (- kinds-of-coins 1))
		   (cc (- amount
			  (first-denomination kinds-of-coins))
		       kinds-of-coins)))))
  (cc amount 5))
	  
;(change-count 11)
; = (cc 11 5)
; = (+ (cc 11 4) (cc -39 5))
; = (+ (cc 11 3) (cc -14 4) 0)
; = (+ (cc 11 2) (cc 1 3) 0 0)
; = (+ (cc 11 1) (cc 6 2) (+ (cc 1 2) (cc -9 3)) 0 0)
; = (+ (cc 11 0) (cc 10 1) (+ (cc 6 1) (cc 1 2)) (+ (+ (cc 1 1) (cc -4 2)) 0 0 0)
; = (+ 1 (cc 10 0) (+ (+ (cc 6 0) (cc 5 1)) (+ (1 1) (-4 2)) (+ (+ (+ (cc 1 0) (cc 0 1)) 0)) 0 0 0) 
; = ( + 1 0 0 (+ (cc 5 0) (cc 4 1)) 


    