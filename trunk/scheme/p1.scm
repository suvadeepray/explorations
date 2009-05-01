;Count Intersections

;There's a line from (x1,y1) to (x2,y2) in a grid of squares of unit length. Write a program to compute the number of squares which are intersected by the line internally, i.e squares which are only touched by the line should not be counted.
;Notes

;    * x1,y1,x2,y2 are all integers.
;    * 0 <= x1,y1,x2,y2 <= 10000
;    * Write the program so that it accepts 4 command line parameters - x1,y1,x2,y2
;    * The output of the program should be a single line consisting of only the integer output value.

(define (intersections x1 y1 x2 y2)
  (define (min x y)
    (cond ((< x y) x)
	  (else y)))
  (define (max x y)
    (cond ((> x y) x)
	  (else y)))
  (define dx (abs (- x2 x1)))
  (define dy (abs (- y2 y1)))
  (define biggerside (max dx dy))
  (define smallerside (min dx dy))
  (define (count x y n)
    (define slope (/ smallerside biggerside))
    (cond ((= smallerside 0)
	   0)
	  ((and (= x biggerside)
	       (= y smallerside))
	   n)
	  ((= (* slope x) y)
	   (count (+ x 1)
		  (+ y 1)
		  (+ n 1)))
	  ((< (* slope x) y)
	   (count (+ x 1)
		  y 
		  (+ n 1)))
	  ((> (* slope x) y)
	   (count x
		  (+ y 1)
		  (+ n 1)))))
  (count 0 0 0))
(intersections 0 0 5 5)