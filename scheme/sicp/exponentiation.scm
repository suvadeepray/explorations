(define (expt b n)
  (if (= 0 n)
      1
      (* b (expt b
		 (- n 1)))))

(define (expt-new  b n)
    (define (expt-itr product counter)
      (if (= 0 counter)
	  product
	  (expt-itr (* product b)
		    (- counter 1))))
    (expt-itr 1 n))

(define (fast-exp b n)
  (define (even? x)
    (= (remainder b 2) 0))
  (cond ((= n 0) 1)
	((even? b) (square (fast-exp b
				  (/ n 2))))
	(else (* b (fast-exp b (- n 1))))))


(expt-new 5 6)
(expt 5 6)
(fast-exp 5 6)
