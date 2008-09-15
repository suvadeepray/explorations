(define (fib n)
  (cond ((= 0 n) 0)
        ((= 1 n) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fibb n)
  (define (fib-itr a b count)
    (if (= count 0) 
        b
        (fib-itr (+ a b) a (- count 1))))
  (fib-itr 1 0 n))


(define (efficient-fib n)
  (define (even? n)
    (= (remainder n 2) 0))
 
  (define (fib-itr a b p q count)
    (cond ((= count 0) b)
	  ((even? count) (fib-itr a
				b
				(+ (* p p) (* q q))
				(+ (* q q) (* 2 p q))
				(/ count 2)))
	  (else (fib-itr (+ (* b q) (* a q) (* a p))
		       (+ (* b p) (* a q))
		       p
		       q
		       (- count 1)))))
  (fib-itr 1 0 0 1 n))