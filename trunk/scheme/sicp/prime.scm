(define (prime? n)
  (= n (smallest-divisor n)))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n m)
  (cond ((> (* m m) n) n)
	((divides m n) m)
	(else (find-divisor n (+ m 1)))))

(define (divides m n)
  (= 0 (remainder n m)))
	

(define (expmod base exp m)
  (cond ((= 0 exp) 1)
	((even exp) (remainder (square (expmod base 
					       (/ exp 2) 
					       m)) 
			       m))
	(else (remainder (* base (expmod base 
					 (- exp 1) 
					 m)) 
			 m))))

(define (even m)
  (= 0 (remainder m 2)))

(define (square x) (* x x))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1(random (- n 1)))))



(define (quick-prime-test? p n)
  (cond ((= n 1) (fermat-test p))
	(else (and (fermat-test p) 
		   (quick-prime-test? p 
				       (- n 1))))))


(define (timed-prime-test p)
  (define (report-prime elapsed-time)
    (display " **** ")
    (display elapsed-time))
  (define (start-prime-test p start-time)
    (if (prime? p)
	(report-prime (- (runtime) start-time))))
  (newline)
  (display p)
  (start-prime-test p (runtime)))
  

(define (search-for-primes start end)
  (if (< start end)
      ( (timed-prime-test start)
	(search-for-primes (+ start 1) end))
      (timed-prime-test start)
))