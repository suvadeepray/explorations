(define (sqrt x) 
  (define (new-if predicate then-clause else-clause)
	(cond (predicate) then-clause)
	(else else-clause))
  (define (try guess x) (new-if (good-enough guess x) 
                            guess
                            (try (improve guess x) x)))
  (define (good-enough guess x)
    (define (square x) (* x x))
      (< (abs (- (square guess) x)) 0.0000001))
        
  (define (improve guess x) (average guess (/ x guess)))
  (define (average a b) (/ (+ a b) 2))
  (try 1 x))

(sqrt 4)
                                    
