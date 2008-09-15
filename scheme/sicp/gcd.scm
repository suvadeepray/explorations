(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))


; gcd .. normal order

; (gcd 206 40) = (gcd 40 (remainder 206 40))
;              = (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;              = (gcd (remainder 40 (remainder 206 40)) (remainder (206 40) (remainder 40 (remainder 206 40))))