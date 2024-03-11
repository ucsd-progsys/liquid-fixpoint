(fixpoint "--rewrite")
(fixpoint "--interpreter=false")
(fixpoint "--fuel=3")
 
 
 
 
(constant sum (func 0 (Int) Int))
 
(define sum ((n Int)) Int (if (<= n 0) 0 (+ n (sum (- n 1)))))
 
 
 
(constraint
  (and
    (forall ((x Int) ((and (<= 0 (sum (- x 7))) (<= 7 x))))
      ((<= 28 (sum x))))))