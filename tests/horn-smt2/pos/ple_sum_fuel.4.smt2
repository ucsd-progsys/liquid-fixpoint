(fixpoint "--rewrite")
(fixpoint "--interpreter=false")
(fixpoint "--fuel=4")
 
 
 
 
(constant sum (func 0 (Int) Int))
 
(define sum ((n Int)) Int (if (<= n 0) 0 (+ n (sum (- n 1)))))
 
 
 
(constraint
  (and
    (forall ((x Int) ((and (<= 0 (sum (- x 5))) (<= 5 x))))
      ((<= 15 (sum x))))))