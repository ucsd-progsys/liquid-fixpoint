(fixpoint "--rewrite")
 
 
 
 
(constant sum (func 0 (Int) Int))
 
(define sum ((n Int)) Int (if (<= n 0) 0 (+ n (sum (- n 1)))))
 
 
 
(constraint
  (and
    (forall ((x Int) ((= x 5)))
      ((= (sum x) 150)))))