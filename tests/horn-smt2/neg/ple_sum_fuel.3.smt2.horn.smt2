 
 
(constant sum ((func 0 ((int)) int)))
 
(define sum ((n (int))) int ((ite  (<=  n  0)  0  (+  n  (sum  (-  n  1))))))
 
 
(constraint
   (and
      (forall ((x (int)) ((and  (<=  0  (sum  (-  x  7)))  (<=  7  x))))
         ((<=  28  (sum  x))))))