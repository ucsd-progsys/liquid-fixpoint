(fixpoint  "--rewrite")
(fixpoint  "--save")
 
 
 
(constant sum ((func 0 ((int)) int)))
 
(define sum ((n (int))) int ((ite  (<=  n  0)  0  (+  n  (sum  (-  n  1))))))
 
 
(constraint
   (and
      (forall ((x (int)) ((==  x  5)))
         ((==  (sum  x)  15)))))