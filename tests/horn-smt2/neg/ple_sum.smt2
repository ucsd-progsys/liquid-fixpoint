(fixpoint  "--rewrite")
 
 
 
(constant sum ((func 0 (int) int)))
 
(define sum ((n int)) int (if  (<=  n  0)  0  (+  n  (sum  (-  n  1)))))
 
 
 
(constraint
   (and
      (forall ((x int) ((==  x  5)))
         ((==  (sum  x)  150)))))